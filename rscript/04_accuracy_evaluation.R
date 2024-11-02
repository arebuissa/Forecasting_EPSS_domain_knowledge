# cross validation

library(tidyverse)
library(tsibble)
library(Metrics)
library(SpecsVerification)
library(greybox)


# data preperation --------------------------------------------------------

# read data

med_qty <- read.csv('data/predicotrs_final_tssible_July_04_2024.csv') |> 
  janitor::clean_names()

product_master <- read_rds('data/product_master.rds')

snaive <- read_rds('results/snaive_prob.rds') |> 
  rename(origin = horizon) |> # i have mistakenly named origin as horizon
  mutate(unique_id = paste0(item_id,'-',origin))


# make the tsibble

med_qty_tsb <- med_qty |> 
  mutate(month = yearmonth(month)) |> 
  group_by(item) |> 
  mutate(item_id = sprintf("id_%02d", cur_group_id())) |>  #creating a item id to handle the data easy. 
  ungroup() |>
  as_tsibble(index = month, key = c(item_id, item))


# create tscv train data

f_horizon <- 6

test_length <- 12

med_tsb_train_tscv <- med_qty_tsb |> 
  filter_index(. ~ as.character(max(med_qty_tsb$month)-(f_horizon))) |>
  stretch_tsibble(.init = (length(unique(med_qty_tsb$month)) - test_length), .step = 1) |> 
  mutate(unique_id = paste0(item_id, '-', .id))  |>  
  rename(origin = .id)


# create tscv test data

med_tsb_test_tscv <- med_qty_tsb |> 
  filter_index(as.character(max(med_qty_tsb$month)-(test_length) + 1) ~ .) |>
  slide_tsibble(.size = 6, .step = 1) |> 
  mutate(unique_id = paste0(item_id, '-', .id)) |> 
  rename(origin = .id)


# Create prediction masters -----------------------------------------------


pred_master <- read_rds('results/timegpt_fc.rds') |> 
  bind_rows(read_rds('results/lstm_uni.rds') |> 
              mutate(model = 'lstm')) |> 
  bind_rows(read_rds('results/lstm_multi.rds') |> 
              mutate(model = 'lstm_reg')) |> 
  rename(origin = horizon) # i have mistakenly named origin as horizon

  
point_pred <- pred_master |> 
  select(month, item_id, origin, model, prediction) |> 
  mutate(unique_id = paste0(item_id,'-',origin))


prob_pred <- pred_master |> 
  select(!c(prediction, quantity)) |> 
  mutate(unique_id = paste0(item_id,'-',origin))


# create id list and model list

# id <- unique(point_pred$unique_id)

id <- med_tsb_test_tscv |> 
  as_tibble() |> 
  filter(stock_out != 1) |> 
  select(unique_id) |> 
  unique() |> 
  pull() # remove stockout cases

model <- unique(point_pred$model)


# Point forecast evaluation -----------------------------------------------

# MASE calculation

computeMASE <- function(forecast, train, test, period) {
  scale <- mean(abs(diff(train, lag = period)))
  mase <- MASE(test, forecast, scale = scale)
  return(mase)
}


# RMSSE calculation

computeRMSSE <- function(forecast, train, test, period) {
  scale <- mean(diff(train, lag = period)^2)
  rmsse <- RMSSE(test, forecast, scale = scale)
  return(rmsse)
}


fc_accuracy <- tibble(
  id = c('test'),
  model = c('test'),
  mase = c(0),
  rmsse = c(0)
)


y <- 1

for (i in id) {
  for (m in model) {
    forecast <- point_pred |> filter(unique_id == i, model == m) |> pull(prediction)
    train <- med_tsb_train_tscv |> filter(unique_id == i) |> pull(quantity)
    test <- med_tsb_test_tscv |> filter(unique_id == i) |> pull(quantity)
    
    mase <- computeMASE(forecast, train, test, 12)
    rmsse <- computeRMSSE(forecast, train, test, 12)
    
    fc <- tibble(
      id = i,
      model = m,
      mase = mase,
      rmsse = rmsse
    )
    
    fc_accuracy <- bind_rows(fc_accuracy, fc)
    
    print(y)
    y <- y + 1
  }
}


# remove the first row

fc_accuracy <- fc_accuracy[-1,]

fc_accuracy <- fc_accuracy |> 
  separate(id, into = c("item_id", "origin"), sep = "-") |> 
  left_join(product_master, by = 'item_id')

# calculate the mase summary

fc_accuracy_summary <- fc_accuracy |> 
  group_by(model, origin) |> 
  summarise(mean_mase = mean(mase), median_mase = median(mase),
            mean_rmsse = mean(rmsse), median_rmsse = median(rmsse))

write_rds(fc_accuracy, 'results/point_accuracy_origin.rds')


# Prob prediction evaluation ----------------------------------------------

# Function to calculate Winkler score

calculate_winkler_score <- function(lower, upper, actual, alpha) {
  score <- ifelse(actual < lower, 
                  (upper - lower) + 2 * alpha * (lower - actual), 
                  ifelse(actual <= upper, 
                         upper - lower, 
                         (upper - lower) + 2 * alpha * (actual - upper)))
  return(score)
}


# Function to calculate skill score - to scale the accuracy measures using naive as the benchmark

calculate_skill_score <- function(score, benchmark_score) {
  skill_score <- (benchmark_score - score) / benchmark_score
  return(skill_score)
}


bs_crps <- tibble(
  id = c('test'),
  model = c('test'),
  crps_skill_score = c(0),
  winkler_skill_score = c(0)
)

y <- 1

for (m in model) {
  for (i in id) {
    
    forecast <- prob_pred |> filter(unique_id == i, model == m) |>
      select(-month, -item, -item_id, -model, -unique_id, -origin)
    
    actual <- med_tsb_test_tscv |> filter(unique_id == i) |> pull(quantity)
    
    mean <- forecast |> rowwise() |> mutate(mean = mean(c_across(starts_with("X")), na.rm = TRUE)) |> pull(mean)
    
    sd <- forecast |> rowwise() |> mutate(sd = sd(c_across(starts_with("X")), na.rm = TRUE)) |> pull(sd)
    
    # Calculate CRPS for current model
    crps <- mean(GaussCrps(mean, sd, actual))
    
    # Calculate CRPS for benchmark (naïve forecast)
    naive_forecast <- snaive |> 
      filter(unique_id == i) |>
      select(-month, -item, -item_id, -unique_id, -origin)
    
    naive_mean <- naive_forecast |> rowwise() |> mutate(mean = mean(c_across(starts_with("X")), na.rm = TRUE)) |> pull(mean)
    
    naive_sd <- naive_forecast |> rowwise() |> mutate(sd = sd(c_across(starts_with("X")), na.rm = TRUE)) |> pull(sd)
    
    crps_naive <- mean(GaussCrps(naive_mean, naive_sd, actual))
    
    # Calculate CRPS skill score
    crps_skill_score <- calculate_skill_score(crps, crps_naive)
    
    alpha <- 0.05
    lower <- forecast |> rowwise() |> mutate(lower = quantile(c_across(starts_with("X")), probs = alpha / 2, na.rm = TRUE)) |> pull(lower)
    upper <- forecast |> rowwise() |> mutate(upper = quantile(c_across(starts_with("X")), probs = 1 - alpha / 2, na.rm = TRUE)) |> pull(upper)
    
    # Calculate Winkler score for current model
    winkler_score <- mean(mapply(calculate_winkler_score, lower, upper, actual, MoreArgs = list(alpha = alpha)))
    
    # Calculate Winkler score for benchmark (naïve forecast)
    naive_lower <- naive_forecast |> rowwise() |> mutate(lower = quantile(c_across(starts_with("X")), probs = alpha / 2, na.rm = TRUE)) |> pull(lower)
    naive_upper <- naive_forecast |> rowwise() |> mutate(upper = quantile(c_across(starts_with("X")), probs = 1 - alpha / 2, na.rm = TRUE)) |> pull(upper)
    winkler_score_naive <- mean(mapply(calculate_winkler_score, naive_lower, naive_upper, actual, MoreArgs = list(alpha = alpha)))
    
    # Calculate Winkler skill score
    winkler_skill_score <- calculate_skill_score(winkler_score, winkler_score_naive)
    
    fc <- tibble(
      id = i,
      model = m,
      crps_skill_score = crps_skill_score,
      winkler_skill_score = winkler_skill_score
    )
    
    bs_crps <- bind_rows(bs_crps, fc)
    
    print(y)
    y <- y + 1
  }
}


# remove the first row

bs_crps = bs_crps[-1,]

bs_crps <- bs_crps |> 
  separate(id, into = c("item_id", "origin"), sep = "-") |> 
  left_join(product_master, by = 'item_id')

# calculate the summary

bs_crps_summary <- bs_crps |> 
  group_by(model, origin) |> 
  summarise(mean_crps = mean(crps_skill_score),
            mean_winkler_score = mean(winkler_skill_score))

write_rds(bs_crps, 'results/prob_accuracy_origin.rds')

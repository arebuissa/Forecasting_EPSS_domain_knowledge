# generate distributions for TimeGPT models

# load libraries

library(tidyverse)
library(fable)
library(tsibble)


# data preparation --------------------------------------------------------

timegpt_malaria <- read.csv('results/timegpt_malaria.csv')
timegpt_no_malaria <- read.csv('results/timegpt_no_malaria.csv')
timegpt_malaria_reg <- read.csv('results/timegpt_malaria_reg.csv')
timegpt_no_malaria_reg <- read.csv('results/timegpt_no_malaria_reg.csv')

product_master <- read_rds('data/product_master.rds')


# Interpolate points ------------------------------------------------------

# Function to interpolate 1000 points from quantiles (timegpt only generates quantiles)

interpolate_points <- function(row_quantiles, quantiles, n_points = 1022) {
  approx(x = quantiles, y = row_quantiles, xout = seq(0, 1, length.out = n_points))$y
}

# Define quantile levels

quantiles <- quantiles <- seq(0.01, 0.99, by = 0.01)


# malaria_reg -------------------------------------------------------------

# Apply the interpolation function row-wise

distribution_points <- t(apply(timegpt_malaria_reg %>% select(-unique_id, -ds, -TimeGPT), 1, interpolate_points, quantiles = quantiles))


# Create column names for the distribution points

colnames(distribution_points) <- paste0('X', seq_len(ncol(distribution_points)))


# Bind the distribution points to the original data

timegpt_malaria_reg_updated <- timegpt_malaria_reg %>%
  bind_cols(as.data.frame(distribution_points)) %>% 
  select(where(function(x) !all(is.na(x)))) %>% 
  select(-contains("Time")) %>% 
  mutate(across(starts_with("X"), ~ if_else(. < 0, 0, .))) %>%
  mutate(prediction = rowMeans(across(starts_with("X")))) %>% 
  rename_with(~ paste0("X", as.integer(str_extract(., "\\d+")) - 12), starts_with("X")) %>% 
  separate(unique_id, into = c("id", "horizon"), sep = "-") %>%
  rename(month = ds, item_id = id) %>%  
  mutate(horizon = as.integer(horizon),
         month = yearmonth(month),
         model = 'timegpt_reg') %>% 
  as_tibble()


# malaria -----------------------------------------------------------------

# Apply the interpolation function row-wise

distribution_points <- t(apply(timegpt_malaria %>% select(-unique_id, -ds, -TimeGPT), 1, interpolate_points, quantiles = quantiles))


# Create column names for the distribution points

colnames(distribution_points) <- paste0('X', seq_len(ncol(distribution_points)))


# Bind the distribution points to the original data

timegpt_malaria_updated <- timegpt_malaria %>%
  bind_cols(as.data.frame(distribution_points)) %>% 
  select(where(function(x) !all(is.na(x)))) %>% 
  select(-contains("Time")) %>% 
  mutate(across(starts_with("X"), ~ if_else(. < 0, 0, .))) %>%
  mutate(prediction = rowMeans(across(starts_with("X")))) %>% 
  rename_with(~ paste0("X", as.integer(str_extract(., "\\d+")) - 12), starts_with("X")) %>% 
  separate(unique_id, into = c("id", "horizon"), sep = "-") %>%
  rename(month = ds, item_id = id) %>%  
  mutate(horizon = as.integer(horizon),
         month = yearmonth(month),
         model = 'timegpt') %>% 
  as_tibble()


# no_malaria_reg ----------------------------------------------------------

# Apply the interpolation function row-wise

distribution_points <- t(apply(timegpt_no_malaria_reg %>% select(-unique_id, -ds, -TimeGPT), 1, interpolate_points, quantiles = quantiles))


# Create column names for the distribution points

colnames(distribution_points) <- paste0('X', seq_len(ncol(distribution_points)))


# Bind the distribution points to the original data

timegpt_no_malaria_reg_updated <- timegpt_no_malaria_reg %>%
  bind_cols(as.data.frame(distribution_points)) %>% 
  select(where(function(x) !all(is.na(x)))) %>% 
  select(-contains("Time")) %>% 
  mutate(across(starts_with("X"), ~ if_else(. < 0, 0, .))) %>%
  mutate(prediction = rowMeans(across(starts_with("X")))) %>% 
  rename_with(~ paste0("X", as.integer(str_extract(., "\\d+")) - 12), starts_with("X")) %>% 
  separate(unique_id, into = c("id", "horizon"), sep = "-") %>%
  rename(month = ds, item_id = id) %>%  
  mutate(horizon = as.integer(horizon),
         month = yearmonth(month),
         model = 'timegpt_reg') %>% 
  as_tibble()


# no_malaria --------------------------------------------------------------

# Apply the interpolation function row-wise

distribution_points <- t(apply(timegpt_no_malaria %>% select(-unique_id, -ds, -TimeGPT), 1, interpolate_points, quantiles = quantiles))


# Create column names for the distribution points

colnames(distribution_points) <- paste0('X', seq_len(ncol(distribution_points)))


# Bind the distribution points to the original data

timegpt_no_malaria_updated <- timegpt_no_malaria %>%
  bind_cols(as.data.frame(distribution_points)) %>% 
  select(where(function(x) !all(is.na(x)))) %>% 
  select(-contains("Time")) %>% 
  mutate(across(starts_with("X"), ~ if_else(. < 0, 0, .))) %>%
  mutate(prediction = rowMeans(across(starts_with("X")))) %>% 
  rename_with(~ paste0("X", as.integer(str_extract(., "\\d+")) - 12), starts_with("X")) %>% 
  separate(unique_id, into = c("id", "horizon"), sep = "-") %>%
  rename(month = ds, item_id = id) %>% 
  mutate(horizon = as.integer(horizon),
         month = yearmonth(month),
         model = 'timegpt') %>% 
  as_tibble()


# prepare master prediction df --------------------------------------------

timegpt_fc <- timegpt_malaria_updated |> 
  bind_rows(timegpt_no_malaria_updated) |> 
  left_join(product_master, by = 'item_id') |> 
  bind_rows(timegpt_malaria_reg_updated |> 
              bind_rows(timegpt_no_malaria_reg_updated) |> 
              left_join(product_master, by = 'item_id')) |> 
  write_rds('results/timegpt_fc.rds')


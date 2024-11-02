# sNaive

library(tidyverse)
library(tsibble)
library(fable)
library(doParallel)
library(foreach)


# data preperation --------------------------------------------------------

# read data

med_qty <- read.csv('data/predicotrs_final_tssible_July_04_2024.csv') |> 
  janitor::clean_names()

product_master <- read_rds('data/product_master.rds')


# make the tsibble

med_qty_tsb <- med_qty |> 
  mutate(month = yearmonth(month)) |> 
  group_by(item) |> 
  mutate(item_id = sprintf("id_%02d", cur_group_id())) |>  #creating a item id to handle the data easy
  ungroup() |>
  as_tsibble(index = month, key = c(item_id, item))


# create tscv train data

f_horizon <- 6

test_length <- 12

med_tsb_train_tscv <- med_qty_tsb |> 
  filter_index(. ~ as.character(max(med_qty_tsb$month)-(f_horizon))) |>
  stretch_tsibble(.init = (length(unique(med_qty_tsb$month)) - test_length), .step = 1) |> 
  mutate(unique_id = paste0(item_id, '-', .id))  |>  
  rename(horizon = .id)


# sNaive ------------------------------------------------------------------

# id list

id_list <- med_tsb_train_tscv |> 
  as_tibble() |> 
  select(unique_id) |> 
  pull() |> 
  unique()


# Register outer parallel backend with 5 cores

cl_outer <- makeCluster(5)
registerDoParallel(cl_outer)


# parallel loop

system.time(snaive_prob_pred <- foreach(i = id_list,
                                        .combine = 'rbind',
                                        .packages=c("doParallel", "foreach", "tidyverse", "fpp3")) %dopar% {
                                          
                                          # i <- id_list[1]
                                          
                                          # train sets
                                          
                                          train_data <- med_tsb_train_tscv |> 
                                            filter(unique_id == i)
                                          
                                          
                                          # model fitting
                                          
                                          snaive_fit <- train_data |> 
                                            model(snaive = SNAIVE(quantity))
                                          
                                          
                                          # forecasting
                                          
                                          fc_horizon <- 6
                                          
                                          boot <- 1000
                                          
                                          snaive_fc <- snaive_fit |>
                                            fabletools::forecast(h = fc_horizon, bootstrap = TRUE, times = boot)
                                          
                                          snaive_bs <- data.frame(matrix(nrow = nrow(snaive_fc), ncol = boot))
                                          
                                          for (n in 1:nrow(snaive_fc)) {
                                            
                                            snaive_bs[n, ] <-  snaive_fc |> pull(quantity) |> nth(n) |> purrr::pluck("x")
                                            
                                          }
                                          
                                          # preparing snaive bootstrap 
                                          
                                          snaive_pred_bs <- snaive_fc |>
                                            as_tibble() |> 
                                            select(month, item, item_id, horizon) |> 
                                            bind_cols(snaive_bs)
                                          
                                          snaive_pred_bs
                                          
                                        })

# Stop outer cluster

stopCluster(cl_outer)

write_rds(snaive_prob_pred |> 
            mutate(unique_id = paste0(paste0(item_id, '-', item_id))), 'results/snaive_prob.rds')

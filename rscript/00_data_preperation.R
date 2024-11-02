# cross validation data preperation for LLMs and DL models

library(tidyverse)
library(fable)
library(tsibble)


# read data

med_qty <- read.csv('data/predicotrs_final_tssible_July_04_2024.csv') |> 
  janitor::clean_names()

# make the tsibble

med_qty_tsb <- med_qty |> 
  mutate(month = yearmonth(month)) |> 
  group_by(item) |> 
  mutate(item_id = sprintf("id_%02d", cur_group_id())) |>  #creating a item id to handle the data easy
  ungroup() |>
  select(-stock_out, -covid_19, -conflict) |> 
  as_tsibble(index = month, key = c(item_id, item))


# create product master

product_master <- med_qty_tsb |> 
  as_tibble() |> 
  select(item, item_id) |>
  unique() |> 
  write_rds('data/product_master.rds')


# check for temporal gaps

med_qty_tsb |> count_gaps()  # seems there are no temporal gaps


# create tscv train data

f_horizon <- 6

test_length <- 12

med_tsb_train_tscv <- med_qty_tsb |> 
  filter_index(. ~ as.character(max(med_qty_tsb$month)-(f_horizon))) |>
  stretch_tsibble(.init = (length(unique(med_qty_tsb$month)) - test_length), .step = 1) |> 
  mutate(unique_id = paste0(item_id, '-', .id))  |>  
  rename(horizon = .id)


# create tscv test data

med_tsb_test_tscv <- med_qty_tsb |> 
  filter_index(as.character(max(med_qty_tsb$month)-(test_length) + 1) ~ .) |>
  slide_tsibble(.size = 6, .step = 1) %>% 
  mutate(unique_id = paste0(item_id, '-', .id)) %>% 
  rename(horizon = .id)


# filter malaria products and save data

med_tsb_train_tscv_no_malaria <- med_tsb_train_tscv |> 
  filter(item != 'Artemether + Lumefanthrine' , item != 'Rapid Diagnostic Test') |> 
  as_tibble() |> 
  select(-item_id, -item) |> 
  write.csv('data/train_tscv_no_malaria.csv', row.names = F)

med_tsb_train_tscv_malaria <- med_tsb_train_tscv |> 
  filter(item == 'Artemether + Lumefanthrine' | item == 'Rapid Diagnostic Test') |> 
  as_tibble() |> 
  select(-item_id, -item) |> 
  write.csv('data/train_tscv_malaria.csv', row.names = F)

med_tsb_test_tscv_no_malaria <- med_tsb_test_tscv |> 
  filter(item != 'Artemether + Lumefanthrine' , item != 'Rapid Diagnostic Test') |> 
  as_tibble() |> 
  select(-item_id, -item, -malaria_seasonality) |> 
  write.csv('data/test_tscv_no_malaria.csv', row.names = F)

med_tsb_test_tscv_malaria <- med_tsb_test_tscv |> 
  filter(item == 'Artemether + Lumefanthrine' | item == 'Rapid Diagnostic Test') |> 
  as_tibble() |> 
  select(-item_id, -item, -malaria_seasonality) |> 
  write.csv('data/test_tscv_malaria.csv', row.names = F)





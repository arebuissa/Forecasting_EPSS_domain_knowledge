library(tidyverse)
library(fpp3)

med_qty1 <- read_csv('data/full_issued_data.csv') |>
  janitor::clean_names() |> mutate(month = dmy(month), month = yearmonth(month))
  
med_qty <- read_csv('data/predicotrs_final_tssible_July_04_2024.csv') |> 
  janitor::clean_names() |> mutate(month = yearmonth(month))

itemselec <- med_qty |> filter(item == "Oxytocin") |> select(-item)
ggplot(itemselec, 
       aes(x=month, y=quantity))+
  geom_point()+
  geom_line(aes(group =1))+
  geom_point(data = itemselec |> filter(stock_out==1) |> select(month,quantity),
             size=3, colour="orange")+
  ggthemes::theme_clean()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

med_qty_tsb <- med_qty |> 
  mutate(month = yearmonth(month)) |> 
  as_tsibble(index = month, key = item)

f_horizon <- 6

test_length <- 12

med_tsb_train_tscv <- med_qty_tsb |> 
  filter_index(. ~ as.character(max(med_qty_tsb$month)-(f_horizon))) |>
  stretch_tsibble(.init = (length(unique(med_qty_tsb$month)) - test_length), .step = 1)

# create tscv test data

med_tsb_test_tscv <- med_qty_tsb |> 
  filter_index(as.character(max(med_qty_tsb$month)-(test_length) + 1) ~ .) |>
  slide_tsibble(.size = 6, .step = 1)

# med_tsb_test_tscv_stockout <- med_tsb_test_tscv |> mutate(quantity = if_else(stock_out ==1, NA,quantity))

# filter malaria products and save data

med_tsb_train_tscv_no_malaria <- med_tsb_train_tscv |> 
  filter(item != 'Artemether + Lumefanthrine' , item != 'Rapid Diagnostic Test')

med_tsb_train_tscv_malaria <- med_tsb_train_tscv |> 
  filter(item == 'Artemether + Lumefanthrine' | item == 'Rapid Diagnostic Test')

med_tsb_test_tscv_no_malaria <- med_tsb_test_tscv |> 
  filter(item != 'Artemether + Lumefanthrine' , item != 'Rapid Diagnostic Test')

med_tsb_test_tscv_malaria <- med_tsb_test_tscv |> 
  filter(item == 'Artemether + Lumefanthrine' | item == 'Rapid Diagnostic Test')

# med_tsb_test_tscv_no_malaria_stockout <- med_tsb_test_tscv_no_malaria |> mutate(quantity = if_else(stock_out ==1, NA,quantity))
# med_tsb_test_tscv_malaria_stockout <- med_tsb_test_tscv_malaria |> mutate(quantity = if_else(stock_out ==1, NA,quantity))

# univaiate
fit_univariate <- med_tsb_train_tscv_no_malaria |> model(regression = TSLM(quantity ~ trend()+season()),
                                                         arima = ARIMA(quantity),
                                                         ets = ETS(quantity)
)

# fcst_univariate1 <- fit_univariate |> forecast(h=f_horizon, bootstrap = TRUE)

# Generate 1000 future sample paths
fcst_univariate <- fit_univariate |> generate(h = f_horizon, times = 1000) |>
  mutate(.sim = if_else(.sim < 0, 0, .sim)) |> 
  # Compute forecast distributions from future sample paths
  as_tibble() |>
  group_by(.id, item, .model, month) |> 
  summarise(
    quantity = distributional::dist_sample(list(.sim)), .mean = mean(.sim)
  ) |>
  ungroup() |>
  # Create fable object
  as_fable(index = month, key = c(.id, item,.model),
           distribution = "quantity", response = "quantity")

# fcct_and_test <- fcst_univariate |> full_join(med_tsb_test_tscv_stockout, by = c(".id", "item","month"))
# forecast_nonstock <- fcct_and_test |> select(.id, item, .model, month, quantity= quantity.x,stock_out, .mean) |> filter(stock_out == 0) |> 
#   select(-stock_out) |> as_fable(response = "quantity", distribution = "quantity")

accuracy_univariate <- fcst_univariate |> accuracy(med_qty_tsb, 
                            list(point_accuracy_measures,interval_accuracy_measures, distribution_accuracy_measures, crps_skill= skill_score(CRPS),skill=skill_score(winkler_score)), by =c("item", ".model",".id")) |> 
  drop_na()

accuracy_univariate_h <- fcst_univariate |> group_by(.id,item,.model) |> mutate(h= row_number()) |>  ungroup() |> as_fable(response = "quantity", distribution = "quantity") |> 
accuracy(med_qty_tsb , list(point_accuracy_measures,interval_accuracy_measures, distribution_accuracy_measures, crps_skill= skill_score(CRPS),skill=skill_score(winkler_score)), by =c("item", ".model","h")) |> 
  drop_na()



#regressor no malaria
fit_univariate_regg <- med_tsb_train_tscv_no_malaria |> model(regression_reg = TSLM(quantity ~ trend()+season()+stock_replinish  +fical_year_counting ),
                                                   arima_reg = ARIMA(quantity ~ stock_replinish  +fical_year_counting )
)

# fcst_predictors1 <- fit_univariate_regg |> 
#   forecast(new_data = med_tsb_test_tscv_no_malaria,bootstrap = TRUE)

fcst_predictors <- fit_univariate_regg |> generate(new_data = med_tsb_test_tscv_no_malaria, times = 1000) |>
  mutate(.sim = if_else(.sim < 0, 0, .sim)) |> 
  # Compute forecast distributions from future sample paths
  as_tibble() |>
  group_by(.id, item, .model, month) |> 
  summarise(
    quantity = distributional::dist_sample(list(.sim)), .mean = mean(.sim)
  ) |>
  ungroup() |>
  # Create fable object
  as_fable(index = month, key = c(.id, item,.model),
           distribution = "quantity", response = "quantity")

# fcct_and_test_predic <- fcst_predictors |> select(.id, item,.model,month,quantity,.mean) |> full_join(med_tsb_test_tscv_no_malaria_stockout, by = c(".id", "item","month"))
# forecast_nonstock_predict <- fcct_and_test_predic |> select(.id, item, .model, month, quantity= quantity.x,stock_out, .mean) |> filter(stock_out == 0) |> 
#   select(-stock_out) |> as_fable(response = "quantity", distribution = "quantity")

accuracy_predic <- fcst_predictors |> accuracy(med_qty_tsb, 
                                                     list(point_accuracy_measures,interval_accuracy_measures, distribution_accuracy_measures, crps_skill= skill_score(CRPS),skill=skill_score(winkler_score)), by =c("item", ".model",".id"))

accuracy_predictor_h <- fcst_predictors |> group_by(.id,item,.model) |> mutate(h= row_number()) |>  ungroup() |> as_fable(response = "quantity", distribution = "quantity") |> 
  accuracy(med_qty_tsb, list(point_accuracy_measures,interval_accuracy_measures, distribution_accuracy_measures, crps_skill= skill_score(CRPS),skill=skill_score(winkler_score)), by =c("item", ".model","h"))

#regressor malaria
fit_univariate_regg_malaria <- med_tsb_train_tscv_malaria |> model(regression_reg = TSLM(quantity ~ trend()+season()+malaria_seasonality+stock_replinish +fical_year_counting),
                                                              arima_reg = ARIMA(quantity ~ malaria_seasonality+stock_replinish +fical_year_counting)
)

# fcst_predictors_malaria1 <- fit_univariate_regg_malaria |> 
#   forecast(new_data = med_tsb_test_tscv_malaria,bootstrap = TRUE)

fcst_predictors_malaria <- fit_univariate_regg_malaria |> generate(new_data = med_tsb_test_tscv_malaria, times = 1000) |>
  mutate(.sim = if_else(.sim < 0, 0, .sim)) |> 
  # Compute forecast distributions from future sample paths
  as_tibble() |>
  group_by(.id, item, .model, month) |> 
  summarise(
    quantity = distributional::dist_sample(list(.sim)), .mean = mean(.sim)
  ) |>
  ungroup() |>
  # Create fable object
  as_fable(index = month, key = c(.id, item,.model),
           distribution = "quantity", response = "quantity")

# fcct_and_test_predic_malaria <- fcst_predictors_malaria |> select(.id, item,.model,month,quantity,.mean) |> full_join(med_tsb_test_tscv_malaria_stockout, by = c(".id", "item","month"))
# forecast_nonstock_predict_malaria <- fcct_and_test_predic_malaria |> select(.id, item, .model, month, quantity= quantity.x,stock_out, .mean) |> filter(stock_out == 0) |> 
#   select(-stock_out) |> as_fable(response = "quantity", distribution = "quantity")

accuracy_predic_malaria <- fcst_predictors_malaria |> accuracy(med_qty_tsb, 
                                                         list(point_accuracy_measures,interval_accuracy_measures, distribution_accuracy_measures, crps_skill= skill_score(CRPS),skill=skill_score(winkler_score)), by =c("item", ".model",".id"))

accuracy_univariate_h__malaria <- fcst_predictors_malaria |> group_by(.id,item,.model) |> mutate(h= row_number()) |>  ungroup() |> as_fable(response = "quantity", distribution = "quantity") |> 
  accuracy(med_qty_tsb, list(point_accuracy_measures,interval_accuracy_measures, distribution_accuracy_measures, crps_skill= skill_score(CRPS),skill=skill_score(winkler_score)), by =c("item", ".model","h"))

accuracy_id <- accuracy_predic |> bind_rows(accuracy_predic_malaria) |> bind_rows(accuracy_univariate) |> select(.id,item,.model,.id,MASE, RMSSE,crps_skill,skill,winkler,CRPS)
accuracy_h <- accuracy_predictor_h |> bind_rows(accuracy_univariate_h__malaria) |> bind_rows(accuracy_univariate_h) |> select(h,item,.model,MASE, RMSSE,crps_skill,skill,winkler,CRPS)

write_rds(accuracy_id,"accuracy_id.rds")
write_rds(accuracy_h,"accuracy_h.rds")

write_csv(accuracy_id,"accuracy_id.csv")
write_csv(accuracy_h,"accuracy_h.csv")

ggplot(accuracy_id, aes(x = crps, y = .model))+
  geom_boxplot()


ggplot(accuracy_h, aes(x = h, y =RMSSE, fill = .model))+
  geom_boxplot()
  ggthemes::scale_fill_colorblind()

  accuracy_id |> group_by(.model) |> 
    summarise(MASE =mean(MASE), RMSSE=mean(RMSSE), crps=mean(crps), skill=mean(skill))
  
  
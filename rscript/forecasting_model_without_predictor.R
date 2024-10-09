install.packages("urca") # installed packages 

library(fpp3)
library(readr)
library(tictoc)
library(distributional)

# Read data
monthly_issues_tsibble <- read_rds("data/monthly_issues_tsibble.rds") |> filter_index("2017 Aug" ~ .)

# we split the data into train an test

f_horizon <- 6 #  forecast horizon = 6 months
percentage_test <- 0.3 #20% of time series for test set



train <- monthly_issues_tsibble |>
  filter_index(. ~ as.character(max(monthly_issues_tsibble$month)-(round(percentage_test*length(unique(monthly_issues_tsibble$month))))))

test <- monthly_issues_tsibble |> 
  filter_index(as.character(max(monthly_issues_tsibble$month)-round(percentage_test*length(unique(monthly_issues_tsibble$month)))+1) ~ .)



#Time series cross validation. Create different rolling origin series (i.e. id)
train_tscv <- monthly_issues_tsibble |> 
  filter_index(. ~ as.character(max(monthly_issues_tsibble$month)-(f_horizon))) |>  
  stretch_tsibble(.init = length(unique(train$month)), .step = 1)


# Specify and train models

model_tscv <- train_tscv |>
  model(
    mean = MEAN(quantity_product_issued),#total average
    naive = NAIVE(quantity_product_issued),#naive
    snaive = SNAIVE(quantity_product_issued),#seasonal naive
    exponential_smoothing = ETS(quantity_product_issued),#exponential smoothing naive
    arima = ARIMA(quantity_product_issued),#ARIMA,
    regression = TSLM(quantity_product_issued ~ trend() + season()),#regression
  )

write_rds(model_tscv,"result/ae_model_tscv.rds")

# ae_model_tscv <- read_rds("result/ae_model_tscv.rds")
# Produce forecast

ae_fcst_tscv <-  model_tscv |> forecast(h=f_horizon)

forecast_truncated <-  ae_fcst_tscv |> 
  mutate(quantity_product_issued= dist_truncated(quantity_product_issued, lower = 0))
ae_fcst_tscv1 <- forecast_truncated |> hilo(level = 95) |> unpack_hilo(`95%`)
write_rds(forecast_truncated,"result/forecast_truncated.rds")

#Evaluate forecast accuracy
#point_accuracy_measures, interval_accuracy_measures, and distribution_accuracy_measures)
fc_accuracy_without_predictor <- forecast_truncated |> 
  accuracy(monthly_issues_tsibble,
           measures = list(point_accuracy_measures,
                           interval_accuracy_measures,
                           distribution_accuracy_measures
           ))

write_rds(fc_accuracy_without_predictor,"result/fc_accuracy_without_predictor.rds")

fc_accuracy_overall__without_predictor <- fc_accuracy_without_predictor |> group_by(.model) |> summarise(
  RMSSE = mean(RMSSE), MASE = mean(MASE),RMSE = mean(RMSE), MAE = mean(MAE),winkler  = mean(winkler), CRPS =mean(CRPS)
) |> select(.model,RMSE,MAE,MASE,RMSSE,winkler,CRPS)

write_csv(fc_accuracy_overall__without_predictor,"result/fc_accuracy_overall__without_predictor.csv")

fc_accuracy_overall__without_predictor


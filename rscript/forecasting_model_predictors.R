
library(distributional)
library(MASS)
library(writexl)
library(openxlsx)
library(fpp3)
library(readr)
library(readxl)



write.xlsx(full_issues_correct_amountissues, "C:/Users/issaa/Documents/EPSS_research/epps_forecasting_inventory/data.xlsx")

#write.xlsx(full_issues_correct_amountissues, "C:/Users/issaa/Documents/EPSS_research/epps_forecasting_inventory/data.xlsx")

monthly_issues_tsibble <- read_rds("data/monthly_issues_tsibble.rds") |> filter_index("2017 Aug" ~ .)

monthly_predictors_tsibble <- read_excel("data/predictors_Final_Tssible 3.xlsx") |> janitor::clean_names() |> 
  mutate(month = yearmonth(month)) |> as_tsibble(index = month, key = item) |> 
  filter_index("2017 Aug" ~ .) |> filter(item  !="Rifampicin + Isoniazid + Pyrazinamide + Ethambutol (RHZE)")

monthly_issues_all <- monthly_issues_tsibble |> left_join(monthly_predictors_tsibble) |> 
  mutate(fical_year_counting_lag = lag(fical_year_counting), fical_year_counting_laed = lead(fical_year_counting))
#write_rds(monthly_issues_all,"data/monthly_issues_all.rds")

# we split the data into train an test

f_horizon <- 6 #  forecast horizon = 6 months
percentage_test <- 0.3 #20% of time series for test set

monthly_issues_all |> filter(item == "Artemether + Lumefanthrine") -> malaria_data

train <- monthly_issues_all |>
  filter_index(. ~ as.character(max(monthly_issues_all$month)-(round(percentage_test*length(unique(monthly_issues_all$month))))))

test <- monthly_issues_all |> 
  filter_index(as.character(max(monthly_issues_all$month)-round(percentage_test*length(unique(monthly_issues_all$month)))+1) ~ .)



#Time series cross validation. Create different rolling origin series (i.e. id)
train_tscv <- monthly_issues_all |> 
  filter_index(. ~ as.character(max(monthly_issues_all$month)-(f_horizon))) |>  
  stretch_tsibble(.init = length(unique(train$month)), .step = 1)


test_tscv <- test |> 
  slide_tsibble(.size = f_horizon, .step = 1, .id = ".id")

# train_tscv |> filter(.id==13) |> tail()
# test_tscv |> filter(.id==13) |> head() |> View()



# Specify and train models

model_tscv_predictor <- train_tscv |>
  model(
    arima_predictor = ARIMA(quantity_product_issued ~ fical_year_counting+budget_release+covid_19+fical_year_counting_lag+fical_year_counting_laed),#ARIMA
    regression_predictor= TSLM(quantity_product_issued ~ trend() + season() +fical_year_counting+budget_release+covid_19+fical_year_counting_lag+fical_year_counting_laed)#regression
  )

#report(model_tscv_predictor)

###### Creat new model without malaria seasonality


ae_fcst_tscv_predictor <- model_tscv_predictor |> forecast(new_data = test_tscv)

forecast_truncated <-  ae_fcst_tscv_predictor |> 
  mutate(quantity_product_issued= dist_truncated(quantity_product_issued, lower = 0)) 


#Evaluate forecast accuracy
#point_accuracy_measures, interval_accuracy_measures, and distribution_accuracy_measures)
fc_accuracy_predictor <- forecast_truncated |> 
  accuracy(monthly_issues_all,
           measures = list(point_accuracy_measures,
                           interval_accuracy_measures,
                           distribution_accuracy_measures
           ))




fc_accuracy_predictor_overall <- fc_accuracy_predictor |> group_by(.model) |> summarise(
  RMSSE = mean(RMSSE), MASE = mean(MASE),RMSE = mean(RMSE), MAE = mean(MAE),winkler  = mean(winkler), CRPS =mean(CRPS)
) |> select(.model,RMSE,MAE,MASE,RMSSE,winkler,CRPS)

write_csv(fc_accuracy_predictor_overall,"result/fc_accuracy_predictor_overall.csv")

fc_accuracy_predictor_overall



### accuracy by item and by model

fc_accuracy_predictor_overall %>% group_by(item, .model) %>%
  
  summarise(ME = mean(ME), RMSE = mean(RMSE),MAE = mean(MAE), MPE =mean(MPE), MASE = mean(MASE),CRPS = mean(CRPS))
  
  
  
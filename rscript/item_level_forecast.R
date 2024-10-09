library(distributional)
library(fpp3)
library(tidyverse)
library(dplyr)
library(tsibble)


f_horizon <- 12 #  forecast horizon = 12 months
percentage_test <- 0.3 #20% of time series for test set
monthly_issues_all <- read_rds("data/monthly_issues_all.rds")
item_selected <- "Metronidazole - 250mg - Capsule" 
# [1] "Adrenaline (Epinephrine) - 0.1% in 1ml Ampoule - Injection"        
# [2] "Albendazole - 400mg - Tablet"                                      
# [3] "Amlodipine - 5mg - Tablet"                                         
# [4] "Amoxicillin - 500mg - Capsule"                                     
# [5] "Anti-Rho (D)"                                                      
# [6] "Artemether + Lumefanthrine"                                        
# [7] "Atenolol - 50mg - Tablet"                                          
# [8] "Atrovastatin - 20mg - Tablet"                                      
# [9] "Ceftriaxone"                                                       
# [10] "Ciprofloxacin - 500mg - Tablet"                                    
# [11] "Dextrose"                                                          
# [12] "Ferrous + Folic Acid"                                              
# [13] "Frusemide - 10mg/ml in 2ml Ampoule - Injection"                    
# [14] "Gentamicin"                                                        
# [15] "Hydralazine - 20mg/ml in 1ml Ampoule - Injection"                  
# [16] "Insulin Isophane Human(Suspension)"                                
# [17] "Insulin Soluble Human"                                             
# [18] "Insuline Isophane Biphasic"                                        
# [19] "Lamivudine  + Efavirenz +Tenofovir"                                
# [20] "Lamivudine + Zidovudine"                                           
# [21] "Lidocaine HCL"                                                     
# [22] "Magnesium Sulphate"                                                
# [23] "Medroxyprogesterone"                                               
# [24] "Metformin - 500mg - Tablet"                                        
# [25] "Metronidazole - 250mg - Capsule"                                   
# [26] "Omeperazole - 20 mg - Capsule (Enclosing Enteric Coated Granules)" 
# [27] "Omeperazole - 4mg/ml in 10ml - Injection"                          
# [28] "Oral Rehydration Salt"                                             
# [29] "Oxytocin"                                                          
# [30] "Pentavalent"                                                       
# [31] "Propylthiouracil - 100mg - Tablet"                                 
# [32] "RHZ (Rifampicin + Isoniazid + Pyrazinamide)"                       
# [33] "Rapid Diagnostic Test"                                             
# [34] "Ringer's Injection"                                                
# [35] "Sodium Chloride (Normal Saline)"                                   
# [36] "Sulphamethoxazole + Trimethoprim - (200mg + 40mg)/5ml - Suspension"
# [37] "Tetracycline - 1% - Eye Ointment" 

monthly_issues_all |> filter(item == item_selected) |> drop_na()-> item_data

  
train <- item_data |> filter_index(. ~ as.character(max(item_data$month)-(round(percentage_test*length(unique(item_data$month))))))

test <- item_data |> 
  filter_index(as.character(max(item_data$month)-round(percentage_test*length(unique(item_data$month)))+1) ~ .)



#Time series cross validation. Create different rolling origin series (i.e. id)
train_tscv <- item_data |> 
  filter_index(. ~ as.character(max(item_data$month)-(f_horizon))) |>  
  stretch_tsibble(.init = length(unique(train$month)), .step = 1)


test_tscv <- test |> 
  slide_tsibble(.size = f_horizon, .step = 1, .id = ".id")

STLF <- decomposition_model(
  STL(quantity_product_issued ~ season(window = Inf)),
  ETS(season_adjust ~ season("N"))
)

# Specify and train models

model_tscv_predictor <- train_tscv |>
  model(
    arima_predictor = ARIMA(quantity_product_issued ~ fical_year_counting+budget_release+covid_19+fical_year_counting_lag+fical_year_counting_laed+conflict+budget_closure+stock_out),#ARIMA
    regression_predictor= TSLM(quantity_product_issued ~ trend() + season() +fical_year_counting+budget_release+covid_19+fical_year_counting_lag+fical_year_counting_laed+conflict+budget_closure+stock_out),#regression
    mean = MEAN(quantity_product_issued),#total average
    naive = NAIVE(quantity_product_issued),#naive
    snaive = SNAIVE(quantity_product_issued),#seasonal naive
    exponential_smoothing = ETS(quantity_product_issued),#exponential smoothing naive
    arima = ARIMA(quantity_product_issued),#ARIMA,
    regression = TSLM(quantity_product_issued ~ trend() + season()),#regression
    stlf = STLF # decomposition
    ) 

model_tscv_predictor_comb <- model_tscv_predictor |> mutate(combination = (stlf+arima+exponential_smoothing)/3)


ae_fcst_tscv_predictor <- model_tscv_predictor_comb |> forecast(new_data = test_tscv)
#ae_fcst_tscv_predictor |> hilo(level = c(80,95)) |> unpack_hilo(c(`80%`,`95%`)) |> View()


forecast_truncated <-  ae_fcst_tscv_predictor |> 
  mutate(quantity_product_issued= dist_truncated(quantity_product_issued, lower = 0)) 
#Evaluate forecast accuracy
#point_accuracy_measures, interval_accuracy_measures, and distribution_accuracy_measures)
fc_accuracy_predictor <- forecast_truncated |> 
  accuracy(item_data,
           measures = list(point_accuracy_measures,
                           interval_accuracy_measures,
                           distribution_accuracy_measures
           ))
fc_accuracy <- fc_accuracy_predictor |> dplyr::select(.model,item, ME, RMSE, MAE, MASE, RMSSE, winkler, CRPS)

write_csv(fc_accuracy, "result/fc_accuracy39.csv")

fc_accuracy_item <- read_csv("result/fc_accuracy1.csv")


fc_accuracy_item <- fc_accuracy_item |> bind_rows(read_csv("result/fc_accuracy37.csv"))


#write_csv(fc_accuracy_item,"result/fc_accuracy_all_item.csv")

view(fc_accuracy)

# lstm model

# Install necessary packages if not already installed

# install.packages(c("keras", "tensorflow"))


# Load libraries

library(keras)
library(tensorflow)
library(tidyverse)
library(tsibble)
library(doParallel)
library(foreach)


# Prepare data ------------------------------------------------------------

# read data

med_qty <- read.csv('data/predicotrs_final_tssible_July_04_2024.csv') |> 
  janitor::clean_names()


# make the tsibble

med_qty_tsb <- med_qty |> 
  mutate(month = yearmonth(month)) |> 
  group_by(item) |> 
  mutate(item_id = sprintf("id_%02d", cur_group_id())) |>  #creating a item id to handle the data easy
  ungroup() |>
  as_tsibble(index = month, key = c(item_id, item)) |> 
  select(-stock_out, -covid_19, -conflict)


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
  slide_tsibble(.size = 6, .step = 1) |> 
  mutate(unique_id = paste0(item_id, '-', .id)) |> 
  rename(horizon = .id)

# univariate model setup (without scaling) --------------------------------------------------


# Define function to create sequences for LSTM

create_sequences <- function(data, target_column, time_steps) {
  X <- array(dim = c(0, time_steps, ncol(data)))
  Y <- NULL
  for (i in 1:(nrow(data) - time_steps)) {
    # X: sequence of time_steps
    sequence <- array(as.matrix(data[i:(i + time_steps - 1), ]), dim = c(1, time_steps, ncol(data)))
    X <- abind::abind(X, sequence, along = 1)
    # Y: the value right after the time_steps window
    Y <- c(Y, data[i + time_steps, target_column])
  }
  return(list(X = X, Y = Y))
}



# Univariate target variable preparation (quantity only)

id <- med_tsb_train_tscv |> 
  as_tibble() |> 
  select(unique_id) |> 
  pull() |> 
  unique()


# Register outer parallel backend with 6 cores

cl_outer <- makeCluster(6)
registerDoParallel(cl_outer)

system.time(uni_prob_pred <- foreach(i = id,
                                     .combine = 'rbind',
                                     .packages=c("doParallel", "foreach", "tidyverse", "tsibble",
                                                 "keras", "tensorflow")) %dopar% {
                                                   
                                                   # i = id[4]
                                                   
                                                   univariate_data <- med_tsb_train_tscv |>
                                                     as_tibble() |> 
                                                     filter(unique_id == i) |>
                                                     mutate(quantity_adj = quantity) |>
                                                     select(quantity_adj)
                                                   
                                                   
                                                   # Set the time steps (sequence length)
                                                   
                                                   time_steps <- 6
                                                   
                                                   
                                                   # Apply the create_sequences function for the univariate case
                                                   
                                                   univariate_sequences <- create_sequences(as.matrix(univariate_data), target_column = 1, time_steps = time_steps)
                                                   
                                                   
                                                   # Reshape the input data for LSTM
                                                   
                                                   x_train_uni <- array(univariate_sequences$X, dim = c(nrow(univariate_sequences$X), time_steps, 1))
                                                   y_train_uni <- univariate_sequences$Y
                                                   
                                                   
                                                   # Same for test data
                                                   
                                                   test_univariate_data <- med_tsb_test_tscv |>
                                                     as_tibble() |> 
                                                     filter(unique_id == id[1]) |>
                                                     bind_rows(med_tsb_test_tscv |>
                                                                 as_tibble() |> 
                                                                 filter(horizon == 7, item_id == substr(id[1], 1, 5))) |>  # To generate 6 steps ahead forecasts while maintaining the sequence length
                                                     mutate(quantity_adj = quantity) |>
                                                     select(quantity_adj)
                                                   
                                                   univariate_sequences_test <- create_sequences(as.matrix(test_univariate_data), target_column = 1, time_steps = time_steps)
                                                   
                                                   x_test_uni <- array(univariate_sequences_test$X, dim = c(nrow(univariate_sequences_test$X), time_steps, 1))
                                                   y_test_uni <- univariate_sequences_test$Y
                                                   
                                                   
                                                   # univariate model --------------------------------------------------------
                                                   
                                                   
                                                   # Define univariate LSTM model
                                                   
                                                   univariate_model <- keras_model_sequential() |>
                                                     layer_lstm(units = 50, input_shape = c(time_steps, 1), return_sequences = FALSE) |>
                                                     layer_dense(units = 100) |>
                                                     layer_dense(units = 1)
                                                   
                                                   
                                                   # Compile the model
                                                   
                                                   univariate_model |> 
                                                     compile(
                                                       loss = 'mean_squared_error',
                                                       optimizer = 'adam'
                                                     )
                                                   
                                                   
                                                   # Train the model
                                                   
                                                   uni_fit <- univariate_model |> 
                                                     fit(
                                                       x_train_uni, y_train_uni, 
                                                       epochs = 100, batch_size = 32, 
                                                       validation_data = list(x_test_uni, y_test_uni)
                                                     )
                                                   
                                                   
                                                   # Function to calculate residuals
                                                   
                                                   calculate_residuals <- function(model, x_data, y_data) {
                                                     preds <- predict(model, x_data)
                                                     residuals <- y_data - preds
                                                     return(residuals)
                                                   }
                                                   
                                                   
                                                   # Function to generate bootstrapped forecasts
                                                   
                                                   generate_bootstrap_forecasts <- function(model, input_data, residuals, n_samples = 1000) {
                                                     predictions <- matrix(nrow = n_samples, ncol = nrow(input_data))
                                                     
                                                     for (i in 1:n_samples) {
                                                       sampled_residuals <- sample(residuals, size = nrow(input_data), replace = TRUE)
                                                       predictions[i, ] <- predict(model, input_data) + sampled_residuals
                                                     }
                                                     
                                                     return(t(predictions))
                                                   }
                                                   
                                                   
                                                   # Univariate model residuals
                                                   
                                                   residuals_uni <- calculate_residuals(univariate_model, x_test_uni, y_test_uni)
                                                   
                                                   
                                                   # Generate bootstrapped probabilistic forecasts for the univariate model
                                                   
                                                   probs_uni_bootstrap <- generate_bootstrap_forecasts(univariate_model, x_test_uni, residuals_uni, n_samples = 1000)
                                                   
                                                   
                                                   # Rename columns from X0 to X999
                                                   
                                                   colnames(probs_uni_bootstrap) <- paste0("X", 0:(ncol(probs_uni_bootstrap)-1))
                                                   
                                                   
                                                   # Bind with the test set
                                                   
                                                   probs_uni_tidy <- med_tsb_test_tscv |>
                                                     as_tibble() |> 
                                                     filter(unique_id == i) |>
                                                     select(item, month, item_id, horizon, quantity) |> 
                                                     bind_cols(probs_uni_bootstrap) |> 
                                                     mutate(prediction = rowMeans(across(starts_with("X")))) 
                                                   
                                                   probs_uni_tidy
                                                   
                                                   
                                                 })


# Stop outer cluster

stopCluster(cl_outer)

write_rds(uni_prob_pred, 'results/lstm_uni_no_scaled.rds')


# univariate model setup --------------------------------------------------


# Define function to create sequences for LSTM

create_sequences <- function(data, target_column, time_steps) {
  X <- array(dim = c(0, time_steps, ncol(data)))
  Y <- NULL
  for (i in 1:(nrow(data) - time_steps)) {
    # X: sequence of time_steps
    sequence <- array(as.matrix(data[i:(i + time_steps - 1), ]), dim = c(1, time_steps, ncol(data)))
    X <- abind::abind(X, sequence, along = 1)
    # Y: the value right after the time_steps window
    Y <- c(Y, data[i + time_steps, target_column])
  }
  return(list(X = X, Y = Y))
}


# Normalize function (LSTM works better with normalized data)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


# Function to rescale data

rescale <- function(x, min, max) {
  return (x * (max - min) + min)
}


# Univariate target variable preparation (quantity only)

id <- med_tsb_train_tscv |> 
  as_tibble() |> 
  select(unique_id) |> 
  pull() |> 
  unique()


# Register outer parallel backend with 6 cores

cl_outer <- makeCluster(6)
registerDoParallel(cl_outer)

system.time(uni_prob_pred <- foreach(i = id,
                                        .combine = 'rbind',
                                        .packages=c("doParallel", "foreach", "tidyverse", "tsibble",
                                                    "keras", "tensorflow")) %dopar% {
                                          
                                          # i = id[4]
                                                      
                                        # store normalization parameters 
                                        
                                        quantity_min <- med_tsb_train_tscv |>
                                          as_tibble() |> 
                                          filter(unique_id == i) |>
                                          select(quantity) |> 
                                          min()
                                        
                                        quantity_max <- med_tsb_train_tscv |>
                                          as_tibble() |> 
                                          filter(unique_id == i) |>
                                          select(quantity) |> 
                                          max()
                                        
                                        univariate_data <- med_tsb_train_tscv |>
                                          as_tibble() |> 
                                          filter(unique_id == i) |>
                                          mutate(quantity_adj = normalize(quantity)) |>
                                          select(quantity_adj)
                                        
                                        
                                        # Set the time steps (sequence length)
                                        
                                        time_steps <- 6
                                        
                                        
                                        # Apply the create_sequences function for the univariate case
                                        
                                        univariate_sequences <- create_sequences(as.matrix(univariate_data), target_column = 1, time_steps = time_steps)
                                        
                                        
                                        # Reshape the input data for LSTM
                                        
                                        x_train_uni <- array(univariate_sequences$X, dim = c(nrow(univariate_sequences$X), time_steps, 1))
                                        y_train_uni <- univariate_sequences$Y
                                        
                                        
                                        # Same for test data
                                        
                                        test_univariate_data <- med_tsb_test_tscv |>
                                          as_tibble() |> 
                                          filter(unique_id == id[1]) |>
                                          bind_rows(med_tsb_test_tscv |>
                                                      as_tibble() |> 
                                                      filter(horizon == 7, item_id == substr(id[1], 1, 5))) |>  # To generate 6 steps ahead forecasts while maintaining the sequence length
                                          mutate(quantity_adj = (quantity - quantity_min) / (quantity_max - quantity_min)) |>
                                          select(quantity_adj)
                                        
                                        univariate_sequences_test <- create_sequences(as.matrix(test_univariate_data), target_column = 1, time_steps = time_steps)
                                        
                                        x_test_uni <- array(univariate_sequences_test$X, dim = c(nrow(univariate_sequences_test$X), time_steps, 1))
                                        y_test_uni <- univariate_sequences_test$Y
                                        
                                        
                                        # univariate model --------------------------------------------------------
                                        
                                        
                                        # Define univariate LSTM model
                                        
                                        univariate_model <- keras_model_sequential() |>
                                          layer_lstm(units = 50, input_shape = c(time_steps, 1), return_sequences = FALSE) |>
                                          layer_dense(units = 100) |>
                                          layer_dense(units = 1)
                                        
                                        
                                        # Compile the model
                                        
                                        univariate_model |> 
                                          compile(
                                            loss = 'mean_squared_error',
                                            optimizer = 'adam'
                                          )
                                        
                                        
                                        # Train the model
                                        
                                        uni_fit <- univariate_model |> 
                                          fit(
                                            x_train_uni, y_train_uni, 
                                            epochs = 100, batch_size = 32, 
                                            validation_data = list(x_test_uni, y_test_uni)
                                          )
                                        
                                        
                                        # Function to calculate residuals
                                        
                                        calculate_residuals <- function(model, x_data, y_data) {
                                          preds <- predict(model, x_data)
                                          residuals <- y_data - preds
                                          return(residuals)
                                        }
                                        
                                        
                                        # Function to generate bootstrapped forecasts
                                        
                                        generate_bootstrap_forecasts <- function(model, input_data, residuals, n_samples = 1000) {
                                          predictions <- matrix(nrow = n_samples, ncol = nrow(input_data))
                                          
                                          for (i in 1:n_samples) {
                                            sampled_residuals <- sample(residuals, size = nrow(input_data), replace = TRUE)
                                            predictions[i, ] <- predict(model, input_data) + sampled_residuals
                                          }
                                          
                                          return(t(predictions))
                                        }
                                        
                                        
                                        # Univariate model residuals
                                        
                                        residuals_uni <- calculate_residuals(univariate_model, x_test_uni, y_test_uni)
                                        
                                        
                                        # Generate bootstrapped probabilistic forecasts for the univariate model
                                        
                                        probs_uni_bootstrap <- generate_bootstrap_forecasts(univariate_model, x_test_uni, residuals_uni, n_samples = 1000)
                                        
                                        
                                        # Rescale predictions
                                        
                                        rescaled_probs_uni_bootstrap <- apply(probs_uni_bootstrap, 2, rescale, min = quantity_min, max = quantity_max)
                                        
                                        
                                        # Rename columns from X0 to X999
                                        
                                        colnames(rescaled_probs_uni_bootstrap) <- paste0("X", 0:(ncol(rescaled_probs_uni_bootstrap)-1))
                                        
                                        
                                        # Bind with the test set
                                        
                                        probs_uni_tidy <- med_tsb_test_tscv |>
                                          as_tibble() |> 
                                          filter(unique_id == i) |>
                                          select(item, month, item_id, horizon, quantity) |> 
                                          bind_cols(rescaled_probs_uni_bootstrap) |> 
                                          mutate(prediction = rowMeans(across(starts_with("X")))) 
                                        
                                        probs_uni_tidy
                                          
                                          
                                        })


# Stop outer cluster

stopCluster(cl_outer)

write_rds(uni_prob_pred, 'results/lstm_uni.rds')


# Multivariate model setup ------------------------------------------------

# Register outer parallel backend with 6 cores

cl_outer <- makeCluster(4)
registerDoParallel(cl_outer)

system.time(multi_prob_pred <- foreach(i = id,
                                     .combine = 'rbind',
                                     .packages=c("doParallel", "foreach", "tidyverse", "tsibble",
                                                 "keras", "tensorflow")) %dopar% {
                                                   
                                                   # i = id[4]
                                                   
                                                   # store normalization parameters 
                                                   
                                                   quantity_min <- med_tsb_train_tscv |>
                                                     as_tibble() |> 
                                                     filter(unique_id == i) |>
                                                     select(quantity) |> 
                                                     min()
                                                   
                                                   quantity_max <- med_tsb_train_tscv |>
                                                     as_tibble() |> 
                                                     filter(unique_id == i) |>
                                                     select(quantity) |> 
                                                     max()
                                                   
                                                   
                                                   # Filter data based on the malaria related products
                                                   
                                                   items <- med_tsb_train_tscv |> 
                                                     as_tibble() |> 
                                                     filter(item %in% c('Artemether + Lumefanthrine', 'Rapid Diagnostic Test')) |> 
                                                     select(unique_id) |> 
                                                     unique() |> 
                                                     pull()
                                                   
                                                   if (i %in% items) {
                                                     multivariate_data <- med_tsb_train_tscv |>
                                                       as_tibble() |>
                                                       filter(unique_id == i) |>
                                                       mutate(quantity_adj = normalize(quantity)) |>
                                                       select(-month, -quantity, -unique_id, -item, -item_id, -horizon)
                                                   } else {
                                                     multivariate_data <- med_tsb_train_tscv |>
                                                       as_tibble() |>
                                                       filter(unique_id == i) |>
                                                       mutate(quantity_adj = normalize(quantity)) |>
                                                       select(-month, -quantity, -unique_id, -item, -item_id, -horizon, -malaria_seasonality)
                                                   }
                                                   
                                                   
                                                   # Prepare sequences for multivariate data
                                                   
                                                   time_steps <- 6
                                                   
                                                   multivariate_sequences <- create_sequences(as.matrix(multivariate_data), target_column = "quantity_adj", time_steps = time_steps)
                                                   
                                                   x_train_multi <- array(multivariate_sequences$X, dim = c(nrow(multivariate_sequences$X), time_steps, ncol(multivariate_data)))
                                                   y_train_multi <- multivariate_sequences$Y
                                                   
                                                   
                                                   # Prepare test data similarly
                                                   
                                                   if (i %in% items) {
                                                     test_multivariate_data <- med_tsb_test_tscv |>
                                                       as_tibble() |>
                                                       filter(unique_id == i) |>
                                                       bind_rows(med_tsb_test_tscv |>
                                                                   as_tibble() |> 
                                                                   filter(horizon == 7, item_id == substr(id[1], 1, 5))) |>  # To generate 6 steps ahead forecasts while maintaining the sequence length
                                                       mutate(quantity_adj = normalize(quantity)) |>
                                                       select(-month, -quantity, -unique_id, -item, -item_id, -horizon)
                                                   } else {
                                                     test_multivariate_data <- med_tsb_test_tscv |>
                                                       as_tibble() |>
                                                       filter(unique_id == i) |>
                                                       bind_rows(med_tsb_test_tscv |>
                                                                   as_tibble() |> 
                                                                   filter(horizon == 7, item_id == substr(id[1], 1, 5))) |>  # To generate 6 steps ahead forecasts while maintaining the sequence length
                                                       mutate(quantity_adj = normalize(quantity)) |>
                                                       select(-month, -quantity, -unique_id, -item, -item_id, -horizon, -malaria_seasonality)
                                                   }
                                                   
                                                   
                                                   # Create sequences for test data
                                                   
                                                   multivariate_sequences_test <- create_sequences(as.matrix(test_multivariate_data), target_column = "quantity_adj", time_steps = time_steps)
                                                   
                                                   x_test_multi <- array(multivariate_sequences_test$X, dim = c(nrow(multivariate_sequences_test$X), time_steps, ncol(test_multivariate_data)))
                                                   y_test_multi <- multivariate_sequences_test$Y
                                                   
                                                   
                                                   # multivariate model ------------------------------------------------------
                                                   
                                                   # Define multivariate LSTM model
                                                   
                                                   multivariate_model <- keras_model_sequential() |>
                                                     layer_lstm(units = 50, input_shape = c(time_steps, ncol(multivariate_data)), return_sequences = FALSE) |>
                                                     layer_dense(units = 100) |>
                                                     layer_dense(units = 1)
                                                   
                                                   
                                                   # Compile the model
                                                   
                                                   multivariate_model |> compile(
                                                     loss = 'mean_squared_error',
                                                     optimizer = 'adam'
                                                   )
                                                   
                                                   
                                                   # Train the model
                                                   
                                                   multi_fit <- multivariate_model |> fit(
                                                     x_train_multi, y_train_multi, 
                                                     epochs = 100, batch_size = 32, 
                                                     validation_data = list(x_test_multi, y_test_multi)
                                                   )
                                                   
                                                   
                                                   # Multivariate model residuals
                                                   
                                                   residuals_multi <- calculate_residuals(multivariate_model, x_test_multi, y_test_multi)
                                                   
                                                   
                                                   # Generate bootstrapped probabilistic forecasts for the univariate model
                                                   
                                                   probs_multi_bootstrap <- generate_bootstrap_forecasts(multivariate_model, x_test_multi, residuals_multi, n_samples = 1000)
                                                   
                                                   
                                                   # Rescale predictions
                                                   
                                                   rescaled_probs_multi_bootstrap <- apply(probs_multi_bootstrap, 2, rescale, min = quantity_min, max = quantity_max)
                                                   
                                                   
                                                   # Rename columns from X0 to X999
                                                   
                                                   colnames(rescaled_probs_multi_bootstrap) <- paste0("X", 0:(ncol(rescaled_probs_multi_bootstrap)-1))
                                                   
                                                   
                                                   # Bind with the test set
                                                   
                                                   probs_multi_tidy <- med_tsb_test_tscv |>
                                                     as_tibble() |> 
                                                     filter(unique_id == i) |>
                                                     select(item, month, item_id, horizon, quantity) |> 
                                                     bind_cols(rescaled_probs_multi_bootstrap) |> 
                                                     mutate(prediction = rowMeans(across(starts_with("X")))) 
                                                   
                                                   probs_multi_tidy             
                                                   
                                                   
                                                 })


# Stop outer cluster

stopCluster(cl_outer)

write_rds(multi_prob_pred, 'results/lstm_multi.rds')














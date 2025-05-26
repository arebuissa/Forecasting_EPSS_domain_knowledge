# Load libraries
library(keras)
library(keras3)
library(tensorflow)
library(tidyverse)
library(tsibble)
library(doParallel)
library(foreach)
library(abind)
library(janitor)

# Set seed for reproducibility
set.seed(123)

# Read and clean data
med_qty <- read.csv('data/predicotrs_final_tssible_July_04_2024.csv') |>
  clean_names()

# Prepare tsibble
med_qty_tsb <- med_qty |>
  mutate(month = yearmonth(month)) |>
  group_by(item) |>
  mutate(item_id = sprintf("id_%02d", cur_group_id())) |>
  ungroup() |>
  as_tsibble(index = month, key = c(item_id, item)) |>
  select(-stock_out, -covid_19, -conflict)

# Set up train/test
f_horizon <- 6
test_length <- 12

med_tsb_train_tscv <- med_qty_tsb |>
  filter_index(. ~ as.character(max(med_qty_tsb$month) - f_horizon)) |>
  stretch_tsibble(.init = (length(unique(med_qty_tsb$month)) - test_length), .step = 1) |>
  mutate(unique_id = paste0(item_id, '-', .id)) |>
  rename(horizon = .id)

med_tsb_test_tscv <- med_qty_tsb |>
  filter_index(as.character(max(med_qty_tsb$month) - (test_length) + 1) ~ .) |>
  slide_tsibble(.size = 6, .step = 1) |>
  mutate(unique_id = paste0(item_id, '-', .id)) |>
  rename(horizon = .id)

# Define helper functions
create_sequences <- function(data, target_column, time_steps) {
  X <- array(dim = c(0, time_steps, ncol(data)))
  Y <- NULL
  for (i in 1:(nrow(data) - time_steps)) {
    sequence <- array(as.matrix(data[i:(i + time_steps - 1), ]), dim = c(1, time_steps, ncol(data)))
    X <- abind::abind(X, sequence, along = 1)
    Y <- c(Y, data[i + time_steps, target_column])
  }
  return(list(X = X, Y = Y))
}

normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

rescale <- function(x, min, max) {
  x * (max - min) + min
}

calculate_residuals <- function(model, x_data, y_data) {
  preds <- predict(model, x_data)
  residuals <- y_data - preds
  return(residuals)
}

generate_bootstrap_forecasts <- function(model, input_data, residuals, n_samples = 1000) {
  predictions <- matrix(nrow = n_samples, ncol = nrow(input_data))
  for (j in 1:n_samples) {
    sampled_residuals <- sample(residuals, size = nrow(input_data), replace = TRUE)
    predictions[j, ] <- predict(model, input_data) + sampled_residuals
  }
  return(t(predictions))
}

# Prepare IDs
id <- med_tsb_train_tscv |>
  as_tibble() |>
  select(unique_id) |>
  pull() |>
  unique()

# Parallel backend
cl_outer <- makeCluster(6)
registerDoParallel(cl_outer)

# Main model training in parallel
system.time(
  uni_prob_pred <- foreach(i = id,
                           .combine = 'rbind',
                           .packages = c("tidyverse", "keras", "tensorflow", "tsibble", "foreach", "doParallel", "abind", "janitor")) %dopar% {
                             
                             # i <- id[1]
                             
                             quantity_min <- med_tsb_train_tscv |>
                               as_tibble() |>
                               filter(unique_id == i) |>
                               pull(quantity) |>
                               min()
                             
                             quantity_max <- med_tsb_train_tscv |>
                               as_tibble() |>
                               filter(unique_id == i) |>
                               pull(quantity) |>
                               max()
                             
                             univariate_data <- med_tsb_train_tscv |>
                               as_tibble() |>
                               filter(unique_id == i) |>
                               mutate(quantity_adj = normalize(quantity)) |>
                               select(quantity_adj)
                             
                             time_steps <- 6
                             
                             univariate_sequences <- create_sequences(as.matrix(univariate_data), target_column = 1, time_steps = time_steps)
                             
                             x_train_uni <- array(univariate_sequences$X, dim = c(nrow(univariate_sequences$X), time_steps, 1))
                             y_train_uni <- univariate_sequences$Y
                             
                             # Prepare correct test data
                             test_univariate_data <- med_tsb_test_tscv |>
                               as_tibble() |>
                               filter(unique_id == i) |>
                               bind_rows(med_tsb_test_tscv |>
                                           as_tibble() |>
                                           filter(horizon == 7, item_id == substr(i, 1, 5))) |>
                               mutate(quantity_adj = (quantity - quantity_min) / (quantity_max - quantity_min)) |>
                               select(quantity_adj)
                             
                             univariate_sequences_test <- create_sequences(as.matrix(test_univariate_data), target_column = 1, time_steps = time_steps)
                             
                             x_test_uni <- array(univariate_sequences_test$X, dim = c(nrow(univariate_sequences_test$X), time_steps, 1))
                             y_test_uni <- univariate_sequences_test$Y
                             
                             # Define model
                             univariate_model <- keras_model_sequential() |>
                               layer_lstm(50, input_shape = c(time_steps, 1), return_sequences = FALSE) |>
                               layer_dropout(0.2) |>
                               layer_dense(100, activation = "relu") |>
                               layer_dense(1)
                             
                             
                             # Compile model
                             univariate_model |> compile(
                               loss = "mean_squared_error",
                               optimizer = optimizer_adam()
                             )
                             
                             # Train model with early stopping
                             uni_fit <- univariate_model |> fit(
                               x = x_train_uni,
                               y = y_train_uni,
                               epochs = 100,
                               batch_size = 32,
                               validation_data = list(x_test_uni, y_test_uni),
                               callbacks = list(callback_early_stopping(monitor = "val_loss", patience = 10, restore_best_weights = TRUE)),
                               verbose = 0
                             )
                             
                             # Residuals and forecasts
                             residuals_uni <- calculate_residuals(univariate_model, x_test_uni, y_test_uni)
                             probs_uni_bootstrap <- generate_bootstrap_forecasts(univariate_model, x_test_uni, residuals_uni, n_samples = 1000)
                             rescaled_probs_uni_bootstrap <- apply(probs_uni_bootstrap, 2, rescale, min = quantity_min, max = quantity_max)
                             
                             colnames(rescaled_probs_uni_bootstrap) <- paste0("X", 0:(ncol(rescaled_probs_uni_bootstrap) - 1))
                             
                             probs_uni_tidy <- med_tsb_test_tscv |>
                               as_tibble() |>
                               filter(unique_id == i) |>
                               select(item, month, item_id, horizon, quantity) |>
                               bind_cols(rescaled_probs_uni_bootstrap) |>
                               mutate(prediction = rowMeans(across(starts_with("X"))))
                             
                             probs_uni_tidy
                             
                           })

# Stop cluster
stopCluster(cl_outer)

# Save results
write_rds(uni_prob_pred, "results/lstm_uni_revised_final.rds")


# Multivariate LSTM model setup ------------------------------------------------

# Pre-compute which IDs have the malaria seasonality feature
malaria_products <- med_tsb_train_tscv |>
  as_tibble() |>
  filter(item %in% c("Artemether + Lumefanthrine", "Rapid Diagnostic Test")) |>
  distinct(unique_id) |>
  pull()

# Parallel backend
cl_outer <- makeCluster(7)
registerDoParallel(cl_outer)

system.time(
  multi_prob_pred <- foreach(
    i = id,
    .combine   = "rbind",
    .packages  = c("tidyverse", "tsibble", "keras3", "tensorflow", "abind", "janitor")
  ) %dopar% {
    
    # i <- id[1]
    
    # Pull out just this series and get its min/max
    df_train <- med_tsb_train_tscv |>
      as_tibble() |>
      filter(unique_id == i)
    quantity_min <- min(df_train$quantity)
    quantity_max <- max(df_train$quantity)
    
    # Build the feature‐matrix (include or drop malaria_seasonality)
    if (i %in% malaria_products) {
      df_train_feats <- df_train |>
        mutate(quantity_adj = (quantity - quantity_min) / (quantity_max - quantity_min)) |>
        select(-month, -quantity, -unique_id, -item, -item_id, -horizon)
    } else {
      df_train_feats <- df_train |>
        mutate(quantity_adj = (quantity - quantity_min) / (quantity_max - quantity_min)) |>
        select(-month, -quantity, -unique_id, -item, -item_id, -horizon, -malaria_seasonality)
    }
    

    time_steps <- 6
    # (2) Locate the target column as an integer index
    target_col <- which(colnames(df_train_feats) == "quantity_adj")
    
    # Create train sequences
    seq_train <- create_sequences(
      as.matrix(df_train_feats),
      target_column = target_col,
      time_steps     = time_steps
    )
    x_train_multi <- array(seq_train$X,
                           dim = c(nrow(seq_train$X), time_steps, ncol(df_train_feats)))
    y_train_multi <- seq_train$Y
    
    # Prepare test features in the same way (and append the horizon=7 window)
    df_test  <- med_tsb_test_tscv |> as_tibble() |> filter(unique_id == i)
    extra    <- med_tsb_test_tscv |> as_tibble() |>
      filter(horizon == 7, item_id == substr(i, 1, 5))
    df_test  <- bind_rows(df_test, extra)
    
    if (i %in% malaria_products) {
      df_test_feats <- df_test |>
        mutate(quantity_adj = (quantity - quantity_min) / (quantity_max - quantity_min)) |>
        select(-month, -quantity, -unique_id, -item, -item_id, -horizon)
    } else {
      df_test_feats <- df_test |>
        mutate(quantity_adj = (quantity - quantity_min) / (quantity_max - quantity_min)) |>
        select(-month, -quantity, -unique_id, -item, -item_id, -horizon, -malaria_seasonality)
    }
    
    
    seq_test <- create_sequences(
      as.matrix(df_test_feats),
      target_column = target_col,
      time_steps     = time_steps
    )
    x_test_multi <- array(seq_test$X,
                          dim = c(nrow(seq_test$X), time_steps, ncol(df_test_feats)))
    y_test_multi <- seq_test$Y
    
    # Build & compile the model
    multivariate_model <- keras_model_sequential() |>
      layer_lstm(units = 50,
                 input_shape     = c(time_steps, ncol(df_train_feats)),
                 return_sequences = FALSE) |>
      layer_dropout(rate = 0.2) |>
      layer_dense(units = 100, activation = "relu") |>
      layer_dense(units = 1)
    
    multivariate_model |> compile(
      loss      = "mean_squared_error",
      optimizer = "adam"
    )
    
    # Fit with early stopping
    multivariate_model |> fit(
      x_train_multi, y_train_multi,
      epochs          = 100,
      batch_size      = 32,
      validation_data = list(x_test_multi, y_test_multi),
      callbacks       = list(
        callback_early_stopping(
          monitor             = "val_loss",
          patience            = 10L,
          restore_best_weights = TRUE
        )
      ),
      verbose = 0
    )
    
    # Residuals & bootstrap forecasts
    resid     <- calculate_residuals(multivariate_model, x_test_multi, y_test_multi)
    boot_preds <- generate_bootstrap_forecasts(
      multivariate_model, x_test_multi, resid, n_samples = 1000
    )
    
    # Rescale back to original units
    rescaled <- apply(boot_preds, 2, rescale, min = quantity_min, max = quantity_max)
    colnames(rescaled) <- paste0("X", 0:(ncol(rescaled) - 1))
    
    # Pred tibb
    multi_prob_pred <- med_tsb_test_tscv |>
      as_tibble() |>
      filter(unique_id == i) |>
      select(item, month, item_id, horizon, quantity) |>
      bind_cols(as_tibble(rescaled)) |>
      mutate(prediction = rowMeans(across(starts_with("X"))))
    
    multi_prob_pred
    
  }  # end foreach
)  # end system.time

# Stop and save
stopCluster(cl_outer)
write_rds(multi_prob_pred, "results/lstm_multi_revised.rds")


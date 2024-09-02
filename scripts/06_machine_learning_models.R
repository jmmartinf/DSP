# Machine Learning Modelling ----
## Tidymodels technique

# Setup ----
library(tidyverse)
library(tidymodels)
library(bonsai)             # Implementation of Ligh Gradient Boosting Machine

load("./data/02_validated.RDATA")
load("./data/03_explore.RData")


# Pre-Processing ----
## Training - Testing - Cross Validation ----
auto_split <- initial_split(corrected, prop = 0.80)
auto_split

auto_train <- training(auto_split)
auto_test  <- testing(auto_split)

auto_cv <- vfold_cv(auto_train, v = 10)

## Define formula ----
predictors_formula <- paste(c(numeric_vars, categorical_vars), collapse = " + ")
formula <- as.formula(paste(target_var, " ~ ", predictors_formula))

## Prepare Data ----
preprocess_recipe <- corrected %>% 
    recipe(formula) %>% 
    step_log(all_outcomes()) %>%                        # Logaritmic Transformation of price
    step_dummy(all_nominal_predictors()) %>%            # Create dummy variables
    step_corr(all_predictors(), threshold = 0.7) %>%    # Revoving predictors highly correlated
    #step_rm("mileage") %>%                             # Removing mileage as High Correlated variable
    step_zv(all_numeric_predictors()) %>%               # Removing Zero Variance variables
    step_normalize(all_numeric_predictors())            # Centering and Scaling numeric variables

# Linear Model ----
## Define Model ----
lm_model <- linear_reg(penalty = tune(),
                       mixture = tune()) %>%
    set_engine("glmnet") %>%
    set_mode("regression")

## Workflow for Linear Regression ----
lm_wf <- workflow() %>%
    add_model(lm_model) %>% 
    add_recipe(preprocess_recipe)

## Hyperparameter Tuning ----
lm_results <- lm_wf %>% 
    tune_grid(resamples = auto_cv,
              metrics = metric_set(rsq, rmse, mae)
              )

lm_results %>% collect_metrics()

## Hyperparameter Selection ----
param_final <- lm_results %>%
    select_best(metric = "mae")

## Finalize workflow ----
lm_wf <- lm_wf %>% finalize_workflow(param_final)

## Last Fit ----
lm_fit <- lm_wf %>% 
    last_fit(auto_split,
             metrics = metric_set(rsq, rmse, mae)
             )

## Test Data Predictions ----
test_performance <- lm_fit %>% collect_predictions()
test_performance

## Performance Metrics ----
auto_metrics <- metric_set(rsq, rmse, mae)
test_performance %>% auto_metrics(truth = target_var, estimate = .pred)

# Support Vector Machines ----
## Define Model ----
svm_model <- svm_rbf(mode = "regression",
            cost = tune(),
            rbf_sigma = tune(),
            engine = "kernlab"
            )

## Workflow for Support vector Machines ----
svm_wf <- workflow() %>%
    add_model(svm_model) %>% 
    add_recipe(preprocess_recipe)

## Hyperparameter Tuning ----
svm_results <- svm_wf %>% 
    tune_grid(resamples = auto_cv,
              metrics = metric_set(rsq, rmse, mae)
    )

svm_results %>%
    collect_metrics() %>%
    pivot_wider(
        id_cols = c(cost, rbf_sigma, .config),
        names_from = .metric,
        values_from = c(mean, std_err)
    )

## Hyperparameter Selection ----
param_final <- svm_results %>%
    select_best(metric = "rmse")

svm_wf <- svm_wf %>% finalize_workflow(param_final)

## Last Fit ----
svm_fit <- svm_wf %>%
    last_fit(auto_split,
             metrics = metric_set(rsq, rmse, mae)
             )

## Test Data Predictions ----
test_performance <- svm_fit %>% collect_predictions()

## Performance metrics ----
auto_metrics <- metric_set(rsq, rmse, mae)
test_performance %>% auto_metrics(truth = target_var, estimate = .pred)

# K-Nearest Neighbor ----
## Define Model ----
knn_model <- nearest_neighbor(mode = "regression",
                              neighbors = tune(),
                              weight_func = tune(),
                              dist_power = tune(),
                              engine = "kknn"
                              )

## Workflow for KNN ----
knn_wf <- workflow() %>%
    add_model(knn_model) %>% 
    add_recipe(preprocess_recipe)

## Hyperparameter Tuning ----
knn_results <- knn_wf %>% 
    tune_grid(resamples = auto_cv,
              metrics = metric_set(rsq, rmse, mae)
              )

knn_results %>% collect_metrics()

## Hyperparameter Selection ----
param_final <- knn_results %>%
    select_best(metric = "mae")

knn_wf <- knn_wf %>%
    finalize_workflow(param_final)

## Last Fit ----
knn_fit <- knn_wf %>%
    last_fit(auto_split,
             metrics = metric_set(rsq, rmse, mae)
             )

## Test Data Predictions ----
test_performance <- knn_fit %>% collect_predictions()

## Performance metrics ----
auto_metrics <- metric_set(rsq, rmse, mae)
test_performance %>% auto_metrics(truth = target_var, estimate = .pred)

# Decision Trees ----
## Define Model ----
dt_model <- decision_tree(mode = "regression",
                          cost_complexity = tune(),
                          tree_depth = tune(),
                          min_n = tune(),
                          engine = "rpart"
                          )

## Workflow for DT ----
dt_wf <- workflow() %>%
    add_model(dt_model) %>% 
    add_recipe(preprocess_recipe)

## Hyperparameter Tuning ----
dt_results <- dt_wf %>% 
    tune_grid(resamples = auto_cv,
              metrics = metric_set(rsq, rmse, mae)
              )

dt_results %>% collect_metrics()

## Hyperparameter Selection ----
param_final <- dt_results %>%
    select_best(metric = "mae")

dt_wf <- dt_wf %>%
    finalize_workflow(param_final)

## Last Fit ----
dt_fit <- dt_wf %>%
    last_fit(auto_split,
             metrics = metric_set(rsq, rmse, mae)
             )

## Test Data Predictions ----
test_performance <- dt_fit %>% collect_predictions()

## Performance metrics ----
auto_metrics <- metric_set(rsq, rmse, mae)
test_performance %>% auto_metrics(truth = target_var, estimate = .pred)

# Random Forest ----
## Define Model ----
rf_model <- rand_forest(mode = "regression",
                        mtry = tune(),
                        trees = tune(),
                        min_n = tune(),
                        engine = "ranger"
                        )

## Workflow for RF ----
rf_wf <- workflow() %>%
    add_model(rf_model) %>% 
    add_recipe(preprocess_recipe)

## Hyperparameter Tuning ----
rf_results <- rf_wf %>% 
    tune_grid(resamples = auto_cv,
              metrics = metric_set(rsq, rmse, mae)
              )

rf_results %>% collect_metrics()

## Hyperparameter Selection ----
param_final <- rf_results %>%
    select_best(metric = "mae")

## Finalize workflow ----
rf_wf <- rf_wf %>% finalize_workflow(param_final)

## Last fit ----
rf_fit <- rf_wf %>% 
    last_fit(auto_split,
             metrics = metric_set(rsq, rmse, mae)
             )

## Test Data Predictions ----
test_performance <- rf_fit %>% collect_predictions()
test_performance

## Performance metrics ----
auto_metrics <- metric_set(rsq, rmse, mae)
test_performance %>% auto_metrics(truth = target_var, estimate = .pred)

# Light Gradient Boosting Machine ----
## Define Model
lgb_model <- boost_tree(mode = "regression",
                         mtry = tune(),
                         trees = tune(),
                         min_n = tune(),
                         tree_depth = tune(),
                         learn_rate = tune(),
                         loss_reduction = tune(),
                         engine = "lightgbm"
                         )

## Workflow for LGBM ----
lgb_wf <- workflow() %>%
    add_model(lgb_model) %>% 
    add_recipe(preprocess_recipe)

## Hyperparameter Tuning ----
lgb_results <- lgb_wf %>% 
    tune_grid(resamples = auto_cv,
              metrics = metric_set(rsq, rmse, mae)
              )

lgb_results %>% collect_metrics()

## Hyperparameter Selection ----
param_final <- lgb_results %>%
    select_best(metric = "mae")

## Finalize workflow ----
lgb_wf <- lgb_wf %>% finalize_workflow(param_final)

## last fit ----
lgb_fit <- lgb_wf %>%
    last_fit(auto_split,
             metrics = metric_set(rsq, rmse, mae)
             )

## Test Data Predictions ----
test_performance <- lgb_fit %>% collect_predictions()
test_performance

## Performormance metrics ----
auto_metrics <- metric_set(rsq, rmse, mae)
test_performance %>% auto_metrics(truth = target_var, estimate = .pred)

# Extreme Gradient Boosting ----
## Define Model ----
xgb_model <- boost_tree( mode = "regression",
                             mtry = tune(), 
                             trees = tune(),
                             min_n = tune(),
                             tree_depth = tune(),
                             learn_rate = tune(),
                             loss_reduction = tune(),
                             sample_size = tune(),
                             stop_iter = tune(),
                             engine = "xgboost"
                             )

## Workflow for XGB ----
xgb_wf <- workflow() %>%
    add_model(xgb_model) %>% 
    add_recipe(preprocess_recipe)

## Hyperparameter Tuning ----
xgb_results <- xgb_wf %>% 
    tune_grid(resamples = auto_cv,
              metrics = metric_set(rsq, rmse, mae)
              )

xgb_results %>% collect_metrics()

## Final Hyperparameter ----
param_final <- xgb_results %>%
    select_best(metric = "mae")

## Finalize workflow ----
xgb_wf <- xgb_wf %>% finalize_workflow(param_final)

## Last fit ----
xgb_fit <- xgb_wf %>%
    last_fit(auto_split,
             metrics = metric_set(rsq, rmse, mae)
             )

## Test Data Predictions ----
test_performance <- xgb_fit %>% collect_predictions()
test_performance

## Performance metrics ----
auto_metrics <- metric_set(rsq, rmse, mae)
test_performance %>% auto_metrics(truth = target_var, estimate = .pred)

# Multilayer Perceptron ----
## Define Model ----
mlp_model <- mlp(mode = "regression",
                 hidden_units = tune(),
                 penalty = tune(),
                 epochs = tune(),
                 engine = "nnet"
                 )

## Workflow for MLP ----
mlp_wf <- workflow() %>%
    add_model(mlp_model) %>% 
    add_recipe(preprocess_recipe)

## Hyperparameter Tuning ----
mlp_results <- mlp_wf %>% 
    tune_grid(resamples = auto_cv,
              metrics = metric_set(rsq, rmse, mae)
              )

mlp_results %>% collect_metrics()

## Hyperparameter Selection ----
param_final <- mlp_results %>%
    select_best(metric = "mae")

## Finalize workflow ----
mlp_wf <- mlp_wf %>% finalize_workflow(param_final)

## last fit ----
mlp_fit <- mlp_wf %>%
    last_fit(auto_split,
             metrics = metric_set(rsq, rmse, mae)
             )

## Test Data Predictions ----
test_performance <- mlp_fit %>% collect_predictions()
test_performance

 ## Performance metrics ----
auto_metrics <- metric_set(rsq, rmse, mae)
test_performance %>% auto_metrics(truth = target_var, estimate = .pred)

# Compare metrics ----
compare_models <- function(model_prefixes) {
    # Función para extraer métricas de un modelo
    extract_metrics <- function(model_name) {
        full_model_name <- paste0(model_name, "_fit")
        if (exists(full_model_name, envir = parent.frame())) {
            model <- get(full_model_name, envir = parent.frame())
            model %>%
                collect_metrics() %>%
                mutate(model = model_name)
        } else {
            warning(paste("Model", full_model_name, "not found. Skipping."))
            return(NULL)
        }
    }
    
    # Aplicar extract_metrics a cada modelo en la lista de nombres
    model_prefixes %>%
        map_dfr(~ extract_metrics(.x)) %>%
        pivot_wider(id_cols = c(model), 
                    names_from = .metric, 
                    values_from = .estimate)
}

# Compare Results
extract_results <- function(model_names) {
    results_list <- map(model_names, function(model_name) {
        full_model_name <- paste0(model_name, "_fit")
        if (exists(full_model_name, envir = parent.frame())) {
            model <- get(full_model_name, envir = parent.frame())
            predictions <- model %>%
                collect_predictions() %>%
                select(.pred) %>% 
                mutate(.pred = exp(.pred)) %>% 
                rename(!!model_name := .pred)
            return(predictions)
        } else {
            warning(paste("Model", full_model_name, "not found!. Skipping."))
            return(NULL)
        }
    })

    # Combina las predicciones por columna junto con los datos del subconjunto de testing
    bind_cols(results_list, testing(auto_split))
}

# Metrics
model_prefixes <- c("lm", "svm", "knn", "dt", "rf", "lgb", "xgb", "mlp")
metrics_table <- compare_models(model_prefixes) %>% 
    arrange(desc(rsq), rmse, mae)
results_table <- extract_results(model_prefixes)

# Response-Prediction Plot
predict_plot <- results_table %>%
    pivot_longer(
        cols = all_of(model_prefixes),  # Selecciona las columnas de predicciones
        names_to = "ml_model",   # Nombre de la nueva columna para los nombres de los modelos
        values_to = "pred"    # Nombre de la nueva columna para los valores de las predicciones
    ) %>% 
    mutate(ml_model = factor(ml_model, levels = model_prefixes, ordered = TRUE)) %>% 
    ggplot(aes(x = pred, y = price)) +
    geom_point(size = 1, alpha = 0.7) +
    geom_abline(slope = 1, color = "red", linetype = "dashed") +
    facet_wrap(~ ml_model, ncol = 4) +
    coord_fixed(ratio = 1) +
    labs(x = "Predictions", y = "Price") +
    ggtitle("Response - Prediction Plot") +
    theme_bw()
ggsave("./figures/06_response_prediction.png", 
       plot = predict_plot, 
       width = 9, 
       height = 5.0)

# Metrics of Mean of Predictions
metrics_mean_pred <- map_dfc(model_prefixes, function(model_name) {
    full_model_name <- paste0(model_name, "_fit")
    if (exists(full_model_name, envir = parent.frame())) {
        model <- get(full_model_name, envir = parent.frame())
        predictions <- model %>%
            collect_predictions() %>%
            select(.pred) %>% 
            rename(!!model_name := .pred)
        
    } else {
        warning(paste("Model", full_model_name, "not found. Skipping."))
        return(NULL)
    }    
    }) %>% 
    mutate(mean_pred = rowMeans(across(everything()), na.rm = TRUE)) %>% 
    select(mean_pred) %>% 
    bind_cols(log(testing(auto_split)[, target_var])) %>% 
    mutate(residual = !!sym(target_var) - mean_pred) %>% 
    summarise(
        # Calcular R²
        rsq = rsq_vec(!!sym(target_var), mean_pred),
        # Calcular RMSE
        rmse = rmse_vec(!!sym(target_var), mean_pred),
        # Calcular MAE
        mae = mae_vec(!!sym(target_var), mean_pred)
    )

# Response - Mean of Predictions Plot
mean_predict_plot <- results_table %>%
    rowwise() %>% 
    mutate(mean_pred = mean(c_across(all_of(model_prefixes)))) %>% 
    ungroup() %>% 
    ggplot(aes(x = mean_pred, y = price)) +
    geom_point(size = 1, alpha = 0.7) +
    geom_abline(slope = 1, color = "red", linetype = "dashed") +
    labs(x = "Price", y = "Prediction") +
    ggtitle("Response - Mean of Prediction Plot") +
    theme_bw()

# Residuals
residuals_table <- map_dfc(model_prefixes, function(model_name) {
    full_model_name <- paste0(model_name, "_fit")
    if (exists(full_model_name, envir = parent.frame())) {
        model <- get(full_model_name, envir = parent.frame())
        predictions <- model %>%
            collect_predictions() %>%
            select(.pred, !!sym(target_var))  %>%  # Extrae predicciones y variable dependiente como vectores
            mutate(residuals = !!sym(target_var) - .pred) %>%   # Calcular los residuos
            select(residuals) %>% 
            rename(!!model_name := residuals)
    } else {
        warning(paste("Model", full_model_name, "not found!. Skipping."))
        return(tibble(!!paste0(model_name, "_resid") := NA))  # Retorna NA si el modelo no existe
    }
}) 

# Residuals Plot
## Histograms
residuals_plot <- residuals_table %>% 
    pivot_longer(everything(), names_to = "model", values_to = "residuals") %>% 
    mutate(model = factor(model, levels = model_prefixes, ordered = TRUE)) %>% 
    ggplot(aes(x = residuals)) +
    geom_histogram(bins = 50, fill = "steelblue", color = "white") +
    facet_wrap(~ model, scales = "fixed", ncol = 4) +
    labs(x = "Residuals", y = "Frequency") +
    ggtitle("Histograms of Residuals by Model") +
    theme_bw()
ggsave("./figures/06_residuals_histogram.png", 
       plot = residuals_plot, 
       width = 8, 
       height = 6)

# Q-Q plot
qq_plot <- residuals_table %>% 
    pivot_longer(everything(), names_to = "model", values_to = "residuals") %>% 
    mutate(model = factor(model, levels = model_prefixes, ordered = TRUE)) %>% 
    ggplot(aes(sample = residuals)) +
    stat_qq(size = 1, alpha = 0.7) +
    stat_qq_line(dparams = list(mean = 0, sd = 0.9), color = "red") +
    facet_wrap(~ model, scales = "fixed", ncol = 4) +
    labs(title = "Q-Q Plots of Residuals by Model",
         x = "Theoric Quantile",
         y = "Residues") +
    theme_bw()
ggsave("./figures/06_residuals_qqplot.png", 
       plot = qq_plot, 
       width = 8, 
       height = 6)

## Guardar los dataframes y variables en un archivo .RData ----
save(lm_fit,
     svm_fit,
     knn_fit,
     dt_fit,
     rf_fit,
     lgb_fit,
     xgb_fit,
     mlp_fit,
     file = "./data/06_models.RData"
     )
save(metrics_table,
     results_table,
     metrics_mean_pred,
     file = "./data/06_metrics.RData"
     )

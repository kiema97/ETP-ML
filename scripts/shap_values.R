## Netoyage
rm(list = ls())
## Librairies
library(tidymodels)
library(kernlab)
library(DALEX)
library(shapper)
#shapper::install_shap()
# Example dataset
#data(iris)
#data(ames, package = "modeldata")


# Split the data into training and testing sets
set.seed(123)
data_split <- initial_split(dragons[,-5], prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Preprocessing recipe
recipe <- recipe(life_length ~ ., data = train_data) %>%
  step_normalize(all_predictors())

# Define the SVM model with RBF kernel
svm_model <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

# Create a workflow
workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(svm_model)

# Define the grid for hyperparameter tuning
param_grid <- grid_regular(cost(), rbf_sigma(), levels = 5)

# Perform cross-validation
set.seed(123)
cv_folds <- vfold_cv(train_data, v = 4)

# Tune the model
tuned_results <- tune_grid(
  workflow,
  resamples = cv_folds,
  grid = param_grid,
  metrics = metric_set(rmse)
)

# Select the best hyperparameters
best_params <- select_best(tuned_results, metric ="rmse")

# Finalize the workflow with the best parameters
final_workflow <- finalize_workflow(workflow, best_params)

# Fit the final model on the entire training set
final_model <- fit(final_workflow, data = train_data)

# Make predictions on the test set
predictions <- predict(final_model, test_data) %>%
  bind_cols(test_data)

# Evaluate model performance
metrics <- predictions %>%
  metrics(truth = life_length, estimate = .pred)

print(metrics)




individual_variable_effect(final_model, data = train_data[,-7], 
                           predict_function = predict,
                           new_observation = train_data[1:10,-7], 
                           nsamples = 50)


# Create an explainer for SHAP values
# Définir explicitement la fonction de prédiction
predict_function <- function(model, new_data) {
  predictions <- predict(model, new_data)
  # Si le modèle de régression renvoie une seule colonne de prédictions
  if (is.data.frame(predictions)) {
    return(predictions$.pred)
  } else {
    return(predictions)
  }
}
predict_function_gbm <-  function(model, newdata) {
  predict(model, newdata) %>% pluck(.,1)
}
# Créer l'explainer
train_data2 <- train_data %>% select(-life_length)
explainer <- DALEX::explain(
  model = final_model,
  predict_function = predict,
  data = train_data,
  y = train_data$life_length,
  predict_function_target_column="life_length",
  type="regression"
)

library(shapviz)
bd <- explainer |> 
  predict_parts(train_data[7, ], keep_distributions = FALSE) |> 
  shapviz()

sv_waterfall(bd)
sv_force(bd)
sv_importance(bd)
sv_dependence(bd,v = "life_length")
sv_dependence2D(bd)
sv_interaction(bd)
DALEX::variable_importance(explainer)
sv_
# Réduire le nombre d'échantillons de fond en utilisant shap.sample

shap_values <- shapper::shap(explainer,data=train_data2,
                             new_observation = train_data2[1:20,],
                             predict_function = predict)


vignette("basic_use",package="shapviz")

# Plot SHAP summary
plot(shap_values)

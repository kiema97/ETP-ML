# Installation des packages nécessaires
#install.packages(c("tidymodels", "kernlab", "ranger", "xgboost", "earth", "nnet", "kknn", "shapviz", "DALEX"))

# Chargement des bibliothèques
library(tidymodels)
library(kernlab)
library(ranger)
library(xgboost)
library(earth)
library(nnet)
library(kknn)
library(shapviz)
library(DALEX)
library(bonsai)


# WORK SPACE
setwd("G:/PROJET/Article/ET_ML")
# Chargement des données
data(dragons) # Assurez-vous que vous avez votre jeu de données
x <- colnames(dragons)[-c(5,8)]
# Division des données en ensembles d'entraînement et de test
set.seed(123)
data_split <- initial_split(dragons[,-5], prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Prétraitement des données
recipe <- recipe(life_length ~ ., data = train_data) %>%
  step_normalize(all_predictors())


# 1. Modèle Boosted Trees
# Définir le modèle Boosted Trees
boosted_trees_model <- boost_tree(
  trees = 1000,
  tree_depth = 6,
  min_n = 2,
  loss_reduction = 0.01,
  sample_size = 0.8,
  mtry = 2,
  learn_rate = 0.01
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# Créer le workflow
boosted_trees_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(boosted_trees_model)

# Validation croisée
cv_folds <- vfold_cv(train_data, v = 4)

# Entraîner et évaluer le modèle
boosted_trees_fit <- fit_resamples(
  boosted_trees_workflow,
  resamples = cv_folds,
  metrics = metric_set(rmse)
)

# Sélectionner le meilleur modèle
best_params <- select_best(boosted_trees_fit, metric="rmse")

# Finaliser et ajuster le modèle final
final_boosted_trees <- finalize_workflow(boosted_trees_workflow, best_params)
boosted_trees_final_model <- fit(final_boosted_trees, data = train_data)

# Prédictions et évaluation sur le test set
boosted_trees_predictions <- predict(boosted_trees_final_model, test_data) %>%
  bind_cols(test_data)

boosted_trees_metrics <- boosted_trees_predictions %>%
  metrics(truth = life_length, estimate = .pred)

print(boosted_trees_metrics)

explainer <- DALEX::explain(
  model = boosted_trees_final_model,
  predict_function = predict,
  data = train_data,
  y = train_data$life_length,
  predict_function_target_column="life_length",
  type="regression"
)

explainer_boosted_trees_shap <- explainer |> 
  predict_parts(train_data[7, ], keep_distributions = FALSE) |> 
  shapviz()

sv_waterfall(explainer_boosted_trees_shap)
sv_force(explainer_boosted_trees_shap)
sv_importance(explainer_boosted_trees_shap)
sv_dependence(explainer_boosted_trees_shap,v = "life_length")


# 2. Modèle Decision Tree

# Définir le modèle Decision Tree
decision_tree_model <- decision_tree(
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("regression")

# Créer le workflow
decision_tree_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(decision_tree_model)

# Validation croisée
cv_folds <- vfold_cv(train_data, v = 4)

# Entraîner et évaluer le modèle
decision_tree_fit <- fit_resamples(
  decision_tree_workflow,
  resamples = cv_folds,
  metrics = metric_set(rmse)
)

# Sélectionner le meilleur modèle
best_params <- select_best(decision_tree_fit, metric = "rmse")

# Finaliser et ajuster le modèle final
final_decision_tree <- finalize_workflow(decision_tree_workflow, best_params)
decision_tree_final_model <- fit(final_decision_tree, data = train_data)

# Prédictions et évaluation sur le test set
decision_tree_predictions <- predict(decision_tree_final_model, test_data) %>%
  bind_cols(test_data)

decision_tree_metrics <- decision_tree_predictions %>%
  metrics(truth = life_length, estimate = .pred)

print(decision_tree_metrics)


decision_tree_explainer <- DALEX::explain(
  model = decision_tree_final_model,
  predict_function = predict,
  data = train_data,
  y = train_data$life_length,
  predict_function_target_column="life_length",
  type="regression"
)

decision_tree_explainer2 <- decision_tree_explainer |> 
  predict_parts(train_data[7, ], keep_distributions = FALSE) |> 
  shapviz()

sv_waterfall(decision_tree_explainer2)
sv_force(decision_tree_explainer2)
sv_importance(decision_tree_explainer2)
sv_dependence(decision_tree_explainer2,v = "life_length")


# 3. Modèle Gradient Boosted

# Définir le modèle Gradient Boosted
gradient_boosted_model <- boost_tree(
  trees = tune(),
  tree_depth = 6,
  min_n = 2,
  loss_reduction = 0.01,
  sample_size = 0.8,
  mtry = 2,
  learn_rate = 0.01
) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")

# Créer le workflow
gradient_boosted_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(gradient_boosted_model)

# Validation croisée
cv_folds <- vfold_cv(train_data, v = 4)

# Entraîner et évaluer le modèle
gradient_boosted_fit <- fit_resamples(
  gradient_boosted_workflow,
  resamples = cv_folds,
  metrics = metric_set(rmse)
)

# Sélectionner le meilleur modèle
best_params <- select_best(gradient_boosted_fit, metric="rmse")

# Finaliser et ajuster le modèle final
final_gradient_boosted <- finalize_workflow(gradient_boosted_workflow, best_params)
gradient_boosted_final_model <- fit(final_gradient_boosted, data = train_data)

# Prédictions et évaluation sur le test set
gradient_boosted_predictions <- predict(gradient_boosted_final_model, test_data) %>%
  bind_cols(test_data)

gradient_boosted_metrics <- gradient_boosted_predictions %>%
  metrics(truth = life_length, estimate = .pred)

print(gradient_boosted_metrics)


gradient_boosted_explainer <- DALEX::explain(
  model = gradient_boosted_final_model,
  predict_function = predict,
  data = train_data,
  y = train_data$life_length,
  predict_function_target_column="life_length",
  type="regression"
)

gradient_boosted_explainer2 <- gradient_boosted_explainer |> 
  predict_parts(train_data[7, ], keep_distributions = FALSE) |> 
  shapviz()

sv_waterfall(gradient_boosted_explainer2)
sv_force(gradient_boosted_explainer2)
sv_importance(gradient_boosted_explainer2)
sv_dependence(gradient_boosted_explainer2,v = "life_length")


# 4. Modèle Xtreme Gradient Boosting (XGBoost)

# Définir le modèle XGBoost
xgboost_model <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = 0.01,
  sample_size = 0.8,
  mtry = tune(),
  learn_rate = 0.01
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# Créer le workflow
xgboost_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(xgboost_model)

# Validation croisée
cv_folds <- vfold_cv(train_data, v = 4)

# Entraîner et évaluer le modèle
xgboost_fit <- fit_resamples(
  xgboost_workflow,
  resamples = cv_folds,
  metrics = metric_set(rmse)
)

# Sélectionner le meilleur modèle
best_params <- select_best(xgboost_fit, metric="rmse")

# Finaliser et ajuster le modèle final
final_xgboost <- finalize_workflow(xgboost_workflow, best_params)
xgboost_final_model <- fit(final_xgboost, data = train_data)

# Prédictions et évaluation sur le test set
xgboost_predictions <- predict(xgboost_final_model, test_data) %>%
  bind_cols(test_data)

xgboost_metrics <- xgboost_predictions %>%
  metrics(truth = life_length, estimate = .pred)

print(xgboost_metrics)



xgboost_explainer <- DALEX::explain(
  model = xgboost_final_model,
  predict_function = predict,
  data = train_data,
  y = train_data$life_length,
  predict_function_target_column="life_length",
  type="regression"
)

xgboost_explainer2 <- xgboost_explainer |> 
  predict_parts(train_data[7, ], keep_distributions = FALSE) |> 
  shapviz()

sv_waterfall(xgboost_explainer2)
sv_force(xgboost_explainer2)
sv_importance(xgboost_explainer2)
sv_dependence(xgboost_explainer2,v = "life_length")


# 5. Modèle Boosting MARS
# Définir le modèle MARS
mars_model <- mars(
  num_terms = tune(),
  prod_degree = tune()
) %>%
  set_engine("earth") %>%
  set_mode("regression")

# Créer le workflow
mars_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(mars_model)

# Définir les hyperparamètres
num_terms_param <- num_terms(range = c(2, 20))
prod_degree_param <- prod_degree(range = c(1, 3))

# Créer la grille de recherche des hyperparamètres
mars_grid <- grid_regular(num_terms_param, prod_degree_param, levels = 5)

# Validation croisée
cv_folds <- vfold_cv(train_data, v = 4)

# Entraîner et évaluer le modèle
mars_fit <- tune_grid(
  mars_workflow,
  resamples = cv_folds,
  grid = mars_grid,
  metrics = metric_set(rmse)
)

# Sélectionner le meilleur modèle
best_params <- select_best(mars_fit, metric="rmse")

# Finaliser et ajuster le modèle final
final_mars <- finalize_workflow(mars_workflow, best_params)
mars_final_model <- fit(final_mars, data = train_data)

# Prédictions et évaluation sur le test set
mars_predictions <- predict(mars_final_model, test_data) %>%
  bind_cols(test_data)

mars_metrics <- mars_predictions %>%
  metrics(truth = life_length, estimate = .pred)

print(mars_metrics)


mars_explainer <- DALEX::explain(
  model = mars_final_model,
  predict_function = predict,
  data = train_data,
  y = train_data$life_length,
  predict_function_target_column="life_length",
  type="regression"
)

mars_explainer2 <- mars_explainer |> 
  predict_parts(train_data[7, ], keep_distributions = FALSE) |> 
  shapviz()

sv_waterfall(mars_explainer2)
sv_force(mars_explainer2)
sv_importance(mars_explainer2)
sv_dependence(mars_explainer2,v = "life_length")


# 6. Modèle MLP (Multi-Layer Perceptron)
# Définir le modèle MLP
mlp_model <- mlp(
  hidden_units = tune(),
  penalty = tune(),
  epochs = tune()
) %>%
  set_engine("nnet") %>%
  set_mode("regression")

# Créer le workflow
mlp_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(mlp_model)

# Définir la grille de recherche des hyperparamètres
mlp_grid <- grid_regular(hidden_units(), penalty(), epochs(), levels = 5)

# Validation croisée
cv_folds <- vfold_cv(train_data, v = 4)

# Entraîner et évaluer le modèle
mlp_fit <- tune_grid(
  mlp_workflow,
  resamples = cv_folds,
  grid = mlp_grid,
  metrics = metric_set(rmse)
)

# Sélectionner le meilleur modèle
best_params <- select_best(mlp_fit, metric="rmse")

# Finaliser et ajuster le modèle final
final_mlp <- finalize_workflow(mlp_workflow, best_params)
mlp_final_model <- fit(final_mlp, data = train_data)

# Prédictions et évaluation sur le test set
mlp_predictions <- predict(mlp_final_model, test_data) %>%
  bind_cols(test_data)

mlp_metrics <- mlp_predictions %>%
  metrics(truth = life_length, estimate = .pred)

print(mlp_metrics)

mlp_explainer <- DALEX::explain(
  model = mlp_final_model,
  predict_function = predict,
  data = train_data,
  y = train_data$life_length,
  predict_function_target_column="life_length",
  type="regression"
)

mlp_explainer2 <- mlp_explainer |> 
  predict_parts(train_data[7, ], keep_distributions = FALSE) |> 
  shapviz()

sv_waterfall(mlp_explainer2)
sv_force(mlp_explainer2)
sv_importance(mlp_explainer2)
sv_dependence(mlp_explainer2,v = "life_length")

# 7. Modèle KNN
# Définir le modèle KNN
knn_model <- nearest_neighbor(
  neighbors = tune()
) %>%
  set_engine("kknn") %>%
  set_mode("regression")

# Créer le workflow
knn_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(knn_model)

# Définir la grille de recherche des hyperparamètres
knn_grid <- grid_regular(neighbors(), levels = 5)

# Validation croisée
cv_folds <- vfold_cv(train_data, v = 4)

# Entraîner et évaluer le modèle
knn_fit <- tune_grid(
  knn_workflow,
  resamples = cv_folds,
  grid = knn_grid,
  metrics = metric_set(rmse)
)

# Sélectionner le meilleur modèle
best_params <- select_best(knn_fit,metric= "rmse")

# Finaliser et ajuster le modèle final
final_knn <- finalize_workflow(knn_workflow, best_params)
knn_final_model <- fit(final_knn, data = train_data)

# Prédictions et évaluation sur le test set
knn_predictions <- predict(knn_final_model, test_data) %>%
  bind_cols(test_data)

knn_metrics <- knn_predictions %>%
  metrics(truth = life_length, estimate = .pred)

print(knn_metrics)


knn_explainer <- DALEX::explain(
  model = knn_final_model,
  predict_function = predict,
  data = train_data,
  y = train_data$life_length,
  predict_function_target_column="life_length",
  type="regression"
)

knn_explainer2 <- knn_explainer |> 
  predict_parts(train_data[7, ], keep_distributions = FALSE) |> 
  shapviz()

sv_waterfall(knn_explainer2)
sv_force(knn_explainer2)
sv_importance(knn_explainer2)
sv_dependence(knn_explainer2,v = "life_length")

# 8. Modèle Random Forest

# Définir le modèle Random Forest
random_forest_model <- rand_forest(
  trees = tune(),
  min_n = tune(),
  mtry = tune()
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# Créer le workflow
random_forest_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(random_forest_model)

# Finaliser le paramètre mtry en fonction du nombre total de prédicteurs
mtry_param <- finalize(mtry(), train_data %>% select(-life_length))

# Définir la grille de recherche des hyperparamètres
random_forest_grid <- grid_regular(
  trees(),
  min_n(),
  mtry_param,
  levels = 5
)

# Validation croisée
cv_folds <- vfold_cv(train_data, v = 4)

# Entraîner et évaluer le modèle
random_forest_fit <- tune_grid(
  random_forest_workflow,
  resamples = cv_folds,
  grid = random_forest_grid,
  metrics = metric_set(rmse)
)

# Sélectionner le meilleur modèle
best_params <- select_best(random_forest_fit, metric = "rmse")

# Finaliser et ajuster le modèle final
final_random_forest <- finalize_workflow(random_forest_workflow, best_params)
random_forest_final_model <- fit(final_random_forest, data = train_data)

# Prédictions et évaluation sur le test set
random_forest_predictions <- predict(random_forest_final_model, test_data) %>%
  bind_cols(test_data)

random_forest_metrics <- random_forest_predictions %>%
  metrics(truth = life_length, estimate = .pred)

print(random_forest_metrics)

random_forest_explainer <- DALEX::explain(
  model = random_forest_final_model,
  predict_function = predict,
  data = train_data,
  y = train_data$life_length,
  predict_function_target_column="life_length",
  type="regression"
)

random_forest_explainer2 <- random_forest_explainer |> 
  predict_parts(train_data[7, ], keep_distributions = FALSE) |> 
  shapviz()

sv_waterfall(random_forest_explainer2)
sv_force(random_forest_explainer2)
sv_importance(random_forest_explainer2)
sv_dependence(random_forest_explainer2,v = "life_length")


# 9. Modèle SVM (Polynomial Kernel)

# Définir le modèle SVM avec noyau polynomial
svm_poly_model <- svm_poly(
  cost = tune(),
  degree = tune()
) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

# Créer le workflow
svm_poly_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(svm_poly_model)

# Définir la grille de recherche des hyperparamètres
svm_poly_grid <- grid_regular(cost(), degree(), levels = 5)

# Validation croisée
cv_folds <- vfold_cv(train_data, v = 4)

# Entraîner et évaluer le modèle
svm_poly_fit <- tune_grid(
  svm_poly_workflow,
  resamples = cv_folds,
  grid = svm_poly_grid,
  metrics = metric_set(rmse)
)

# Sélectionner le meilleur modèle
best_params <- select_best(svm_poly_fit,metric= "rmse")

# Finaliser et ajuster le modèle final
final_svm_poly <- finalize_workflow(svm_poly_workflow, best_params)
svm_poly_final_model <- fit(final_svm_poly, data = train_data)

# Prédictions et évaluation sur le test set
svm_poly_predictions <- predict(svm_poly_final_model, test_data) %>%
  bind_cols(test_data)

svm_poly_metrics <- svm_poly_predictions %>%
  metrics(truth = life_length, estimate = .pred)

print(svm_poly_metrics)



svm_poly_explainer <- DALEX::explain(
  model = svm_poly_final_model,
  predict_function = predict,
  data = train_data,
  y = train_data$life_length,
  predict_function_target_column="life_length",
  type="regression"
)

svm_poly_explainer2 <- svm_poly_explainer |> 
  predict_parts(train_data[7, ], keep_distributions = FALSE) |> 
  shapviz()

sv_waterfall(svm_poly_explainer2)
sv_force(svm_poly_explainer2)
sv_importance(svm_poly_explainer2)
sv_dependence(svm_poly_explainer2,v = "life_length")




# 10. Modèle SVM (Radial Basis Function)
# Définir le modèle SVM avec noyau RBF
svm_rbf_model <- svm_rbf(
  cost = tune(),
  rbf_sigma = tune()
) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

# Créer le workflow
svm_rbf_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(svm_rbf_model)

# Définir la grille de recherche des hyperparamètres
svm_rbf_grid <- grid_regular(cost(), rbf_sigma(), levels = 5)

# Validation croisée
cv_folds <- vfold_cv(train_data, v = 4)

# Entraîner et évaluer le modèle
svm_rbf_fit <- tune_grid(
  svm_rbf_workflow,
  resamples = cv_folds,
  grid = svm_rbf_grid,
  metrics = metric_set(rmse)
)

# Sélectionner le meilleur modèle
best_params <- select_best(svm_rbf_fit,metric= "rmse")

# Finaliser et ajuster le modèle final
final_svm_rbf <- finalize_workflow(svm_rbf_workflow, best_params)
svm_rbf_final_model <- fit(final_svm_rbf, data = train_data)

# Prédictions et évaluation sur le test set
svm_rbf_predictions <- predict(svm_rbf_final_model, test_data) %>%
  bind_cols(test_data)

svm_rbf_metrics <- svm_rbf_predictions %>%
  metrics(truth = life_length, estimate = .pred)

print(svm_rbf_metrics)

# 10. Modèle SVM (Radial Basis Function)
# Définir le modèle SVM avec noyau RBF
svm_rbf_model <- svm_rbf(
  cost = tune(),
  rbf_sigma = tune()
) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

# Créer le workflow
svm_rbf_workflow <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(svm_rbf_model)

# Définir la grille de recherche des hyperparamètres
svm_rbf_grid <- grid_regular(cost(), rbf_sigma(), levels = 5)

# Validation croisée
cv_folds <- vfold_cv(train_data, v = 4)

# Entraîner et évaluer le modèle
svm_rbf_fit <- tune_grid(
  svm_rbf_workflow,
  resamples = cv_folds,
  grid = svm_rbf_grid,
  metrics = metric_set(rmse)
)

# Sélectionner le meilleur modèle
best_params <- select_best(svm_rbf_fit, metric = "rmse")

# Finaliser et ajuster le modèle final
final_svm_rbf <- finalize_workflow(svm_rbf_workflow, best_params)
svm_rbf_final_model <- fit(final_svm_rbf, data = train_data)

# Prédictions et évaluation sur le test set
svm_rbf_predictions <- predict(svm_rbf_final_model, test_data) %>%
  bind_cols(test_data)

svm_rbf_metrics <- svm_rbf_predictions %>%
  metrics(truth = life_length, estimate = .pred)

print(svm_rbf_metrics)


svm_rbf_explainer <- DALEX::explain(
  model = svm_rbf_final_model,
  predict_function = predict,
  data = train_data,
  y = train_data$life_length,
  predict_function_target_column="life_length",
  type="regression"
)

svm_rbf_explainer2 <- svm_rbf_explainer |> 
  predict_parts(train_data[7, ], keep_distributions = FALSE) |> 
  shapviz()

sv_waterfall(svm_rbf_explainer2)
sv_force(svm_rbf_explainer2)
sv_importance(svm_rbf_explainer2)
sv_dependence(svm_rbf_explainer2,v = "life_length")

sv_importance(svm_rbf_explainer2, kind = "beeswarm") 
sv_dependence(svm_rbf_explainer2, v = x)

sv_interaction(svm_rbf_explainer2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

shp_i <- shapviz(
  svm_rbf_final_model, X_pred = data.matrix(train_data[x]), X = train_data, interactions = TRUE
)

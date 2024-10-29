# Installer et charger les packages nécessaires
library(tidymodels)
library(lightgbm)
library(bonsai)
library(tidyverse)
library(parsnip)
library(treesnip)
library(catboost)
library(fastshap)
library(DALEXtra)
# Charger l'ensemble de données iris
data(iris)

# Diviser les données en ensemble d'entraînement et de test
set.seed(123)
data_split <- initial_split(iris, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

# Prétraitement des données
iris_recipe <- recipe(Species ~ ., data = train_data) %>%
  step_normalize(all_predictors()) %>%
  step_dummy(all_nominal_predictors())



## Xtreme Gradient Boosting (XGBoost)

xgboost_model <- boost_tree(
  trees = 1000,
  tree_depth = 6,
  min_n = 2,
  loss_reduction = 0.01,
  sample_size = 0.8,
  mtry = 2,
  learn_rate = 0.01
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

xgboost_workflow <- workflow() %>%
  add_recipe(iris_recipe) %>%
  add_model(xgboost_model)

xgboost_fit <- xgboost_workflow %>%
  fit(data = train_data)

xgboost_predictions <- xgboost_fit %>%
  predict(test_data) %>%
  bind_cols(test_data)

xgboost_metrics <- xgboost_predictions %>%
  metrics(truth = Species, estimate = .pred_class)

print(xgboost_metrics)

############

fit <- lm(Sepal.Length ~ . + Species:Petal.Width, data = iris)
train_data2 <- train_data%>%
  select(-Species)


test_data2 <- test_data%>%
  select(-Species)

shap <- fastshap::explain(xgboost_fit, X = train_data, nsim = 100, 
  pred_wrapper = predict,new_data=train_data,shap_only = FALSE)

sv <- shapviz(shap)
sv_dependence(sv, "Species")




# SHAP analysis: X can even contain factors
dia_2000 <-test_data[,-5]
shp <- shapviz(xgboost_fit, X_pred = data.matrix(dia_2000), X = dia_2000)

sv_importance(shp, show_numbers = TRUE)


# Préparer les données pour SHAP
test_data_prepared <- bake(prep(iris_recipe), new_data = test_data)

# Extraire les caractéristiques sans la colonne cible
X_test <- as.matrix(test_data_prepared %>% select(-Species))

# Obtenir les prédictions du modèle pour les données de test
X_pred <- predict(xgboost_fit, test_data, type = "prob") %>% pull(.pred_class)

X_pred <- as.matrix(X_pred)
# Calculer les valeurs SHAP
shap_values <- shapviz(xgboost_obj, X = X_test, X_pred = X_pred)




## Gradient Boosted (LightGBM)

gradient_boosted_model <- boost_tree(
  trees = 1000,
  tree_depth = 6,
  min_n = 2,
  loss_reduction = 0.01,
  sample_size = 0.8,
  mtry = 2,
  learn_rate = 0.01
) %>%
  set_engine("lightgbm") %>%
  set_mode("classification")

gradient_boosted_workflow <- workflow() %>%
  add_recipe(iris_recipe) %>%
  add_model(gradient_boosted_model)

gradient_boosted_fit <- gradient_boosted_workflow %>%
  fit(data = train_data)

gradient_boosted_predictions <- gradient_boosted_fit %>%
  predict(test_data) %>%
  bind_cols(test_data)

gradient_boosted_metrics <- gradient_boosted_predictions %>%
  metrics(truth = Species, estimate = .pred_class)

print(gradient_boosted_metrics)

## Modèle Boosted Trees
install.packages("catboost")
library(catboost)

boosted_trees_model <- boost_tree(
  trees = 1000,
  tree_depth = 6,
  min_n = 2,
  loss_reduction = 0.01,
  sample_size = 0.8,
  mtry = 0.5,
  learn_rate = 0.01
) %>%
  set_engine("catboost") %>%
  set_mode("classification")

boosted_trees_workflow <- workflow() %>%
  add_recipe(iris_recipe) %>%
  add_model(boosted_trees_model)

boosted_trees_fit <- boosted_trees_workflow %>%
  fit(data = train_data)

boosted_trees_predictions <- boosted_trees_fit %>%
  predict(test_data) %>%
  bind_cols(test_data)

boosted_trees_metrics <- boosted_trees_predictions %>%
  metrics(truth = Species, estimate = .pred_class)

print(boosted_trees_metrics)


##  Modèle Decision Tree

decision_tree_model <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

decision_tree_workflow <- workflow() %>%
  add_recipe(iris_recipe) %>%
  add_model(decision_tree_model)

decision_tree_fit <- decision_tree_workflow %>%
  fit(data = train_data)

decision_tree_predictions <- decision_tree_fit %>%
  predict(test_data) %>%
  bind_cols(test_data)

decision_tree_metrics <- decision_tree_predictions %>%
  metrics(truth = Species, estimate = .pred_class)

print(decision_tree_metrics)


## Modèle Gradient Boosted

gradient_boosted_model <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("classification")

gradient_boosted_workflow <- workflow() %>%
  add_recipe(iris_recipe) %>%
  add_model(gradient_boosted_model)

gradient_boosted_fit <- gradient_boosted_workflow %>%
  fit(data = train_data)

gradient_boosted_predictions <- gradient_boosted_fit %>%
  predict(test_data) %>%
  bind_cols(test_data)

gradient_boosted_metrics <- gradient_boosted_predictions %>%
  metrics(truth = Species, estimate = .pred_class)

print(gradient_boosted_metrics)


## Modèle MARS

mars_model <- mars() %>%
  set_engine("earth") %>%
  set_mode("classification")

mars_workflow <- workflow() %>%
  add_recipe(iris_recipe) %>%
  add_model(mars_model)

mars_fit <- mars_workflow %>%
  fit(data = train_data)

mars_predictions <- mars_fit %>%
  predict(test_data) %>%
  bind_cols(test_data)

mars_metrics <- mars_predictions %>%
  metrics(truth = Species, estimate = .pred_class)

print(mars_metrics)

## Modèle MLP (Multi-Layer Perceptron)

mlp_model <- mlp(hidden_units = 10, penalty = 0.1, 
                 epochs = 200,learn_rate=.1) %>%
  set_engine("keras", trace = FALSE) %>%
  set_mode("classification")


mlp_workflow <- workflow() %>%
  add_recipe(iris_recipe) %>%
  add_model(mlp_model)

mlp_fit <- mlp_workflow %>%
  fit(data = train_data)

mlp_predictions <- mlp_fit %>%
  predict(test_data) %>%
  bind_cols(test_data)

mlp_metrics <- mlp_predictions %>%
  metrics(truth = Species, estimate = .pred_class)

print(mlp_metrics)


## Modèle KNN

knn_model <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("classification")

knn_workflow <- workflow() %>%
  add_recipe(iris_recipe) %>%
  add_model(knn_model)

knn_fit <- knn_workflow %>%
  fit(data = train_data)

knn_predictions <- knn_fit %>%
  predict(test_data) %>%
  bind_cols(test_data)

knn_metrics <- knn_predictions %>%
  metrics(truth = Species, estimate = .pred_class)

print(knn_metrics)


## Modèle Random Forest

random_forest_model <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("classification")

random_forest_workflow <- workflow() %>%
  add_recipe(iris_recipe) %>%
  add_model(random_forest_model)

random_forest_fit <- random_forest_workflow %>%
  fit(data = train_data)

random_forest_predictions <- random_forest_fit %>%
  predict(test_data) %>%
  bind_cols(test_data)

random_forest_metrics <- random_forest_predictions %>%
  metrics(truth = Species, estimate = .pred_class)

print(random_forest_metrics)


## Modèle SVM (Polynomial Kernel)

svm_poly_model <- svm_poly() %>%
  set_engine("kernlab") %>%
  set_mode("classification")

svm_poly_workflow <- workflow() %>%
  add_recipe(iris_recipe) %>%
  add_model(svm_poly_model)

svm_poly_fit <- svm_poly_workflow %>%
  fit(data = train_data)

svm_poly_predictions <- svm_poly_fit %>%
  predict(test_data) %>%
  bind_cols(test_data)

svm_poly_metrics <- svm_poly_predictions %>%
  metrics(truth = Species, estimate = .pred_class)

print(svm_poly_metrics)


## Modèle SVM (Radial Basis Function)
svm_rbf_model <- svm_rbf() %>%
  set_engine("kernlab") %>%
  set_mode("classification")

svm_rbf_workflow <- workflow() %>%
  add_recipe(iris_recipe) %>%
  add_model(svm_rbf_model)

svm_rbf_fit <- svm_rbf_workflow %>%
  fit(data = train_data)

svm_rbf_predictions <- svm_rbf_fit %>%
  predict(test_data) %>%
  bind_cols(test_data)

svm_rbf_metrics <- svm_rbf_predictions %>%
  metrics(truth = Species, estimate = .pred_class)

print(svm_rbf_metrics)

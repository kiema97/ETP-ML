## Nettoyage
rm(list = ls())

# Charger les bibliothèques nécessaires
library(tidymodels)
library(readxl)
library(dplyr)
library(timetk)
library(rsample)
library(parsnip)
library(recipes)
library(workflows)
library(tune)
library(yardstick)
library(corrplot)
library(randomForest)
library(xgboost)

setwd("G:/PROJET/Article/ET_ML")
# Définir le chemin du fichier
file_path <- "data/cli_data_bf.xlsx"

# Charger les données de chaque feuille
tx_data <- read_excel(file_path, sheet = "tx")%>%
  gather(key = "stations",value ="tx" ,-Date)

tn_data <- read_excel(file_path, sheet = "tn")%>%
  gather(key = "stations",value ="tn" ,-Date)

rh_data <- read_excel(file_path, sheet = "rh")%>%
  gather(key = "stations",value ="rh" ,-Date)

rs_data <- read_excel(file_path, sheet = "rs")%>%
  gather(key = "stations",value ="rs" ,-Date)

ws_data <- read_excel(file_path, sheet = "ws")%>%
  gather(key = "stations",value ="ws" ,-Date)

et0_data <- read_excel(file_path, sheet = "et0")%>%
  gather(key = "stations",value ="et0" ,-Date)


# Fusionner toutes les données en une seule base

merged_data <- tx_data %>%
  inner_join(tn_data, by = c("Date","stations")) %>%
  inner_join(rh_data, by = c("Date","stations")) %>%
  inner_join(rs_data , by = c("Date","stations")) %>%
  inner_join(ws_data , by = c("Date","stations")) %>%
  inner_join(et0_data , by = c("Date","stations"))%>%
  filter(stations=="bobo")

# Diviser les données en ensembles d'entraînement et de test en respectant la chronologie
train_size <- floor(0.8 * nrow(merged_data))
train_data <- merged_data[1:train_size, ]
test_data <- merged_data[(train_size + 1):nrow(merged_data), ]

# Créer des plis de validation croisée par blocs
set.seed(123)
cv_splits <- time_series_cv(
  data = train_data,
  initial = train_size / 2,
  assess = train_size / 4,
  skip = train_size / 8,
  cumulative = TRUE
)

# Préparation des recettes de pré-traitement
recipe_et0 <- recipe(et0_bobo ~ ., data = train_data) %>%
  step_normalize(all_predictors())

# Fonction pour ajuster et évaluer un modèle avec validation croisée
fit_and_evaluate_cv <- function(model_spec, cv_splits, recipe) {
  # Définir le workflow
  wf <- workflow() %>%
    add_model(model_spec) %>%
    add_recipe(recipe)
  
  # Ajuster le modèle avec validation croisée
  res <- tune_grid(
    wf,
    resamples = cv_splits,
    grid = 10,
    control = control_grid(save_pred = TRUE)
  )
  
  # Collecter les métriques
  metrics <- res %>% collect_metrics()
  
  return(list(tuned_results = res, metrics = metrics))
}

# Définir et ajuster les modèles avec validation croisée

# Boosted Trees
boosted_tree_spec <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("regression")

boosted_tree_results <- fit_and_evaluate_cv(boosted_tree_spec, cv_splits, recipe_et0)

# Decision Tree
decision_tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("regression")

decision_tree_results <- fit_and_evaluate_cv(decision_tree_spec, cv_splits, recipe_et0)

# Gradient Boosted
gradient_boosted_spec <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("regression")

gradient_boosted_results <- fit_and_evaluate_cv(gradient_boosted_spec, cv_splits, recipe_et0)

# Extreme Gradient Boosting
xgboost_spec <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgboost_results <- fit_and_evaluate_cv(xgboost_spec, cv_splits, recipe_et0)

# MARS
mars_spec <- mars() %>%
  set_engine("earth") %>%
  set_mode("regression")

mars_results <- fit_and_evaluate_cv(mars_spec, cv_splits, recipe_et0)

# MLP
mlp_spec <- mlp() %>%
  set_engine("nnet") %>%
  set_mode("regression")

mlp_results <- fit_and_evaluate_cv(mlp_spec, cv_splits, recipe_et0)

# KNN
knn_spec <- nearest_neighbor() %>%
  set_engine("kknn") %>%
  set_mode("regression")

knn_results <- fit_and_evaluate_cv(knn_spec, cv_splits, recipe_et0)

# Random Forest
random_forest_spec <- rand_forest() %>%
  set_engine("randomForest") %>%
  set_mode("regression")

random_forest_results <- fit_and_evaluate_cv(random_forest_spec, cv_splits, recipe_et0)

# SVM (Polynomial)
svm_poly_spec <- svm_poly() %>%
  set_engine("kernlab") %>%
  set_mode("regression")

svm_poly_results <- fit_and_evaluate_cv(svm_poly_spec, cv_splits, recipe_et0)

# SVM (Radial Basis)
svm_rbf_spec <- svm_rbf() %>%
  set_engine("kernlab") %>%
  set_mode("regression")

svm_rbf_results <- fit_and_evaluate_cv(svm_rbf_spec, cv_splits, recipe_et0)

# Afficher les résultats des métriques
results <- list(
  Boosted_Trees = boosted_tree_results$metrics,
  Decision_Tree = decision_tree_results$metrics,
  Gradient_Boosted = gradient_boosted_results$metrics,
  XGBoost = xgboost_results$metrics,
  MARS = mars_results$metrics,
  MLP = mlp_results$metrics,
  KNN = knn_results$metrics,
  Random_Forest = random_forest_results$metrics,
  SVM_Polynomial = svm_poly_results$metrics,
  SVM_Radial = svm_rbf_results$metrics
)

results_df <- bind_rows(results, .id = "Model")
print(results_df)

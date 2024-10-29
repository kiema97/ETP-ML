# Installer le package elmNNRcpp si nécessaire
if (!requireNamespace("elmNNRcpp", quietly = TRUE)) {
  install.packages("elmNNRcpp")
}


library(elmNNRcpp)
library(caret)
library(tidyverse)
library(purrr)
library(tidymodels)

# Import Data
stations_data <- read.csv(file = "stations_clim_data.csv") %>%
  mutate(across(tx:ws, ~scale(.x)))
names(stations_data)

train_elm_model <- function(data, station_name) {
  # Filtrer les données pour la station spécifique
  station_data <- data %>% filter(stations == station_name) %>% select(-Date, -stations)
  
  # Diviser les données en ensembles d'entraînement et de test
  set.seed(123)
  data_split <- initial_split(station_data, prop = 0.8)
  train_data <- training(data_split)
  test_data <- testing(data_split)
  
  # Préparer les matrices pour elmNNRcpp
  xtr <- as.matrix(train_data %>% select(-et0))
  ytr <- as.matrix(train_data$et0)
  xte <- as.matrix(test_data %>% select(-et0))
  yte <- as.matrix(test_data$et0)
  
  # Définir les hyperparamètres pour la recherche sur grille
  nhid_values <- seq(5,50,5)
  actfun_values <- c("relu", "sig","tansig","radbas","hardlim","hardlims","satlins","tribas","purelin")
  
  # Initialiser les meilleures métriques
  best_rmse <- Inf
  best_model <- NULL
  best_params <- list()
  
  # Configuration de la validation croisée
  cv_splits <- vfold_cv(train_data, v = 5)
  df_cv_results=rbind()
  # Recherche sur grille
  for (nhid in nhid_values) {
    for (actfun in actfun_values) {
      # Effectuer la validation croisée
      cv_results <- map(cv_splits$splits, function(split) {
        train_split <- analysis(split)
        test_split <- assessment(split)
        
        # Préparer les matrices pour elmNNRcpp
        xtr_cv <- as.matrix(train_split %>% select(-et0))
        ytr_cv <- as.matrix(train_split$et0)
        xte_cv <- as.matrix(test_split %>% select(-et0))
        yte_cv <- as.matrix(test_split$et0)
        
        # Entraîner le modèle ELM
        fit_elm <- elm_train(xtr_cv, ytr_cv, nhid = nhid, actfun = actfun,
                             init_weights = "uniform_negative", bias = TRUE, verbose = FALSE)
        
        # Faire des prédictions sur l'ensemble de test
        predictions <- elm_predict(fit_elm, xte_cv)
        
        # Calculer le RMSE
        rmse_value <- sqrt(mean((yte_cv - predictions)^2))
        return(rmse_value)
      })
      df_cv_result=data.frame(actfuns=actfun,
                              nhids=nhid,
                              cv1=unlist(cv_results)[1],
                              cv2=unlist(cv_results)[2],
                              cv3=unlist(cv_results)[3],
                              cv4=unlist(cv_results)[4],
                              cv5=unlist(cv_results)[5])
      
      df_cv_results=rbind(df_cv_results,df_cv_result)
      # Calculer le RMSE moyen pour cette combinaison d'hyperparamètres
      mean_rmse <- mean(unlist(cv_results))
      
      # Si le RMSE est meilleur, sauvegarder le modèle et les paramètres
      if (mean_rmse < best_rmse) {
        best_rmse <- mean_rmse
        best_model <- elm_train(xtr, ytr, nhid = nhid, actfun = actfun,
                                init_weights = "uniform_negative", bias = TRUE, verbose = FALSE)
        best_params <- list(nhid = nhid, actfun = actfun)
      }
    }
  }
  
  # Évaluer les performances du meilleur modèle
  final_predictions <- elm_predict(best_model, xte)
  final_rmse <- sqrt(mean((yte - final_predictions)^2))
  final_rsq <- cor(yte, final_predictions)^2
  
  cat("Station:", station_name, "\n")
  cat("Best Parameters: nhid =", best_params$nhid, ", actfun =", best_params$actfun, "\n")
  cat("RMSE:", final_rmse, "\n")
  cat("R²:", final_rsq, "\n\n")
  
  return(list(model = best_model, rmse = final_rmse, rsq = final_rsq, params = best_params,cv_results=df_cv_results))
}

# Lancer l'entraînement pour chaque station
stations <- unique(stations_data$stations)
stations
results <- purrr::map(stations, ~train_elm_model(stations_data, .x))
# Afficher les résultats pour chaque station
names(results) <- stations
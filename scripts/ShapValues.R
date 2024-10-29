## Netoyage
rm(list = ls())
## Librairies
library(tidyverse)
library(tidymodels)
library(kernlab)
library(DALEX)
library(shapper)
#shapper::install_shap()
# Example dataset
#data(iris)
#data(ames, package = "modeldata")


setwd("G:/PROJET/Article/ET_ML")

# Chargement des données
ml_data <- read.csv(file = "data/stations_clim_data.csv")%>%
  dplyr::filter(stations=="bobo")%>%
  select(-Date,-stations)

# Division des données en ensembles d'entraînement et de test
set.seed(123)
data_split <- initial_split(ml_data, prop = 0.8)
train_data <- training(data_split)
test_data <- testing(data_split)

final_model <- readRDS("G:/PROJET/Article/ET_ML/outputs/BaggingTrees/bagging_model_bobo.rds")


individual_variable_effect(final_model, data = train_data[,-6], 
                           predict_function = predict,
                           new_observation = test_data[1:10,-6], 
                           nsamples = 50)




# Créer l'explainer
train_data2 <- train_data %>% select(-life_length)
explainer <- DALEX::explain(
  model = final_model,
  predict_function = predict,
  data = train_data,
  y = train_data$et0,
  predict_function_target_column="et0",
  type="regression"
)

library(shapviz)
bd <- explainer |> 
  predict_parts(test_data, keep_distributions = FALSE) |> 
  shapviz()

sv_waterfall(bd)
sv_force(bd)
sv_importance(bd)
sv_dependence(bd,v = "et0")
#sv_dependence2D(bd)
#sv_interaction(bd)
DALEX::variable_importance(explainer)

# Réduire le nombre d'échantillons de fond en utilisant shap.sample

shap_values <- shapper::shap(explainer,data=train_data,
                             new_observation = test_data,
                             predict_function = predict)



# Plot SHAP summary
plot(shap_values)

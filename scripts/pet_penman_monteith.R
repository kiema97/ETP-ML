setwd("G:/PROJET/Article/ET_ML")

# Étape 1 : Enregistrer le modèle, les modes et les arguments
library(tidymodels)
library(Evapotranspiration)
source("scripts/PM-FAO56.R")

# Step 1: Register the model, modes, and arguments
set_new_model("penman_monteith")
set_model_mode(model = "penman_monteith", mode = "regression")
set_model_engine(
  "penman_monteith", 
  mode = "regression", 
  eng = "custom_pet"
)
set_dependency("penman_monteith", eng = "custom_pet", pkg = "base")

# Show model info
show_model_info("penman_monteith")


## Étape 2 : Créer la fonction de modèle
penman_monteith <- function(mode = "regression") {
  if (mode != "regression") {
    rlang::abort("`mode` should be 'regression'")
  }
  args <- list()
  new_model_spec(
    "penman_monteith",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}
## Étape 3 : Ajouter un module de fit (ajustement)

custom_fit <- function(formula, data, lat, z) {
  # Extraire la variable réponse et les prédicteurs de la formule
  response <- all.vars(formula)[1]
  predictors <- all.vars(formula)[-1]
  
  # Préparer les données climatiques
  climate_data <- data[predictors]
  
  # Appeler la fonction PET.PM
  result <- PET.PM(climate_data, lat, z,albedo = 0.23, 
                   rs_rso_min = 0.33, rs_rso_max = 1, checkTdiff = F)
  
  return(result)
}

set_fit(
  model = "penman_monteith",
  eng = "custom_pet",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data", "lat", "z"),
    func = c(pkg = NULL, fun = "custom_fit"),
    defaults = list()
  )
)

show_model_info("penman_monteith")


## Étape 4 : Ajouter des modules pour la prédiction
custom_predict <- function(model, new_data) {
  predict(model, new_data)
}

set_pred(
  model = "penman_monteith",
  eng = "custom_pet",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(pkg = NULL, fun = "custom_predict"),
    args = list(object = quote(object), new_data = quote(new_data))
  )
)

show_model_info("penman_monteith")


##============================================================================##
# Charger les données
clim_data <- read.csv(file = "data/stations_clim_data.csv") %>%
  mutate(Date=lubridate::as_date(Date))

# Définir la latitude et l'altitude
lat <- 35.6895 # exemple de latitude (Tokyo)
z <- 44 # exemple d'altitude (Tokyo)

# Créer la recette
recipe_spec <- recipe(et0 ~ tx + tn + rh + rs + ws, data = clim_data) %>%
  step_normalize(all_predictors())

# Créer le workflow
workflow_spec <- workflow() %>%
  add_model(penman_monteith()) %>%
  add_recipe(recipe_spec)

# Séparer les données
set.seed(123)
split <- initial_split(clim_data, prop = 0.8)
train_data <- training(split)
test_data <- testing(split)

# Ajuster le workflow aux données d'entraînement
workflow_fit <- workflow_spec %>%
  fit(data = train_data, )

# Faire des prédictions sur les données de test
predictions <- predict(workflow_fit, new_data = test_data)

# Évaluer le modèle
metrics <- metric_set(rmse, rsq)
eval_results <- predictions %>%
  bind_cols(test_data) %>%
  metrics(truth = PET, estimate = .pred)

print(eval_results)


result <- PET.PM(clim_data, lat, z,albedo = 0.23, 
                 rs_rso_min = 0.33, rs_rso_max = 1, checkTdiff = F)

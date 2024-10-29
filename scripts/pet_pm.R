# Étapes de définition du modèle
## Charger les bibliothèques nécessaires
rm(list = ls())
library(tidymodels)
library(parsnip)

## 2. Définir et enregistrer le modèle, les modes et les arguments
# Définir un nouveau modèle appelé 'pet_pm'
set_new_model("pet_pm")

# Définir le mode du modèle comme 'regression'
set_model_mode(model = "pet_pm", mode = "regression")

# Définir le moteur du modèle comme 'PET.PM2'
set_model_engine(
  "pet_pm", 
  mode = "regression", 
  eng = "PET.PM2"
)

# Définir les dépendances du modèle
set_dependency("pet_pm", eng = "PET.PM2", pkg = "base")

# Montrer les informations du modèle
show_model_info("pet_pm")

## 3. Configurer les arguments du modèle

# Configurer les arguments du modèle
set_model_arg(
  model = "pet_pm",
  eng = "PET.PM2",
  parsnip = "cd",
  original = "cd",
  func = list(pkg = "base", fun = "PET.PM2"),
  has_submodel = FALSE
)

set_model_arg(
  model = "pet_pm",
  eng = "PET.PM2",
  parsnip = "lat",
  original = "lat",
  func = list(pkg = "base", fun = "PET.PM2"),
  has_submodel = FALSE
)

set_model_arg(
  model = "pet_pm",
  eng = "PET.PM2",
  parsnip = "z",
  original = "z",
  func = list(pkg = "base", fun = "PET.PM2"),
  has_submodel = FALSE
)

set_model_arg(
  model = "pet_pm",
  eng = "PET.PM2",
  parsnip = "albedo",
  original = "albedo",
  func = list(pkg = "base", fun = "PET.PM2"),
  has_submodel = FALSE
)

set_model_arg(
  model = "pet_pm",
  eng = "PET.PM2",
  parsnip = "rs_rso_min",
  original = "rs_rso_min",
  func = list(pkg = "base", fun = "PET.PM2"),
  has_submodel = FALSE
)

set_model_arg(
  model = "pet_pm",
  eng = "PET.PM2",
  parsnip = "rs_rso_max",
  original = "rs_rso_max",
  func = list(pkg = "base", fun = "PET.PM2"),
  has_submodel = FALSE
)

set_model_arg(
  model = "pet_pm",
  eng = "PET.PM2",
  parsnip = "checkTdiff",
  original = "checkTdiff",
  func = list(pkg = "base", fun = "PET.PM2"),
  has_submodel = FALSE
)

show_model_info("pet_pm")


## 4. Créer la fonction du modèle

pet_pm <- function(mode = "regression", cd = NULL, lat = NULL, z = NULL, albedo = 0.23, rs_rso_min = 0.33, rs_rso_max = 1, checkTdiff = FALSE) {
  # Vérifier le mode
  if (mode  != "regression") {
    rlang::abort("`mode` should be 'regression'")
  }
  
  # Capturer les arguments en quosures
  args <- list(cd = rlang::enquo(cd), lat = rlang::enquo(lat), z = rlang::enquo(z), albedo = rlang::enquo(albedo), rs_rso_min = rlang::enquo(rs_rso_min), rs_rso_max = rlang::enquo(rs_rso_max), checkTdiff = rlang::enquo(checkTdiff))
  
  # Sauvegarder des slots vides pour les parties futures de la spécification
  new_model_spec(
    "pet_pm",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = NULL
  )
}


## 5. Ajouter un module de fitting

set_fit(
  model = "pet_pm",
  eng = "PET.PM2",
  mode = "regression",
  value = list(
    interface = "formula",
    protect = c("formula", "data"),
    func = c(pkg = "base", fun = "PET.PM2"),
    defaults = list()
  )
)
show_model_info("pet_pm")

## 6. Configurer l'encodage des prédicteurs
set_encoding(
  model = "pet_pm",
  eng = "PET.PM2",
  mode = "regression",
  options = list(
    predictor_indicators = "none",
    compute_intercept = TRUE,
    remove_intercept = FALSE,
    allow_sparse_x = FALSE
  )
)

## 7. Ajouter des modules de prédiction
numeric_info <- list(
  pre = NULL,
  post = NULL,
  func = c(fun = "predict"),
  args = list(
    object = quote(object$fit),
    newdata = quote(new_data),
    type = "numeric"
  )
)

set_pred(
  model = "pet_pm",
  eng = "PET.PM2",
  mode = "regression",
  type = "numeric",
  value = numeric_info
)

show_model_info("pet_pm")


##============================================================================##
setwd("G:/PROJET/Article/ET_ML")
# Charger les données
clim_data <- read.csv(file = "data/stations_clim_data.csv") %>%
  mutate(Date=lubridate::as_date(Date))

# Définir la latitude et l'altitude
lat <- 35.6895 # exemple de latitude (Tokyo)
z <- 44 # exemple d'altitude (Tokyo)

# Définir un modèle 'pet_pm' en utilisant 'parsnip'
pet_model <- pet_pm(lat = lat, z = z) %>%
  set_engine("PET.PM2")

# Ajuster le modèle
fit_pet <- pet_model %>%
  fit(et0 ~ tx + tn + rh + rs + ws, data = clim_data)

# _targets.R file

suppressPackageStartupMessages({
  library(targets)
  library(tarchetypes)
  library(rio)
  library(here)
  library(tidyverse)
  library(missForest)
  library(lavaan)
  library(pROC)
  library(caret)
  library(randomForest)
  library(MVN)
  library(semTools)
  library(performance)
  library(datawizard)
  library(effectsize)
  library(ROSE)
  library(xgboost)
})


tar_source(
  here::here(
    "scripts", "redcap", "funs", "redcap_functions.R"
  )
)

tar_option_set(
  packages = c(
    "rio", "here", "tidyverse", "missForest"
  )
)

list(
  tar_target(
    file, 
    here::here("data", "raw", "redcap", "FAIv10_Cleaned.csv"), 
    format = "file"
  ),
  tar_target(
    fai_raw, 
    get_data(file)
  ),
  tar_target(
    fai_clean, 
    clean_data(fai_raw)
  ),
  tar_target(
    fai_clean_complete, 
    impute_data(fai_clean)
  )
  # tar_target(model, fit_model(data)),
  # tar_target(plot, plot_model(model, data))
)

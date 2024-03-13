# Script name: 002_read_data.R
# Project: FAI Meyer
# Script purpose: read and clean data.
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Thu Nov 17 06:10:31 2022
# Last Modified Date: Thu Nov 17 06:10:31 2022
# 
# Notes: I use the 000_name.R notation for the names of the new scripts.

library("here")
library("tidyverse")
library("readxl")
library("mice")
library("careless")

source(here::here("functions", "funs_flag_careless_responding.R"))
source(here::here("functions", "fai_funs.R"))


# Read the Excel data.
fai <- read_xlsx(here("data", "raw", "FAI_TOT_2020_corrected.xlsx"), col_names = TRUE)

# Demographic information.
demo_info <- fai[, 1:50]



# Items data.
items <- fai[, 51:247]

# Items only relevant for families with two or more children.
# Families with only one child have NAs for these items.
brothers_items <- c(
  188, 17, 184, 147, 109, 37, 171, 78, 126, 84, 55, 110, 123, 150, 158
)
# I need to remove these items before multiple imputation.

freq_nas <- items %>% 
  summarise_all(~sum(is.na(.))) 

data.frame(ii = 1:197, t(freq_nas))
# Too many NAs.
# too_many_nas <- c(
#   17, 37, 55, 78, 84, 109, 110, 123, 126, 147, 150, 158, 171, 184, 188
# )

# Remove columns with NAs (questions about siblings).

items_reduced <- items |> 
  dplyr::select(
    -c(
      FAI_17, FAI_37, FAI_55, FAI_78, FAI_84, FAI_109, FAI_110, FAI_123, 
      FAI_126, FAI_147, FAI_150, FAI_158, FAI_171, FAI_184, FAI_188
    )
  )

imputed_data <- mice(
  items_reduced, 
  m = 1, 
  maxit = 1, 
  method = 'midastouch', 
  seed = 12345
)

complete_data <- complete(imputed_data, 1)
dim(complete_data)

# Correct the multiple imputation max and min values.
complete_data_corrected <- complete_data |> 
  mutate(across(where(is.numeric), function(x) ifelse(x > 4, 4, x)))

complete_data_corrected <- complete_data_corrected |> 
  mutate(across(where(is.numeric), function(x) ifelse(x < 0, 0, x)))

mydata <- complete_data_corrected

mydata <- flag_careless_responding_LPA(mydata)

dim(mydata)

demo_info_clean <- recode_demo_information(demo_info)

fai_df <- bind_cols(demo_info_clean, mydata)


saveRDS(
  fai_df, 
  here("data", "processed", "fai_2022_11_20.rds")
)


# --- eof ---


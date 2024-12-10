# Read REDCAP data

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(rio)
  library(readr)
})


# Attempt reading with different delimiters
d <- read_csv(
  here::here("data", "raw", "redcap", "FAIv10_Cleaned.csv")
)




d <- rio::import(
  here::here("data", "raw", "redcap", "prova.xlsx")
)

data <- read.csv(
  here::here("data", "raw", "redcap", "FAIv10_DATA_2024-12-02_1208.csv"),
  sep = ",", stringsAsFactors = FALSE, fileEncoding = "latin1")

lines <- readLines(
  here::here(
  "data", "raw", "redcap", "FAIv10_20241202.csv")
)

cleaned_lines <- gsub("\"", "", lines)
writeLines(
  cleaned_lines, 
  here::here("data", "raw", "redcap", "cleaned_file.csv")
)
data <- read.csv(here::here("data", "raw", "redcap", "cleaned_file.csv"), sep = ",", stringsAsFactors = FALSE)



# Metodo 1: Utilizzando read.delim()
data <- read.delim(here::here("data", "raw", "redcap", "prova.txt"), 
                   sep = "\t",  # Specificare il separatore di tabulazione
                   na.strings = c("", "NA"),  # Gestire valori mancanti
                   stringsAsFactors = FALSE,
                   encoding = "UTF-8")  # Se ci sono caratteri speciali


library(readr)
data <- read_tsv(here::here("data", "raw", "redcap", "prova.txt"), 
                 na = c("", "NA"),  # Gestire valori mancanti
                 locale = locale(encoding = "UTF-8"))


data <- read.csv(here::here("data", "raw", "redcap", "prova.csv"),  
                 sep = "\t",
                 na.strings = c("", "NA"),
                 stringsAsFactors = FALSE,
                 fileEncoding = "UTF-8")


# Se ancora hai problemi, prova:
data <- read.csv(here::here("data", "raw", "redcap", "prova.csv"), 
                 header = TRUE, 
                 stringsAsFactors = FALSE, 
                 fileEncoding = "UTF-8", 
                 sep = ",", 
                 quote = "\"\"", 
                 blank.lines.skip = TRUE, 
                 fill = TRUE)




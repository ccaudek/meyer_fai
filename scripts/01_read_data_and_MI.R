destfile <- here("data", "processed", "fai_2019_12_17.rds")

if (!file.exists(destfile)) {
  
  # read data
  fai_p <- read_xlsx(here("data", "raw", "FAI_O_12_12_19.xlsx"), col_names = TRUE)
  fai_s <- read_xlsx(here("data", "raw", "FAI_S_27_11_19.xlsx"), col_names = TRUE)
  fai <- rbind(fai_p, fai_s)
  
  items <- fai[, 51:247]
  
  item_names  <- paste("i", 1:ncol(items), sep="")
  colnames(items) <- item_names
  
  imputed_data <- mice(
    items, 
    m = 1, 
    maxit = 1, 
    method = 'midastouch', 
    seed = 123
  )
  
  complete_data <- complete(imputed_data, 1)
  dim(complete_data)
  
  saveRDS(
    complete_data, 
    here("data", "processed", "fai_2019_12_17.rds")
  )
  
} else {
  complete_data <- readRDS(destfile)
}

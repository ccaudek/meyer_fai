
# redcap/funs/redcap_functions.R

get_data <- function(file) {
  d <- rio::import(
    here::here("data", "raw", "redcap", "FAIv10_Cleaned.csv")
  ) 
  d$`punteggi_complete\n` <- NULL
  d$redcap_survey_identifier <- NULL
  d$questionari_complete <- NULL
  d$record_id <- NULL
  
  return(d)
}

# clean data --------------------------------------------------------------

clean_data <- function(df) {
  
  df$parentela <- factor(
    df$parentela, levels = c(1, 2), labels = c("father", "mother")
  )
  
  df$sesso <- factor(
    df$sesso, levels = c(1, 2), labels = c("male", "female")
  )
  
  df$nazionalita <- factor(
    df$nazionalita, levels = c(1, 2), labels = c("italian", "foreign")
  )
  
  df$luogo_nascita <- factor(
    df$luogo_nascita, levels = c(1, 2), labels = c("italy", "other")
  )
  
  df$fratelli_o_sorelle <- factor(
    df$fratelli_o_sorelle, levels = c(1, 2), labels = c("yes", "no")
  )
  
  df$quanti_fratelli <- 
    replace(df$quanti_fratelli, is.na(df$quanti_fratelli), 0)
  
  df$presenza_malattia <- ifelse(
    df$presenza_malattia == 2, 0, df$presenza_malattia
  )
  
  df$nome_malattia <- 
    replace(df$nome_malattia, df$nome_malattia == "", "nessuna")
  
  df$altri_problemi <- ifelse(
    df$altri_problemi == 2, 0, df$altri_problemi
  )
  
  df$diffolt_psichiatriche <- ifelse(
    df$diffolt_psichiatriche == 2, 0, df$diffolt_psichiatriche
  )
  
  df$motivo_ospedale <- factor(
    df$motivo_ospedale,
    levels = c(1, 2, 3),
    labels = c("Admission", "Follow-up visit", "Specialist examination")
  )
  
  df$reparto <- 
    replace(df$reparto, df$reparto == "", NA)
  
  df$ricovero <- ifelse(df$ricovero == 2, 0, df$ricovero)
  
  df$quante_volte_ricovero[4] <- "4"
  df$quante_volte_ricovero[16] <- "10"
  df$quante_volte_ricovero[93] <- "2"
  df$quante_volte_ricovero[125] <- "2"
  df$quante_volte_ricovero[144] <- "1"
  
  df$quante_volte_ricovero <- 
    as.numeric(
      replace(df$quante_volte_ricovero, df$quante_volte_ricovero == "", "0")
    )
  
  df$nazione_madre <- ifelse(
    df$nazione_madre == 2, 0, df$nazione_madre
  )
  
  df$madre_stato_civile <- factor(df$madre_stato_civile,
                                  levels = c(1, 2, 3),
                                  labels = c("Single", "Married", "Divorced"))
  
  years_map <- c(0, 5, 8, 13, 16, 18, 19, 21)
  
  df$titolo_di_studio_madre <- ifelse(!is.na(df$titolo_di_studio_madre), 
                                      years_map[df$titolo_di_studio_madre], 
                                      NA)
  
  # Convert to numeric (if necessary, but already numeric in this approach)
  df$titolo_di_studio_madre <- as.numeric(df$titolo_di_studio_madre)
  
  df$professione_madre <- factor(df$professione_madre,
                                 levels = c(1, 2, 3, 4, 5),
                                 labels = c("Unemployed", 
                                            "Employee (fixed-term)", 
                                            "Employee (permanent)", 
                                            "Self-employed", 
                                            "Other"))
  
  df$altra_prof_madre <- 
    ifelse(df$altra_prof_madre == "", "none", df$altra_prof_madre)
  
  # Replace profession names with their English equivalents
  df$altra_prof_madre <- gsub("Ricercatrice", "Researcher", df$altra_prof_madre)
  df$altra_prof_madre <- gsub("inattiva", "Inactive", df$altra_prof_madre)
  df$altra_prof_madre <- gsub("educatrice", "Educator", df$altra_prof_madre)
  df$altra_prof_madre <- gsub("Imprenditrice", "Entrepreneur", df$altra_prof_madre)
  df$altra_prof_madre <- gsub("casalinga", "Housewife", df$altra_prof_madre, ignore.case = TRUE)
  df$altra_prof_madre <- gsub("impiegata", "Employee", df$altra_prof_madre)
  df$altra_prof_madre <- gsub("Collaboratore familiare", "Family Helper", df$altra_prof_madre)
  df$altra_prof_madre <- gsub("artigiano", "Craftsperson", df$altra_prof_madre)
  
  df$madre <- ifelse(df$madre == 2, 0, df$madre)
  
  df$nazionalit_padre <- ifelse(df$nazionalit_padre == 2, 0, df$nazionalit_padre)
  
  df$stato_civile_padre <- factor(df$stato_civile_padre,
                                  levels = c(1, 2, 3, 4),
                                  labels = c("Single", "Married", "Divorced", "Widower"))
  
  df$titolo_studio_padre <- ifelse(!is.na(df$titolo_studio_padre), 
                                      years_map[df$titolo_studio_padre], 
                                      NA)
  
  df$titolo_studio_padre <- as.numeric(df$titolo_studio_padre)
  
  df$professione_altra_padre <- factor(df$professione_altra_padre,
                                       levels = c(1, 2, 3, 4, 5),
                                       labels = c("Unemployed", 
                                                  "Employee (fixed-term)", 
                                                  "Employee (permanent)", 
                                                  "Self-employed", 
                                                  "Other"))
  
  df$altra_prof_padre <- ifelse(df$altra_prof_padre == "", "none", df$altra_prof_padre)
  
  # Replace Italian profession names with English equivalents
  df$altra_prof_padre <- gsub("imprenditore", "Entrepreneur", df$altra_prof_padre, ignore.case = TRUE)
  df$altra_prof_padre <- gsub("Marina Militare", "Navy Officer", df$altra_prof_padre, ignore.case = TRUE)
  df$altra_prof_padre <- gsub("operaio", "Worker", df$altra_prof_padre, ignore.case = TRUE)
  df$altra_prof_padre <- gsub("Ristoratore", "Restaurateur", df$altra_prof_padre, ignore.case = TRUE)
  df$altra_prof_padre <- gsub("pensionato", "Retired", df$altra_prof_padre, ignore.case = TRUE)
  df$altra_prof_padre <- gsub("Carabiniere", "Police Officer", df$altra_prof_padre, ignore.case = TRUE)
  df$altra_prof_padre <- gsub("muratore", "Mason", df$altra_prof_padre, ignore.case = TRUE)
  df$altra_prof_padre <- gsub("commerciante", "Shopkeeper", df$altra_prof_padre, ignore.case = TRUE)
  
  df$padre <- ifelse(df$padre == 2, 0, df$padre)
  
  df$legge_104 <- ifelse(df$legge_104 == 2, 0, df$legge_104)
  
  df <- df |> 
    dplyr::rename(
      professione_padre = professione_altra_padre
    )
  
  df$fai_child_charact <- 
    df$fai1 + df$fai2 + df$fai3 + df$fai4
  
  df$fai_caregiving <- 
    df$fai5 + df$fai6 + df$fai7 + df$fai8
  
  df$fai_cure <- df$fai9 + df$fai10 + df$fai11 + 
    df$fai12 + df$fai13 + df$fai14 + df$fai15
  
  df$fai_intrapsych <- df$fai16 + df$fai17 + df$fai18 + 
    df$fai19 + df$fai20
  
  df$fai_support <- df$fai21 + df$fai22 + df$fai23 +
    df$fai24
  
  df$fai_hyper <- df$fai26 + df$fai27 + df$fai28 + 
    df$fai29 


  # colnames(df)[1:35] <- c(
  #   "record_id",               # ID
  #   "questionnaire_timestamp", # Timestamp for the questionnaire
  #   "consent",                 # Consent given
  #   "relationship",            # Relationship (e.g., parent)
  #   "gender",                  # Gender
  #   "age_patient",             # Patient's age
  #   "nationality",             # Nationality
  #   "birthplace",              # Place of birth
  #   "siblings",                # Siblings
  #   "number_of_siblings",      # Number of siblings
  #   "chronic_disease",          # Presence of chronic disease
  #   "disease_name",            # Name of the disease
  #   "other_medical_problems",  # Other medical problems
  #   "psychiatric_difficulties",# Psychiatric difficulties
  #   "hospital_reason",         # Reason for hospital visit
  #   "department",              # Hospital department
  #   "admission",               # Admission status
  #   "number_of_admissions",    # Number of admissions
  #   "emergency_room",          # Emergency room visit
  #   "mother_age",              # Mother's age
  #   "mother_nationality",      # Mother's nationality
  #   "mother_marital_status",   # Mother's marital status
  #   "mother_education",        # Mother's education
  #   "mother_job",              # Mother's job
  #   "mother_other_job",        # Mother's other job
  #   "mother_law_104",          # Law 104 for the mother
  #   "father_age",              # Father's age
  #   "father_nationality",      # Father's nationality
  #   "father_marital_status",   # Father's marital status
  #   "father_education",        # Father's education
  #   "father_job",              # Father's job
  #   "father_other_job",        # Father's other job
  #   "father_law_104",          # Law 104 for the father
  #   "residence",               # Place of residence
  #   "social_services"          # Contact social services beyong the Law 104 (disability law)
  # )
  
  return(df)
}

# imputation with missForest ----------------------------------------------

impute_data <- function(fai_clean) {
  # Ensure missForest is available
  if (!requireNamespace("missForest", quietly = TRUE)) {
    stop("Please install the `missForest` package.")
  }
  
  fai_clean <- fai_clean[, c(4:44, 155:169)]
  
  # Convert character columns to factors
  fai_clean[] <- lapply(fai_clean, function(x) {
    if (is.character(x)) as.factor(x) else x
  })
  
  # Convert `POSIXct` and `POSIXt` to numeric (seconds since Unix epoch)
  fai_clean[] <- lapply(fai_clean, function(x) {
    if (inherits(x, c("POSIXct", "POSIXt"))) as.numeric(x) else x
  })
  
  # Identify Columns with Too Many Levels
  offending_columns <- names(fai_clean)[sapply(fai_clean, function(x) {
    is.factor(x) && nlevels(x) > 53
  })]
  
  # Create a Subset Excluding Offending Columns
  fai_clean_subset <- fai_clean[, !names(fai_clean) %in% offending_columns]
  
  # Convert Low-Cardinality Numeric Variables to Factors
  fai_clean_subset[] <- lapply(fai_clean_subset, function(x) {
    if (is.numeric(x) && length(unique(x)) <= 5) as.factor(x) else x
  })
  
  # Perform Imputation
  set.seed(123)
  fai_clean_imputed <- missForest(fai_clean_subset, maxiter = 10, ntree = 100)
  
  # Add Excluded Columns Back to the Imputed Dataframe
  fai_clean_complete <- cbind(
    fai_clean_imputed$ximp, fai_clean[, offending_columns, drop = FALSE]
  )
  
  # Return the Complete Dataframe
  return(fai_clean_complete)
}





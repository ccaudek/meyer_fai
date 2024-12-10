# Overview ----------------------------------------------------------------
# Associated project: Meyer FAI scale
# Script purpose: logistic regression
#
# Written by: Corrado Caudek (corrado.caudek@unifi.it)
# Version: Tue Oct 29 07:24:16 2024
# Last update: Tue Oct 29 07:24:16 2024
# Status: In progress
# Notes: 


# Load necessary libraries ------------------------------------------------

if (!requireNamespace("pacman")) install.packages("pacman")

pacman::p_load(
  here, tidyverse, TAM, mirt, lavaan, mokken, psych,
  gridExtra, grid, semTools, ids, pROC
)

source(here::here("functions", "fai_funs.R"))

df_tot <- readRDS(
  here("data", "processed", "fai_2022_11_20.rds")
)
# Add random idx for each participant.
df_tot$idx <- ids::random_id(nrow(df_tot), 4)


# Creare una nuova colonna 'categoria_diagnosi' per classificare le diagnosi
df_tot$categoria_diagnosi <-
  ifelse(grepl("Fibrosi Cistica|Leucemia|Linfoma|Sarcoma|Neuroblastoma|Tumore|Insufficienza renale|Cardiopatia|Mieloma",
    df_tot$kind_of_chronic_disease,
    ignore.case = TRUE
  ),
  "molto grave, con pericolo di morte",
  "tutto il resto"
  )


# Verificare la nuova classificazione
table(df_tot$categoria_diagnosi)

df_tot$is_life_threatening <- ifelse(
  df_tot$categoria_diagnosi == "molto grave, con pericolo di morte", 1, 0
)

# Remove carless responding participants.
temp <- df_tot |> 
  dplyr::filter(FLAG == "keep")
temp$FLAG <- NULL

good_participants <- temp$idx

# All items were first examined for normality, given recommendations that 
# item skewness values should not exceed 2 to 3 and that kurtosis values 
# should not exceed 7 to 8 (Curran et al., 1996; Finney & DiStefano, 2006).
# Get only FAI items.
# Seleziona le colonne che iniziano con "FAI_" e la colonna "is_life_threatening"
# Tutti gli item sono stati esaminati per la normalità, seguendo le raccomandazioni che 
# i valori di skewness non dovrebbero superare 2-3 e i valori di kurtosis non dovrebbero 
# superare 7-8 (Curran et al., 1996; Finney & DiStefano, 2006).

# Seleziona le colonne che iniziano con "FAI_" e la colonna "is_life_threatening"
items_df <- temp %>%
  dplyr::select(starts_with("FAI_"), is_life_threatening)

# Calcola le statistiche descrittive solo per le colonne che iniziano con "FAI_"
items_stats <- psych::describe(items_df %>% dplyr::select(-is_life_threatening))

# Filtra le variabili "FAI_" con skewness > 2.5 o kurtosis > 7.5 e ottieni i nomi delle variabili
items_skew_kurt_bad <- items_stats %>%
  dplyr::filter(skew > 2.5 | skew < -2.5 | kurtosis > 7.5) %>%
  rownames()

# Crea il dataset finale selezionando le colonne "FAI_" non problematiche e mantenendo "is_life_threatening"
df <- items_df %>%
  dplyr::select(-any_of(items_skew_kurt_bad), is_life_threatening)

# Definizione delle aree
area1 <- c("FAI_124", "FAI_106", "FAI_60", "FAI_49")
area2 <- c("FAI_129", "FAI_133", "FAI_25", "FAI_103")
area3 <- c("FAI_87", "FAI_79", "FAI_143", "FAI_192", "FAI_156", "FAI_90", "FAI_114")
area4 <- c("FAI_111", "FAI_104", "FAI_42", "FAI_119", "FAI_93")
area5 <- c("FAI_128", "FAI_115", "FAI_7", "FAI_155", "FAI_195", "FAI_80", "FAI_36", "FAI_16")
area6 <- c("FAI_101", "FAI_159", "FAI_137", "FAI_96")

# Item delle sottoscale
item_subscales <- c(area1, area2, area3, area4, area5, area6)

# Seleziona solo gli item delle sottoscale, mantenendo anche is_life_threatening
fai_data <- df %>%
  dplyr::select(all_of(item_subscales), is_life_threatening)

# Visualizza dimensioni e struttura del dataset finale
dim(fai_data)
glimpse(fai_data)

# Crea un nuovo data frame con is_life_threatening e la somma delle colonne FAI_
fai_df <- fai_data %>%
  dplyr::mutate(FAI_sum = rowSums(across(starts_with("FAI_")))) %>%
  dplyr::select(is_life_threatening, FAI_sum)

# Visualizza il nuovo data frame
glimpse(fai_df)

fai_df$fai_score <- scale(fai_df$FAI_sum) |> as.numeric()

fm <- glm(
  is_life_threatening ~ fai_score,
  family = binomial(),
  data = fai_df
)
summary(fm)

exp(coefficients(fm)[2])

confint.default(fm)[2, ]

exp(confint.default(fm)[2, ])


# 3. Plot the predicted probabilities to visualize the logistic curve
# Define 10 equally spaced points for the standardized fai_score
# Define 10 bins for the standardized fai_score
fai_df$score_bin <- cut(fai_df$fai_score, breaks = 10)

# Calculate observed data with a minimum threshold of observations per bin
observed_data <- fai_df %>%
  dplyr::group_by(score_bin) %>%
  dplyr::summarize(
    mean_score = mean(fai_score),
    observed_prob = mean(is_life_threatening),
    se = sqrt((observed_prob * (1 - observed_prob)) / n()),
    n = n()
  ) %>%
  dplyr::filter(n > 5) # Only include bins with more than 5 observations

# Define a finer sequence for the logistic curve
x_continuous <- seq(min(fai_df$fai_score), max(fai_df$fai_score), length.out = 100)
probs_continuous <- predict(fm, newdata = data.frame(fai_score = x_continuous), type = "response")

# Plot observed data
plot(fai_df$fai_score, fai_df$is_life_threatening,
     pch = 16, col = "blue", ylab = "Probability of Life-Threatening Condition",
     xlab = "Standardized FAI Sum Score (fai_score)", main = "Observed Data with Logistic Curve and Error Bars")
grid()

# Add the logistic curve
lines(x_continuous, probs_continuous, col = "darkgreen", lwd = 2)

# Plot observed data points with error bars
points(observed_data$mean_score, observed_data$observed_prob, pch = 19, col = "red")
arrows(observed_data$mean_score, 
       observed_data$observed_prob - 1.96 * observed_data$se, 
       observed_data$mean_score, 
       observed_data$observed_prob + 1.96 * observed_data$se, 
       angle = 90, code = 3, length = 0.05, col = "red")





# Calcola le probabilità previste dalla regressione logistica
fai_df$predicted_prob <- predict(fm, type = "response")

# Calcola l'AUC
auc_value <- pROC::auc(fai_df$is_life_threatening, fai_df$predicted_prob)
print(paste("AUC:", auc_value))




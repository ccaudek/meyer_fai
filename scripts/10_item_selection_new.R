# Script name: 10_item_selection.R
# Project: FAI
# Script purpose: item selection
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Jan 27 08:04:19 2021
# Last Modified Date: Thu Jul  1 08:54:06 2021
#
# Notes: Item selection follows the procedure used by Paul Silvia in
#   Psychometrics of the Self-Reflection and Insight Scale (SRIS),
#   https://osf.io/qsa5w/

# Prelims
suppressPackageStartupMessages({
  library("tidyverse")
  library("TAM")
  library("psych")
  library("lordif")
  library("ggridges")
  library("ggpubr")
  library("paletteer")
  library("readxl")
  library("mice")
  library("VIM")
  library("paran")
  library("lavaan")
  library("polycor") # for hetcor()
  library("multilevel")
  library("miceRanger")
  library("papaja")
  theme_set(theme_apa())
  library("outForest")
  library("lavaanExtra")
  # Caricare le librerie necessarie
  library(glmnet) # Per LASSO regression
  library(randomForest) # Per valutare importanza delle variabili
  library(Boruta) # Per selezione robusta delle variabili
  library(missRanger)
})

library("here")

# Increase max print
options(max.print = .Machine$integer.max)

source(here("functions", "fai_funs.R"))


# Import data -------------------------------------------------------------

fai_s <- read_xlsx(
  here("data", "raw", "FAI_TOT_2020_corrected.xlsx"),
  col_names = TRUE
)


# Demographic information -------------------------------------------------

demo_info <- recode_demo_info(fai_s)

# Creare una funzione per categorizzare la gravità delle malattie
categorize_severity <- function(disease) {
  disease <- tolower(disease) # Normalizzare il testo per evitare problemi con maiuscole/minuscole

  if (grepl("allergia|asma lieve|rinite", disease)) {
    return("Lieve")
  } else if (grepl("diabete|colite ulcerosa|artrite|cardiopatia|insufficienza renale|ipertensione", disease)) {
    return("Moderata")
  } else if (grepl("fibrosi cistica|leucemia|epilessia|distrofia|sindrome nefrosica|osteosarcoma|sarcoma|neuroblastoma", disease)) {
    return("Grave")
  } else if (grepl("tumore|cancro|metastasi|cuore ipoplasico|malattia metabolica|aciduria|encefalopatia", disease)) {
    return("Critica")
  } else {
    return(NA) # Non categorizzato
  }
}

# Applicare la funzione alla variabile chronic_disease
demo_info$severity_level <- sapply(demo_info$chronic_disease, categorize_severity)

# Definire l'ordine corretto dei livelli della variabile ordinale
severity_levels <- c("Moderata", "Lieve", "Grave", "Critica")

# Convertire la variabile in un fattore ordinale con i livelli specificati
demo_info <- demo_info %>%
  mutate(severity_level = factor(severity_level,
    levels = severity_levels,
    ordered = TRUE
  ))

# Verificare la corretta trasformazione
table(demo_info$severity_level, demo_info$death_risk)


# FAI ---------------------------------------------------------------------

# Select columns with only items
items <- fai_s[, 51:247]
hist(rowSums(items, na.rm = TRUE))

# Change items' name
item_names <- paste("item_", 1:ncol(items), sep = "")
colnames(items) <- item_names


# Remove items with too many NAs.
# Count NAs in each column
n_nas <- sapply(items, function(x) sum(is.na(x)))
n_nas
hist(n_nas)
bad_items <- names(n_nas[n_nas > 50])
bad_items

# Remove items with more than 100 NAs.
new_data <- items %>%
  dplyr::select(!all_of(bad_items))
dim(new_data)
# removed 15 items

mydata <- bind_cols(demo_info, new_data)

# add subject ID
mydata$subj_id <- as.factor(1:nrow(mydata))


# Identify bad participants -----------------------------------------------

# Select only items' responses
d_num <- mydata %>%
  dplyr::select(starts_with("i")) %>%
  dplyr::select_if(is.numeric)

# mahanobis distance ---

out <- careless::mahad(d_num)
out1 <- boxplot(out)
out1$stats[5]

foo <- data.frame(
  subj_id = mydata$subj_id,
  y = out
)

foo1 <- foo[, c(1, 2)] %>%
  dplyr::filter(y > out1$stats[5])

bad_id_mahad <- factor(foo1$subj_id)


# longstring ---

out <- careless::longstring(d_num)
out1 <- boxplot(out)
out1$stats[5]

foo <- data.frame(
  subj_id = mydata$subj_id,
  y = out
)

foo1 <- foo[, c(1, 2)] %>%
  dplyr::filter(y > out1$stats[5])

bad_id_longstring <- factor(foo1$subj_id)


# irv ---

out <- careless::irv(d_num)
boxplot(out)

out1 <- boxplot(out)
out1$stats[5]

foo <- data.frame(
  subj_id = mydata$subj_id,
  y = out
)

foo1 <- foo[, c(1, 2)] %>%
  dplyr::filter(y > out1$stats[5])

bad_id_irv <- factor(foo1$subj_id)


# person total correlation ---

cm <- colMeans(d_num, na.rm = TRUE)
person_tot_cor <- apply(d_num, 1, function(x) cor(x, cm))
out <- boxplot(person_tot_cor)

foo <- data.frame(
  subj_id = mydata$subj_id,
  person_tot_cor
) %>%
  dplyr::filter(person_tot_cor < out$stats[1])

# Flagged IDs and LPA data.frame.
bad_ids_person_tot_cor <- factor(foo$subj_id)

bad_ids <- union(
  union(
    union(bad_id_mahad, bad_id_longstring),
    bad_id_irv
  ),
  bad_ids_person_tot_cor
) |> as.numeric()

# Remove problematic participans
d_clean <- mydata[-bad_ids, ]

# Multiple imputation
temp <- d_clean %>%
  dplyr::select(starts_with("item_")) %>%
  dplyr::select_if(is.numeric)

imp <- missRanger(temp, num.trees = 100)

# Ensure the imputed values are integers and within the original range
for (col in names(temp)) {
  if (any(is.na(temp[[col]]))) { # Only adjust columns with missing values
    imp[[col]] <- round(imp[[col]]) # Round to nearest integer
    imp[[col]] <-
      pmax(min(temp[[col]], na.rm = TRUE), pmin(imp[[col]], max(temp[[col]], na.rm = TRUE)))
  }
}

demo_info2 <- mydata |>
  dplyr::select(!starts_with("item_"))

demo_clean <- demo_info2[-bad_ids, ]

df <- bind_cols(imp, demo_clean)


# Item selection ----------------------------------------------------------

# Creare una matrice con gli item come predittori
X <- as.matrix(imp)
y <- df$death_risk # Variabile binaria di esito

# **1. Selezione degli item con LASSO (Modello di regressione penalizzato)**
cv_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1) # LASSO penalizza per selezione di variabili
lasso_model <- glmnet(X, y, family = "binomial", alpha = 1, lambda = cv_lasso$lambda.min)

# Estrai gli item selezionati dal modello
selected_items <- colnames(X)[which(coef(lasso_model) != 0)] # Item selezionati da LASSO
selected_items <- selected_items[selected_items != "(Intercept)"] # Rimuovi l'intercetta
top_items_lasso <- selected_items[1:min(length(selected_items), 50)] # Seleziona i primi 30

# **2. Selezione con Random Forest (Importanza delle variabili)**
rf_model <- randomForest(X, y, importance = TRUE, ntree = 500)
importance_scores <- importance(rf_model, type = 2) # Importanza media sulla riduzione dell'errore Gini
# Convert to data frame for easier manipulation
importance_df <- as.data.frame(importance_scores)
# Sort by IncNodePurity in decreasing order
sorted_scores <- importance_df[order(importance_df[, "IncNodePurity"], decreasing = TRUE), , drop = FALSE]
# Extract row names of the top 40 items
top_items_rf <- rownames(sorted_scores)[1:min(50, nrow(sorted_scores))]


# **3. Selezione con Boruta (Approccio più conservativo)**
boruta_model <- Boruta(X, y, doTrace = 0)
confirmed_items <- getSelectedAttributes(boruta_model, withTentative = TRUE)
top_items_boruta <- confirmed_items[1:min(length(confirmed_items), 50)]

# **Risultati Finali**
top_items_lasso # Da LASSO
top_items_rf # Da Random Forest
top_items_boruta # Da Boruta


# This ensures you get the top items from any method.
# However, it may include less relevant variables.
final_items <- unique(c(top_items_lasso, top_items_rf, top_items_boruta))
final_items <- final_items[1:min(length(final_items), 30)]
print(final_items)

# This balances robustness and inclusiveness.
# Helps prevent over-filtering due to small overlaps.


# At least two methods ---

# Create a table of item frequencies across selection methods
all_items <- c(top_items_lasso, top_items_rf, top_items_boruta)
item_freq <- table(all_items)
# Select items that appear in at least two methods
final_items <- names(item_freq[item_freq >= 2])
# Ensure at most 30 items are selected
final_items <- final_items[1:min(length(final_items), 30)]
print(final_items)


# Subscales ---------------------------------------------------------------

# Define item groups for each subscale
caratteristiche_bambino <- c(
  "item_49", "item_175", "item_106", "item_60", "item_124",
  "item_86", "item_152", "item_1", "item_47", "item_121",
  "item_57", "item_167", "item_91", "item_99", "item_135",
  "item_63", "item_168", "item_5", "item_132", "item_85",
  "item_81", "item_183"
)

richieste_caregiving <- c(
  "item_164", "item_180", "item_154", "item_162", "item_105",
  "item_40", "item_196", "item_8", "item_134", "item_131",
  "item_43", "item_187", "item_136", "item_54", "item_181",
  "item_50", "item_48", "item_25", "item_133", "item_11",
  "item_33", "item_129", "item_169", "item_46", "item_174",
  "item_73", "item_127", "item_39", "item_112", "item_103",
  "item_51", "item_53", "item_58", "item_32"
)

percezione_cura_formale <- c(
  "item_156", "item_67", "item_176", "item_77", "item_28",
  "item_149", "item_35", "item_192", "item_87", "item_143",
  "item_90", "item_160", "item_69", "item_170", "item_79",
  "item_24", "item_76", "item_114", "item_74", "item_92",
  "item_163"
)

fattori_intrapsichici <- c(
  "item_177", "item_191", "item_122", "item_62", "item_104",
  "item_42", "item_111", "item_64", "item_34", "item_75",
  "item_65", "item_108", "item_119", "item_138", "item_116",
  "item_20", "item_145", "item_59", "item_118", "item_173",
  "item_23", "item_45", "item_179", "item_41", "item_139",
  "item_4", "item_56", "item_166", "item_193", "item_83",
  "item_21", "item_93"
)

coping <- c(
  "item_98", "item_125", "item_186", "item_130", "item_44", "item_172",
  "item_195", "item_66", "item_190", "item_18", "item_94", "item_80",
  "item_117", "item_100", "item_38", "item_36", "item_97", "item_197",
  "item_189", "item_185", "item_88", "item_107", "item_30", "item_19",
  "item_161", "item_128", "item_22", "item_12", "item_52", "item_95",
  "item_68", "item_148", "item_140", "item_71", "item_72", "item_142",
  "item_70", "item_141", "item_15", "item_115", "item_120", "item_7",
  "item_102", "item_178", "item_16", "item_89", "item_10", "item_6",
  "item_155", "item_82", "item_113", "item_3", "item_146", "item_29",
  "item_157", "item_26", "item_61"
)

iperprotezione <- c(
  "item_9", "item_153", "item_2", "item_137", "item_96",
  "item_165", "item_194", "item_13", "item_14", "item_101",
  "item_144", "item_31", "item_151", "item_159", "item_182",
  "item_27"
)

fratelli <- c(
  "item_188", "item_17", "item_184", "item_147", "item_109",
  "item_37", "item_171", "item_78", "item_126", "item_84",
  "item_55", "item_110", "item_123", "item_150", "item_158"
)



# Define the full subscale lists (ensure all items are included)
subscales <- list(
  "CARATTERISTICHE_DEL_BAMBINO" = c(
    "item_49", "item_175", "item_106", "item_60", "item_124",
    "item_86", "item_152", "item_1", "item_47", "item_121",
    "item_57", "item_167", "item_91", "item_99", "item_135",
    "item_63", "item_168", "item_5", "item_132", "item_85",
    "item_81", "item_183"
  ),
  "RICHIESTE_DI_CAREGIVING" = c(
    "item_164", "item_180", "item_154", "item_162", "item_105",
    "item_40", "item_196", "item_8", "item_134", "item_131",
    "item_43", "item_187", "item_136", "item_54", "item_181",
    "item_50", "item_48", "item_25", "item_133", "item_11",
    "item_33", "item_129", "item_169", "item_46", "item_174",
    "item_73", "item_127", "item_39", "item_112", "item_103",
    "item_51", "item_53", "item_58", "item_32"
  ),
  "PERCEZIONE_DELLA_CURA_FORMALE" = c(
    "item_156", "item_67", "item_176", "item_77", "item_28",
    "item_149", "item_35", "item_192", "item_87", "item_143",
    "item_90", "item_160", "item_69", "item_170", "item_79",
    "item_24", "item_76", "item_114", "item_74", "item_92",
    "item_163"
  ),
  "FATTORI_INTRAPSICHICI" = c(
    "item_177", "item_191", "item_122", "item_62", "item_104",
    "item_42", "item_111", "item_64", "item_34", "item_75",
    "item_65", "item_108", "item_119", "item_138", "item_116",
    "item_20", "item_145", "item_59", "item_118", "item_173",
    "item_23", "item_45", "item_179", "item_41", "item_139",
    "item_4", "item_56", "item_166", "item_193", "item_83",
    "item_21", "item_93"
  ),
  "COPING" = c(
    "item_98", "item_125", "item_186", "item_130", "item_44", "item_172",
    "item_195", "item_66", "item_190", "item_18", "item_94", "item_80",
    "item_117", "item_100", "item_38", "item_36", "item_97", "item_197",
    "item_189", "item_185", "item_88", "item_107", "item_30", "item_19",
    "item_161", "item_128", "item_22", "item_12", "item_52", "item_95",
    "item_68", "item_148", "item_140", "item_71", "item_72", "item_142",
    "item_70", "item_141", "item_15", "item_115", "item_120", "item_7",
    "item_102", "item_178", "item_16", "item_89", "item_10", "item_6",
    "item_155", "item_82", "item_113", "item_3", "item_146", "item_29",
    "item_157", "item_26", "item_61"
  ),
  "IPERPROTEZIONE" = c(
    "item_9", "item_153", "item_2", "item_137", "item_96",
    "item_165", "item_194", "item_13", "item_14", "item_101",
    "item_144", "item_31", "item_151", "item_159", "item_182",
    "item_27"
  ),
  "FRATELLI" = c(
    "item_188", "item_17", "item_184", "item_147", "item_109",
    "item_37", "item_171", "item_78", "item_126", "item_84",
    "item_55", "item_110", "item_123", "item_150", "item_158"
  )
)

# Input: Vector of selected items
final_items <- c(
  "item_106", "item_11", "item_121", "item_124", "item_125", "item_129",
  "item_131", "item_132", "item_133", "item_134", "item_137", "item_139",
  "item_151", "item_154", "item_157", "item_159", "item_164", "item_181",
  "item_187", "item_194", "item_20", "item_23", "item_25", "item_26",
  "item_29", "item_3", "item_33", "item_4", "item_40", "item_41"
)

# Function to filter selected items by subscale
filter_subscales <- function(subscales, final_items) {
  filtered_subscales <- lapply(subscales, function(items) intersect(items, final_items))
  filtered_subscales <- filtered_subscales[sapply(filtered_subscales, length) > 0] # Remove empty subscales
  return(filtered_subscales)
}

# Apply function and get result
filtered_result <- filter_subscales(subscales, final_items)

# Print result
print(filtered_result)



# Define the CFA model
cfa_model <- "
  CARATTERISTICHE_DEL_BAMBINO =~ item_106 + item_124 + item_121
  RICHIESTE_DI_CAREGIVING =~ item_164 + item_134 + item_131 + item_25 + item_133 + item_11 + item_33 + item_129
  FATTORI_INTRAPSICHICI =~ item_20 + item_41 + item_139 + item_4
  COPING =~ item_29 + item_157 + item_26
  IPERPROTEZIONE =~ item_137  + item_151 + item_159
"

# Fit the model using lavaan
fit <- cfa(cfa_model, data = df, std.lv = TRUE, estimator = "MLR")

# Print the summary with standardized estimates
summary(fit, fit.measures = TRUE, standardized = TRUE)



# Ensure at least 5 COGNIZIONE DI MALATTIA items are retained
cognizione_di_malattia <- c(
  "item_116", "item_20", "item_145", "item_59", "item_118",
  "item_173", "item_23", "item_45", "item_179", "item_41",
  "item_139", "item_4", "item_56", "item_166", "item_193",
  "item_83", "item_21", "item_93"
)

# Step 1: Select the 5 most predictive items from COGNIZIONE DI MALATTIA
X <- as.matrix(imp[, cognizione_di_malattia]) # Matrix of predictor variables
y <- df$death_risk # Variabile binaria di esito

cv_lasso <- cv.glmnet(X, y, family = "binomial", alpha = 1) # LASSO for variable selection
lasso_model <- glmnet(X, y, family = "binomial", alpha = 1, lambda = cv_lasso$lambda.min)

selected_cognizione <- colnames(X)[which(coef(lasso_model) != 0)] # Selected items
selected_cognizione <- selected_cognizione[selected_cognizione != "(Intercept)"] # Remove intercept

# Ensure at least 5 items are retained
selected_cognizione <- selected_cognizione[1:min(length(selected_cognizione), 5)]

augmented_items <- union(final_items, selected_cognizione)

# Function to filter selected items by subscale
filter_subscales <- function(subscales, augmented_items) {
  filtered_subscales <- lapply(subscales, function(items) intersect(items, augmented_items))
  filtered_subscales <- filtered_subscales[sapply(filtered_subscales, length) > 0] # Remove empty subscales
  return(filtered_subscales)
}

# Apply function and get result
filtered_result <- filter_subscales(subscales, augmented_items)

# Print result
print(filtered_result)

# Define the CFA model with 5 correlated factors
cfa_model <- "
  # Measurement model (CFA part)
  CARATTERISTICHE_DEL_BAMBINO =~ item_106 + item_124 + item_121
  RICHIESTE_DI_CAREGIVING =~ item_25 + item_133 + item_11 + item_33 + item_129
  FATTORI_INTRAPSICHICI =~ item_116 + item_20 + item_145 + item_59 + item_45 + item_179
  COPING =~  item_29 + item_157 + item_26
  IPERPROTEZIONE =~ item_137 + item_151 + item_159

  # Correlations between all factors
  CARATTERISTICHE_DEL_BAMBINO ~~ RICHIESTE_DI_CAREGIVING + FATTORI_INTRAPSICHICI + COPING + IPERPROTEZIONE
  RICHIESTE_DI_CAREGIVING ~~ FATTORI_INTRAPSICHICI + COPING + IPERPROTEZIONE
  FATTORI_INTRAPSICHICI ~~ COPING + IPERPROTEZIONE
  COPING ~~ IPERPROTEZIONE
"

# Fit the CFA model using lavaan
fit <- cfa(cfa_model, data = df, std.lv = TRUE, estimator = "MLR")

# Print the summary with standardized estimates and fit measures
summary(fit, fit.measures = TRUE, standardized = TRUE)

# Get modification indices (sorted by highest value)
head(modindices(fit, sort. = TRUE), 100)

# ESEM
# In ESEM, le variabili osservate sono caricate principalmente sul proprio
# fattore target, ma si permette di avere caricamenti secondari sugli altri
# fattori.

library(lavaan)

# Definizione del modello ESEM
esem_model <- "
  # Modello di misurazione con rotazione target (ESEM)
  f1 =~ item_106 + item_124 + item_121 + start(0)*item_25 + start(0)*item_133 + start(0)*item_11 + start(0)*item_33 + start(0)*item_129 +
        start(0)*item_116 + start(0)*item_20 + start(0)*item_145 + start(0)*item_59 + start(0)*item_45 + start(0)*item_179 +
        start(0)*item_29 + start(0)*item_157 + start(0)*item_26 + start(0)*item_137 + start(0)*item_151 + start(0)*item_159

  f2 =~ item_25 + item_133 + item_11 + item_33 + item_129 + start(0)*item_106 + start(0)*item_124 + start(0)*item_121 +
        start(0)*item_116 + start(0)*item_20 + start(0)*item_145 + start(0)*item_59 + start(0)*item_45 + start(0)*item_179 +
        start(0)*item_29 + start(0)*item_157 + start(0)*item_26 + start(0)*item_137 + start(0)*item_151 + start(0)*item_159

  f3 =~ item_116 + item_20 + item_145 + item_59 + item_45 + item_179 + start(0)*item_106 + start(0)*item_124 + start(0)*item_121 +
        start(0)*item_25 + start(0)*item_133 + start(0)*item_11 + start(0)*item_33 + start(0)*item_129 +
        start(0)*item_29 + start(0)*item_157 + start(0)*item_26 + start(0)*item_137 + start(0)*item_151 + start(0)*item_159

  f4 =~ item_29 + item_157 + item_26 + start(0)*item_106 + start(0)*item_124 + start(0)*item_121 +
        start(0)*item_25 + start(0)*item_133 + start(0)*item_11 + start(0)*item_33 + start(0)*item_129 +
        start(0)*item_116 + start(0)*item_20 + start(0)*item_145 + start(0)*item_59 + start(0)*item_45 + start(0)*item_179 +
        start(0)*item_137 + start(0)*item_151 + start(0)*item_159

  f5 =~ item_137 + item_151 + item_159 + start(0)*item_106 + start(0)*item_124 + start(0)*item_121 +
        start(0)*item_25 + start(0)*item_133 + start(0)*item_11 + start(0)*item_33 + start(0)*item_129 +
        start(0)*item_116 + start(0)*item_20 + start(0)*item_145 + start(0)*item_59 + start(0)*item_45 + start(0)*item_179 +
        start(0)*item_29 + start(0)*item_157 + start(0)*item_26

  # Correlazioni tra i fattori
  f1 ~~ f2 + f3 + f4 + f5
  f2 ~~ f3 + f4 + f5
  f3 ~~ f4 + f5
  f4 ~~ f5
"

# Stima del modello ESEM con lavaan
fit_esem <- sem(esem_model, data = df, std.lv = TRUE, estimator = "MLR")

# Stampa dei risultati
summary(fit_esem, fit.measures = TRUE, standardized = TRUE)


## Variabili ordinali
df1 <- df

# Convertire le variabili in ordinali (assumiamo che abbiano valori interi da 1 a 5)
ordinal_vars <- c(
  "item_106", "item_124", "item_121", "item_25", "item_133", "item_11", "item_33",
  "item_129", "item_116", "item_20", "item_145", "item_59", "item_45", "item_179",
  "item_29", "item_157", "item_26", "item_137", "item_151", "item_159"
)

df1[ordinal_vars] <- lapply(df1[ordinal_vars], function(x) ordered(x))

library(lavaan)

# Convertire le variabili in ordinali (assumiamo che abbiano valori interi da 1 a 5)
ordinal_vars <- c(
  "item_106", "item_124", "item_121", "item_25", "item_133", "item_11", "item_33",
  "item_129", "item_116", "item_20", "item_145", "item_59", "item_45", "item_179",
  "item_29", "item_157", "item_26", "item_137", "item_151", "item_159"
)

df[ordinal_vars] <- lapply(df[ordinal_vars], function(x) ordered(x))

# Definizione del modello ESEM per variabili ordinali
esem_model <- "
  # Modello di misurazione con rotazione target (ESEM)
  f1 =~ item_106 + item_124 + item_121 + start(0)*item_25 + start(0)*item_133 + start(0)*item_11 + start(0)*item_33 + start(0)*item_129 +
        start(0)*item_116 + start(0)*item_20 + start(0)*item_145 + start(0)*item_59 + start(0)*item_45 + start(0)*item_179 +
        start(0)*item_29 + start(0)*item_157 + start(0)*item_26 + start(0)*item_137 + start(0)*item_151 + start(0)*item_159

  f2 =~ item_25 + item_133 + item_11 + item_33 + item_129 + start(0)*item_106 + start(0)*item_124 + start(0)*item_121 +
        start(0)*item_116 + start(0)*item_20 + start(0)*item_145 + start(0)*item_59 + start(0)*item_45 + start(0)*item_179 +
        start(0)*item_29 + start(0)*item_157 + start(0)*item_26 + start(0)*item_137 + start(0)*item_151 + start(0)*item_159

  f3 =~ item_116 + item_20 + item_145 + item_59 + item_45 + item_179 + start(0)*item_106 + start(0)*item_124 + start(0)*item_121 +
        start(0)*item_25 + start(0)*item_133 + start(0)*item_11 + start(0)*item_33 + start(0)*item_129 +
        start(0)*item_29 + start(0)*item_157 + start(0)*item_26 + start(0)*item_137 + start(0)*item_151 + start(0)*item_159

  f4 =~ item_29 + item_157 + item_26 + start(0)*item_106 + start(0)*item_124 + start(0)*item_121 +
        start(0)*item_25 + start(0)*item_133 + start(0)*item_11 + start(0)*item_33 + start(0)*item_129 +
        start(0)*item_116 + start(0)*item_20 + start(0)*item_145 + start(0)*item_59 + start(0)*item_45 + start(0)*item_179 +
        start(0)*item_137 + start(0)*item_151 + start(0)*item_159

  f5 =~ item_137 + item_151 + item_159 + start(0)*item_106 + start(0)*item_124 + start(0)*item_121 +
        start(0)*item_25 + start(0)*item_133 + start(0)*item_11 + start(0)*item_33 + start(0)*item_129 +
        start(0)*item_116 + start(0)*item_20 + start(0)*item_145 + start(0)*item_59 + start(0)*item_45 + start(0)*item_179 +
        start(0)*item_29 + start(0)*item_157 + start(0)*item_26

  # Correlazioni tra i fattori
  f1 ~~ f2 + f3 + f4 + f5
  f2 ~~ f3 + f4 + f5
  f3 ~~ f4 + f5
  f4 ~~ f5
"

# Stima del modello ESEM con variabili ordinali
fit_esem <- sem(esem_model, data = df, std.lv = TRUE, estimator = "WLSMV", ordered = ordinal_vars)

# Stampa dei risultati
summary(fit_esem2, fit.measures = TRUE, standardized = TRUE)


# eof ---





# Multiple imputation

temp <- d_clean %>%
  dplyr::select(starts_with("i")) %>%
  dplyr::select_if(is.numeric)

imp <- mice::mice(temp, method = "norm.predict", m = 1)
df <- round(complete(imp))



# AREA 1: Caratteristiche bambino ----

source(here("scripts", "areas", "area_1.R"))

# severity of pathology
severity_items <- c("i124", "i106", "i60", "i49", "i83")







# -------------------------------------------------------------------

#

selected_items <- c(
  "i124", "i106", "i60", "i49", "i83",
  "i25", "i105", "i129", "i133",
  "i192", "i143", "i156", "i87", "i79", "i69", "i92",
  "i177", "i191", "i62", "i64", "i119",
  "i195", "i161", "i178", "i128", "i115", "i66", "i7",
  "i159", "i96", "i137", "i101", "i151"
)


# was good before
# selected_items <- c(
#   "i124", "i106",  "i60",
#   "i25", "i129", "i127", "i133",
#   "i192", "i143", "i156", "i87", "i79", "i69", "i92",
#   "i177", "i191", "i62", "i64", "i65",
#   "i195", "i161", "i178", "i128",  "i115", "i66", "i7",
#   "i171", "i126", "i147" )

final_data <- df %>%
  dplyr::select(selected_items)


fai_model <- "
  F1 =~ i124 + i106 + i60 + i49 + i83
  F2 =~ i25 + i129 + i105 + i133
  F3 =~ i192 + i143 + i156 + i87 + i79 + i69 + i92
  F4 =~ i177 + i191 + i62 + i64 + i119
  F5 =~ i195 + i161 + i178 + i128 + i115 + i66 + i7
  F6 =~ i159 + i96 + i137 + i101 + i151
"

# Get fit indices
nice_fit(fit)
#   Model     chi2  df chi2.df p   CFI   TLI RMSEA  SRMR AIC BIC
# 1   fit 1484.483 480   3.093 0 0.981 0.979 0.068 0.073  NA  NA

nice_lavaanPlot(fit, graph_options = list(rankdir = "TB"))


# fai_model <-  '
#   F1 =~ i124 + i106 + i60 + i40 + i175
#   F2 =~ i25 + i129 + i127 + i133
#   F3 =~ i192 + i143 + i156 + i87 + i79 + i69 + i92
#   F4 =~ i177 + i191 + i62 + i64 + i65
#   F5 =~ i195 + i161 + i178 + i128 + i115 + i66 + i7
#   F6 =~ i171 + i126 + i17 + i147 + i110
# '

# A1: caratteristiche bambino
# A2: richieste caregiving
# A3: percezione cura formale
# A4: fattori intrapsichici
# A5: coping
# A6: iperprotezione

# fai_model <-  '
#   F1 =~ i124 + i106 + i60
#   F2 =~ i25 + i129 + i127 + i133
#   F3 =~ i192 + i143 + i156 + i87 + i79 + i69 + i92
#   F4 =~ i177 + i191 + i62 + i64 + i65
#   F5 =~ i195 + i161 + i178 + i128 + i115 + i66 + i7
#   F6 =~ i171 + i126 + i147
# '

fit <- lavaan:::cfa(
  fai_model,
  data = final_data,
  ordered = names(final_data),
  std.lv = TRUE
)

standardizedSolution(fit)
# Latent Variables:
#   Estimate  Std.Err  z-value  P(>|z|)
# F1 =~
# i124              0.855    0.020   43.150    0.000
# i106              0.807    0.025   31.896    0.000
# i60               0.830    0.023   36.593    0.000
# i49               0.674    0.038   17.752    0.000
# i83               0.815    0.030   27.170    0.000
# F2 =~
# i25               0.765    0.025   30.285    0.000
# i129              0.791    0.024   33.218    0.000
# i105              0.850    0.025   33.655    0.000
# i133              0.842    0.022   38.393    0.000
# F3 =~
# i192              0.852    0.017   48.709    0.000
# i143              0.841    0.016   52.577    0.000
# i156              0.849    0.018   48.217    0.000
# i87               0.875    0.013   65.558    0.000
# i79               0.837    0.019   45.215    0.000
# i69               0.801    0.018   44.071    0.000
# i92               0.841    0.019   43.573    0.000
# F4 =~
# i177              0.774    0.024   31.728    0.000
# i191              0.777    0.027   28.839    0.000
# i62               0.781    0.028   28.304    0.000
# i64               0.767    0.026   30.018    0.000
# i119              0.649    0.035   18.454    0.000
# F5 =~
# i195              0.809    0.024   33.868    0.000
# i161              0.756    0.025   30.235    0.000
# i178              0.667    0.031   21.731    0.000
# i128              0.786    0.025   31.580    0.000
# i115              0.795    0.023   34.406    0.000
# i66               0.741    0.027   27.103    0.000
# i7                0.793    0.025   31.396    0.000
# F6 =~
# i159              0.775    0.031   25.005    0.000
# i96               0.721    0.033   21.737    0.000
# i137              0.734    0.031   23.647    0.000
# i101              0.555    0.037   14.901    0.000
# i151              0.667    0.036   18.590    0.000
#
# Covariances:
#   Estimate  Std.Err  z-value  P(>|z|)
# F1 ~~
# F2                0.783    0.025   31.419    0.000
# F3               -0.100    0.050   -1.987    0.047
# F4                0.027    0.048    0.556    0.578
# F5                0.012    0.049    0.237    0.813
# F6                0.541    0.041   13.269    0.000
# F2 ~~
# F3               -0.067    0.050   -1.327    0.184
# F4                0.203    0.047    4.314    0.000
# F5                0.073    0.052    1.392    0.164
# F6                0.723    0.030   24.158    0.000
# F3 ~~
# F4                0.486    0.039   12.593    0.000
# F5                0.590    0.033   17.780    0.000
# F6               -0.142    0.051   -2.764    0.006
# F4 ~~
# F5                0.602    0.035   17.415    0.000
# F6                0.105    0.051    2.058    0.040
# F5 ~~
# F6               -0.026    0.053   -0.483    0.629



# summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitMeasures(
  fit,
  c(
    "chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
    "rmsea", "rmsea.scaled", "srmr"
  )
)
#    chisq           df          cfi   cfi.scaled          tli   tli.scaled        rmsea
# 1484.483      480.000        0.981        0.941        0.979        0.935        0.068
# rmsea.scaled         srmr
#        0.066        0.073


# Descriptives ----

lowerCor(final_data, use = "pairwise.complete.obs")
corr.test(final_data, use = "pairwise.complete.obs")$ci

#####################################################
# Interpratation of KMO Value (Kaiser & Rice, 1974) #
# KMO > 0.90 ==> Marvelous,                         #
# 0.80 < KMO < 0.90 ==> Meritorious,                #
# 0.70 < KMO < 0.80 ==> Middling,                   #
# 0.60 < KMO < 0.70 ==> Mediocre,                   #
# 0.50 < KMO < 0.60 ==> Miserable,                  #
# KMO < 0.50 ==> Unacceptable.                      #
#####################################################
C <- psych::polychoric(final_data)

# Bartlett’s (1950) test of sphericity is an inferential statistic
# used to assess the factorability of R.  This statistic tests the
# null hypothesis that the population correlation matrix is equal to an
# identity matrix.  However, this test is known to be extremely sensitive
# and should be used with only relatively small samples (Tabachnick &
# Fidell, 2001).  For this reason, Dziuban and Shirkey (1974)
# propose using this statistic as a conservative indicator of factorability.
# While a significant test is still dubious, a correlation matrix with a
# non‐significant test should certainly not be subjected to factor analysis
bart <- psych::cortest.bartlett(C$rho, n = nrow(final_data), diag = TRUE)

# Kaiser (1970, 1981) viewed factorability as a psychometric issue,
# opting to use the term sampling adequacy to reflect the importance
# of sampling the right set of variables in order to detect
# any meaningful underlying structure.  He based his work in this area
# on that of Guttman, who had shown that the closer the off‐diagonal
# elements of an anti‐image correlation matrix were to zero,
# the stronger the evidence that the data have a common‐factor structure
# (Cerny & Kaiser, 1977; Kaiser, 1970).  The anti‐image of a variable is
# the residual portion of that variable remaining after
# removing the variance that can be associated with the other variables in
# the set (Gorsuch, 1983). The correlation between the anti‐images of two
# observed variables (from a set of p) can conveniently be defined as the
# opposite value of the corresponding (p - 2)-th-order partial
# correlation (Rummel, 1970).  Consequently, if there is in fact at least
# one common factor underlying a set of observed variables, then the
# anti‐image correlations will be relatively small in absolute magnitude
# as compared to the zero‐order correlations.  Using this property, Kaiser
# sought to create a non‐inferential, psychometrically based indicator of
# factorability.  What resulted from this line of thought is now better
# known as the Kaiser‐Meyer‐Olkin (KMO) measure of sampling adequacy.
kaiser <- psych::KMO(C$rho)

interpretation_KMO <- dplyr::case_when(
  kaiser$MSA >= 0.90 ~ "Marvelous",
  kaiser$MSA >= 0.80 & kaiser$MSA < 0.90 ~ "Mertitourious",
  kaiser$MSA >= 0.70 & kaiser$MSA < 0.80 ~ "Middling",
  kaiser$MSA >= 0.60 & kaiser$MSA < 0.70 ~ "Medicore",
  kaiser$MSA >= 0.50 & kaiser$MSA < 0.60 ~ "Miserable",
  kaiser$MSA < 0.50 ~ "Unacceptable"
)

Bart_KMO <- data.frame(
  KMO = round(kaiser$MSA, 3),
  Interpretation_KMO = interpretation_KMO,
  Bartlett_Chi = bart$chisq,
  Bartlett_df = bart$df,
  Bartlett_sig = sprintf("%.3f", bart$p.value)
)
Bart_KMO

# Scree Plot for determine number of factors
eigenvalues <- nFactors::eigenComputes(x = final_data)
eigen_for_graph <- data.frame(item_number = 1:ncol(final_data), eigenvalues)

scree_plot <- ggplot(data = eigen_for_graph) +
  geom_point(aes(x = item_number, y = eigenvalues)) +
  geom_line(aes(x = item_number, y = eigenvalues)) +
  xlab("Factor Number") +
  ylab("Eigenvalues") +
  papaja::theme_apa() +
  scale_x_continuous(breaks = seq(from = 1, to = ncol(final_data), by = 1))
scree_plot

# MAP analysis for examine number of dimensions
map_analysis <- psych::vss(final_data, n = (ncol(final_data) - 1))
map_factors <- which(map_analysis$map == min(map_analysis$map))
# Parallel analysis for examine number of dimensions
# Conduct Parallel analysis with Pearson Correlation Matrix
PA_pearson <- psych::fa.parallel(final_data, fa = "both", cor = "cor")
# Conduct Parallel Analysis with Polychoric Correlation Matrix
PA_poly <- psych::fa.parallel(final_data, fa = "both", cor = "poly")
results_factor_retentation <- list(
  MAP_Result = map_factors, Parallel_Analysis_Pearson = PA_pearson$nfact,
  Parallel_Analysis_Polychoric = PA_poly$nfact,
  Scree_Plot = "Look at the Plots Section for Scree Plot",
  scree_plot
)

results_factor_retentation

alpha(final_data)
splitHalf(final_data)

scree(final_data, factors = FALSE)

tot_score <- rowSums(final_data)
plot(density(tot_score, na.rm = TRUE),
  main = "Total score"
)
describe(final_data)
error.dots(final_data)
error.bars(final_data)



semTools::compRelSEM(fit)
#    F1    F2    F3    F4    F5    F6
# 0.922 0.865 0.925 0.859 0.886 0.790

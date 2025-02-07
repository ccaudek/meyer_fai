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
  library(caret)
  library(pROC)
  library(lavaan)
  library(semTools)
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


# Item selection with clinical criteria only ------------------------------

# These items have been selected on the basis of clinical criteria only.

selected_items <- c(
  "i_86", "i_57", "i_5", "i_85", "i_81", 
  "i_105", "i_48", "i_133", "i_129", "i_39", "i_103", 
  "i_143", "i_79", 
  "i_111", "i_34", "i_119", "i_116", "i_23", "i_45", "i_41", 
  "i_186", "i_38", "i_128", "i_7", "i_16", "i_29", 
  "i_137", "i_96", "i_194"
)

# Convert selected_items naming format to match column names in imp
selected_items_corrected <- gsub("^i_", "item_", selected_items)

# Select only the matching columns using dplyr
imp_selected <- imp %>% 
  dplyr::select(all_of(selected_items_corrected))

# Check the result
print(names(imp_selected))

# Define the target matrix with 29 rows and 6 columns
TARGET <- matrix(0, nrow = 29, ncol = 6)

# Assign 1s to the appropriate positions based on your target rotation
# F1 "i_86", "i_57", "i_5", "i_85", "i_81"
TARGET[1:5, 1] <- 1

# F2 "i_105", "i_48", "i_133", "i_129", "i_39", "i_103",
TARGET[6:11, 2] <- 1

# F3 "i_143", "i_79", 
TARGET[12:13, 3] <- 1

# F4 "i_111", "i_34", "i_119", "i_116", "i_23", "i_45", "i_41", 
TARGET[14:20, 4] <- 1

# F5 "i_186", "i_38", "i_128", "i_7", "i_16", "i_29", 
TARGET[21:26, 5] <- 1

# F6 "i_137", "i_96", "i_194"
TARGET[27:29, 6] <- 1

print(TARGET)

# Define the ESEM model
model <- '
    # EFA block
    efa("efa1")*f1 + 
    efa("efa1")*f2 + 
    efa("efa1")*f3 + 
    efa("efa1")*f4 + 
    efa("efa1")*f5 + 
    efa("efa1")*f6 =~ 
       item_86 + item_57 + item_5 + item_85 + item_81 + 
       item_105 + item_48 + item_133 + item_129 + item_39 + item_103 + 
       item_143 + item_79 + 
       item_111 + item_34 + item_119 + item_116 + item_23 + item_45 + item_41 + 
       item_186 + item_38 + item_128 + item_7 + item_16 + item_29 + 
       item_137 + item_96 + item_194
'

# Fit the ESEM model with target rotation
fit <- sem(
  model = model,
  data = imp_selected,  
  ordered = TRUE,
  rotation = "target",
  rotation.args = list(target = TARGET)
)

summary(fit, standardized = TRUE, fit.measures = TRUE)


# Remove the worse items ----------------------

selected_items <- c(
  "i_86", "i_57", "i_5", "i_81", 
  "i_105", "i_48", "i_133", "i_129", "i_39", "i_103", 
  "i_143", "i_79", 
  "i_111", "i_34", "i_119", "i_116", "i_23", "i_41", 
  "i_186", "i_38", "i_128", "i_7", "i_16", 
  "i_137", "i_96", "i_194"
)

# Convert selected_items naming format to match column names in imp
selected_items_corrected <- gsub("^i_", "item_", selected_items)

# Select only the matching columns using dplyr
imp_selected <- imp %>% 
  dplyr::select(all_of(selected_items_corrected))

# Check the result
print(names(imp_selected))

# Define the target matrix with 29 rows and 6 columns
TARGET <- matrix(0, nrow = 26, ncol = 6)

# Assign 1s to the appropriate positions based on your target rotation
# F1 "i_86", "i_57", "i_5", "i_81"
TARGET[1:4, 1] <- 1

# F2 "i_105", "i_48", "i_133", "i_129", "i_39", "i_103",
TARGET[5:10, 2] <- 1

# F3 "i_143", "i_79", 
TARGET[11:12, 3] <- 1

# F4 "i_111", "i_34", "i_119", "i_116", "i_23", "i_41", 
TARGET[13:18, 4] <- 1

# F5 "i_186", "i_38", "i_128", "i_7", "i_16", 
TARGET[19:23, 5] <- 1

# F6 "i_137", "i_96", "i_194"
TARGET[24:26, 6] <- 1

print(TARGET)

# Define the ESEM model
model <- '
    # EFA block
    efa("efa1")*f1 + 
    efa("efa1")*f2 + 
    efa("efa1")*f3 + 
    efa("efa1")*f4 + 
    efa("efa1")*f5 + 
    efa("efa1")*f6 =~ 
       item_86 + item_57 + item_5 + item_81 + 
       item_105 + item_48 + item_133 + item_129 + item_39 + item_103 + 
       item_143 + item_79 + 
       item_111 + item_34 + item_119 + item_116 + item_23 + item_41 + 
       item_186 + item_38 + item_128 + item_7 + item_16 +
       item_137 + item_96 + item_194
'

# Fit the ESEM model with target rotation
fit <- sem(
  model = model,
  data = imp_selected,  
  ordered = TRUE,
  rotation = "target",
  rotation.args = list(target = TARGET)
)

summary(fit, standardized = TRUE, fit.measures = TRUE)


# Combine the first two factors -------------------------------------------

selected_items <- c(
  "i_86", "i_57", "i_5", "i_81", 
  "i_105", "i_48", "i_133", "i_129", "i_39", "i_103", 
  
  "i_143", "i_79", 
  
  "i_111", "i_34", "i_119", "i_23", "i_41", "i_139",
  
  "i_186", "i_38", "i_128", "i_7", "i_16",      "i_125", 
  
  "i_137", "i_96", "i_194",    "i_159"
)

# Convert selected_items naming format to match column names in imp
selected_items_corrected <- gsub("^i_", "item_", selected_items)

# Select only the matching columns using dplyr
imp_selected <- imp %>% 
  dplyr::select(all_of(selected_items_corrected))

# Check the result
print(names(imp_selected))

# Define the target matrix with 33 rows and 5 columns
TARGET <- matrix(0, nrow = 28, ncol = 5)

#### caratteristiche bambino e richieste caregiving
# Assign 1s to the appropriate positions based on your target rotation
# F1 "i_86", "i_57", "i_5", "i_81", "i_105", "i_48", "i_133", "i_129", "i_39", 
# "i_103", ### i_40, i_99, i_121 i_164 i_187
TARGET[1:10, 1] <- 1

#### percezione cura
# F2 "i_143", "i_79", 
TARGET[11:12, 2] <- 1

#### fattori intrapsichici
# F3 "i_111", "i_34", "i_119", "i_23", "i_41", ###  i_139
TARGET[13:18, 3] <- 1

#### coping
# F4 "i_186", "i_38", "i_128", "i_7", "i_16", ###  i_125, 
TARGET[19:24, 4] <- 1

#### iperprotezione
# F5 "i_137", "i_96", "i_194" ### i_159
TARGET[25:28, 5] <- 1

print(TARGET)

# Define the ESEM model
model <- '
    # EFA block
    efa("efa1")*f1 + 
    efa("efa1")*f2 + 
    efa("efa1")*f3 + 
    efa("efa1")*f4 + 
    efa("efa1")*f5 =~ 
       item_86 + item_57 + item_5 + item_81 + 
       item_105 + item_48 + item_133 + item_129 + item_39 + item_103 + 
       item_143 + item_79 + 
       item_111 + item_34 + item_119 + item_23 + item_41 + item_139 +
       item_186 + item_38 + item_128 + item_7 + item_16 + item_125 +
       item_137 + item_96 + item_194 + item_159
'

# Fit the ESEM model with target rotation
fit <- sem(
  model = model,
  data = imp_selected,  
  ordered = TRUE,
  rotation = "target",
  rotation.args = list(target = TARGET)
)

summary(fit, standardized = TRUE, fit.measures = TRUE)

# ---- Additional items that maximize classification accuracy

# item_22  item_23  item_26  item_40  item_99 item_121 item_125 item_139 
# item_157 item_159 item_164 item_187 


# Extract manifest variables (indicators) from fit_6f
indicator_names <- lavNames(fit, type = "ov")


XX <- imp_selected |> 
  dplyr::select(all_of(indicator_names))

X <- as.matrix(XX)
y <- df$death_risk # Variabile binaria di esito


# Ensure death_risk is a factor (binary classification)
df$death_risk <- as.factor(df$death_risk)

# Merge imp_selected with the criterion variable
df_model <- imp_selected %>%
  mutate(death_risk = df$death_risk)

# Split data into training and test sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(df_model$death_risk, p = 0.8, list = FALSE)
train_data <- df_model[trainIndex, ]
test_data <- df_model[-trainIndex, ]

# Fit logistic regression model
logit_model <- glm(death_risk ~ ., data = train_data, family = binomial)

# Predict on test set
pred_probs <- predict(logit_model, test_data, type = "response")
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)  # Convert probabilities to class labels

# Compute classification accuracy
accuracy <- mean(pred_classes == test_data$death_risk)
cat("Classification Accuracy:", accuracy, "\n")

# Compute AUC (Area Under the Curve)
roc_curve <- roc(test_data$death_risk, pred_probs)
auc_value <- auc(roc_curve)
cat("AUC:", auc_value, "\n")

# Permutation Test to Compare Against Chance Level
set.seed(123)
null_accuracies <- replicate(1000, {
  shuffled_risk <- sample(train_data$death_risk)  
  # Shuffle the labels within train_data
  
  null_model <- glm(shuffled_risk ~ ., data = train_data, family = binomial)
  
  null_preds <- predict(null_model, test_data, type = "response")
  null_classes <- ifelse(null_preds > 0.5, 1, 0)
  
  mean(null_classes == test_data$death_risk)  
  # Compare against actual test labels
})

# Compute p-value for classification above chance
p_value <- mean(null_accuracies >= accuracy)
cat("P-value for classification above chance:", p_value, "\n")


# Plot ROC Curve
plot(roc_curve, col = "blue", main = "ROC Curve")
abline(a = 0, b = 1, lty = 2, col = "red")  # Reference line for random classification

table(df$death_risk)


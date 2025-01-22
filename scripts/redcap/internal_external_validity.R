# Overview ----------------------------------------------------------------
# Associated project: Meyer FAI
# Script purpose: Internal and external validity of the FAI scale
# questionnaire.
#
# Written by: Corrado Caudek (corrado.caudek@unifi.it)
# Version: Tue Dec 10 13:07:41 2024
# Last update: 
# Status: In progress
# Notes: source the target.R file.


library(here)


# Import RedCap data for the second administration of FAI -----------------

setwd("~/_repositories/meyer_fai/scripts/redcap")

# This will load the necessary libreries
source(here::here("scripts", "redcap", "_targets.R"))

# Read cleaned RedCap data
temp <- tar_read(fai_clean_complete)
# Some cases are empty and will be ignored: 12 18 76 81 152 182 193 202
temp1 <- temp[-c(12, 18, 76, 81, 152, 182, 193, 202), ]
# Remove outlier found with the performance package
fai_clean_complete <- temp1[-92, ]
rm(temp, temp1)

fai <- fai_clean_complete |> 
  dplyr::select(
    fai_child_charact, fai_caregiving, fai_cure, fai_intrapsych, 
    fai_support, fai_hyper,
    cope_sc1, cope_sc2, cope_sc3, cope_sc4, cope_sc5,
    dass_de_sc, dass_ax_sc, dass_str_sc
  ) |> 
  dplyr::rename(
    fai_child_char = fai_child_charact,
    fai_caregiving = fai_caregiving,
    fai_percz_cure = fai_cure,
    fai_intra_psyc = fai_intrapsych,
    fai_soc_fam_sp = fai_support,
    fai_hyper_prot = fai_hyper,
    cope_soc_supp = cope_sc1,
    cope_evit_str = cope_sc2,
    cope_posi_att = cope_sc3,
    cope_prob_ori = cope_sc4,
    cope_tran_ori = cope_sc5,
    dass_de = dass_de_sc,
    dass_ax = dass_ax_sc,
    dass_st = dass_str_sc
  )

psych::describe(fai)

cor(fai) |> round(2)


fit <- lm(
  cbind(
    fai_child_char, fai_caregiving, fai_percz_cure,
    fai_intra_psyc, fai_soc_fam_sp, fai_hyper_prot
  ) ~
    cope_soc_supp + cope_evit_str + cope_posi_att + cope_prob_ori +
    cope_tran_ori +
    dass_de + dass_ax + dass_st,
  data = fai
)

# Analisi complessiva
summary(fit)


eigen(cov(cbind(
  fai$cope_soc_supp, fai$cope_evit_str, fai$cope_posi_att,
  fai$cope_prob_ori, fai$cope_tran_ori)))
# $values
# [1] 107.80433  31.13201  24.34650  14.63922  10.27568

# Eseguire una PCA sulle scale del COPE
cope_data <- fai[, c("cope_soc_supp", "cope_evit_str", "cope_posi_att", "cope_prob_ori", "cope_tran_ori")]
pca_cope <- prcomp(cope_data, scale. = TRUE)

# Estrarre i punteggi dei primi 2 componenti principali
fai$COPE_PC1 <- pca_cope$x[, 1]
fai$COPE_PC2 <- pca_cope$x[, 2]

eigen(cov(cbind(fai$dass_de, fai$dass_ax, fai$dass_st)))
# $values
# [1] 45.097073  3.848489  3.076940

dass_data <- fai[, c("dass_de", "dass_ax", "dass_st"), ]
pca_dass <- prcomp(dass_data, scale. = TRUE)
fai$DASS_PC1 <- pca_dass$x[, 1]

# Modello MANOVA
manova_fit <- manova(
  cbind(
    fai_child_char, fai_caregiving, fai_percz_cure,
    fai_intra_psyc, fai_soc_fam_sp, fai_hyper_prot
  ) ~
    COPE_PC1 + COPE_PC2 + DASS_PC1,
  data = fai
)

summary(manova_fit)




fm <- glm(
  presenza_malattia ~ fai_child_charact + fai_caregiving + fai_cure +
    fai_intrapsych + fai_support + fai_hyper,
  family = binomial(link = "logit"),
  data = fai_clean_complete
)

summary(fm)
#                   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)       -1.94699    1.08756  -1.790  0.07342 .  
# fai_child_charact -0.09340    0.07746  -1.206  0.22788    
# fai_caregiving     0.45487    0.08469   5.371 7.82e-08 ***
# fai_cure           0.08181    0.03789   2.159  0.03084 *  
# fai_intrapsych    -0.01432    0.07104  -0.202  0.84022    
# fai_support       -0.05821    0.07845  -0.742  0.45807    
# fai_hyper         -0.15211    0.05668  -2.684  0.00728 ** 

# fai_hyper: iperprotezione
# fai_cure: percezione della cura formale
# fai_caregiving: richieste di caregiving

predicted_probs <- predict(fm, type = "response")

# Compute AUC
roc_obj <- roc(fai_clean_complete$presenza_malattia, predicted_probs)
auc_value <- auc(roc_obj)

# Print AUC value
print(auc_value)
# Area under the curve: 0.8565

# Optional: Plot ROC curve
plot(roc_obj, main = "ROC Curve", col = "blue", lwd = 2)

weights <- ifelse(
  fai_clean_complete$presenza_malattia == 1,
  163 / 50, 1
) # Inverti il rapporto

fm <- glm(
  presenza_malattia ~ fai_child_charact + fai_caregiving + fai_cure +
    fai_intrapsych + fai_support + fai_hyper,
  family = binomial(link = "logit"),
  data = fai_clean_complete,
  weights = weights
)

predicted_probs <- predict(fm, type = "response")
# Area under the curve: 0.8643


# Compute AUC
roc_obj <- roc(fai_clean_complete$presenza_malattia, predicted_probs)
auc_value <- auc(roc_obj)

# Print AUC value
print(auc_value)



# Confusion matrix e metriche
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)
conf_matrix <- confusionMatrix(
  factor(predicted_classes),
  factor(fai_clean_complete$presenza_malattia)
)

# Stampare precisione, richiamo e F1-Score
conf_matrix$byClass[c("Precision", "Recall", "F1")]
# Precision    Recall        F1 
# 0.9104478 0.7922078 0.8472222 


fm1 <- lm(
  fai_caregiving ~ cope_sc1 + cope_sc2 + cope_sc3 + cope_sc4 + cope_sc5,
  data = fai_clean_complete
)
summary(fm1)

fm2 <- lm(
  fai_caregiving ~ dass_de_sc + dass_ax_sc + dass_str_sc,
  data = fai_clean_complete
)
summary(fm2)


# Ribilanciamento del Dataset ---------------------------------------------

# Oversampling della classe minoritaria: Puoi aumentare la frequenza 
# della classe minoritaria (1 nel tuo caso) per bilanciare il dataset. 
# Questo può essere fatto replicando i casi della classe minoritaria.

d <- fai_clean_complete |> 
  dplyr::select(
    c(fai_child_charact, fai_caregiving, fai_cure, fai_intrapsych, 
      fai_support, fai_hyper, presenza_malattia)
  )
# d$presenza_malattia <- as.numeric(as.character(d$presenza_malattia))

table(d$presenza_malattia)
#   0   1 
# 154  50 

data_balanced <- 
  ovun.sample(
    presenza_malattia ~ ., 
    data = d, 
    method = "over", 
    N = 2000)$data

table(data_balanced$presenza_malattia)
#   0    1 
# 154 1846 

mod1 <- glm(
  presenza_malattia ~ fai_child_charact + fai_caregiving + fai_cure +
    fai_intrapsych + fai_support + fai_hyper,
  family = binomial(link = "logit"),
  data = data_balanced
)

predicted_probs <- predict(mod1, type = "response")
roc_obj <- roc(data_balanced$presenza_malattia, predicted_probs)
auc_value <- auc(roc_obj)
print(auc_value)
# Area under the curve: 0.8651

# Modifica della Funzione di Costo ----------------------------------------

# Ponderazione delle classi: Assegna un peso maggiore alla classe 
# minoritaria nella funzione di perdita. Questo fa sì che gli errori nella 
# classe minoritaria abbiano un impatto maggiore sul modello.

mod2 <- glm(
  presenza_malattia ~ fai_child_charact + fai_caregiving + fai_cure +
    fai_intrapsych + fai_support + fai_hyper, 
  family = binomial(), 
  data = d, 
  weights = ifelse(presenza_malattia == 1, 3, 1)
)

predicted_probs <- predict(mod2, type = "response")
roc_obj <- roc(d$presenza_malattia, predicted_probs)
auc_value <- auc(roc_obj)
print(auc_value)
# Area under the curve: 0.8642

# Valutazione del Modello con Metriche Appropriate ------------------------

# Precision e Recall: Sono particolarmente utili quando le classi sono 
# sbilanciate.
# F1-Score: Combina precision e recall in un unico indicatore.
# Curva ROC e AUC: Forniscono una misura della capacità del modello di 
# distinguere tra le classi a vari livelli di soglia.

confusionMatrix(
  data = predicted, 
  reference = fai_clean_complete$presenza_malattia)

# vedi sopra

# Uso di Metodi di Classificazione Robusti al Sbilanciamento --------------

# Algoritmi come Random Forest, XGBoost, o SVM possono essere configurati 
# per gestire meglio le classi sbilanciate, impostando parametri specifici o 
# utilizzando funzioni di costo modificati.

data_matrix <- 
  xgb.DMatrix(data = model.matrix(~ .-1, data = d), label = d$presenza_malattia)
params <- list(objective = "binary:logistic", scale_pos_weight = 9)
xgb_model <- xgb.train(params = params, data = data_matrix, nrounds = 100)
# NON funziona

train_control <- trainControl(
  method = "cv", number = 10, classProbs = TRUE, 
  summaryFunction = twoClassSummary, sampling = "down")

temp <- d
temp$y <- ifelse(temp$presenza_malattia == 1, "yes", "no") |> 
  as.factor()
temp$presenza_malattia <- NULL

model <- train(
  y ~ ., 
  data = temp, 
  method = "rf", 
  trControl = train_control, 
  metric = "ROC"
)
model
# mtry  ROC        Sens       Spec
# 2     0.8206667  0.6883333  0.80
# 4     0.8175000  0.6954167  0.78
# 6     0.8067500  0.6637500  0.76


# CFA ---------------------------------------------------------------------

temp <- tar_read(fai_clean)

# some cases are empty and will be ignored: 12 18 76 81 152 182 193 202
fai_clean <- temp[-c(12, 18, 76, 81, 152, 182, 193, 202), ]

# select only the FAI items
fai_dat <- fai_clean |>
  dplyr::select(all_of(starts_with("fai"))) |>
  dplyr::select(
    -c(
      fai_sc, fai_child_charact, fai_caregiving, fai_cure, fai_intrapsych,
      fai_support, fai_hyper
    )
  )

sapply(fai_dat, function(x) length(unique(x)))

# convert to factors for missForest
fai_dat[] <- lapply(fai_dat, as.factor)

# Perform imputation
set.seed(123)
temp <- missForest(fai_dat, maxiter = 10, ntree = 100)
fai_imputed <- temp$ximp
str(fai_imputed)

# Convert all factor columns to numeric
fai_imputed[] <- lapply(fai_imputed, function(x) as.numeric(as.character(x)))
str(fai_imputed)

# Remove outliers ---------------------------------------------------------

# These two items are not handled by check_outliers()
zero_iqr_cols <- names(fai_imputed)[sapply(fai_imputed, IQR) == 0]
zero_iqr_cols

# I will not consider fai1 and fai6 for the outlier analysis. 
# I exclude them from the analysis of check_outliers().
fai_imputed_filtered <- 
  fai_imputed[, !names(fai_imputed) %in% c("fai1", "fai6")]

outliers <- performance::check_outliers(
  fai_imputed_filtered,
  method = c("zscore_robust", "mcd", "cook", "bci", "eti", "ci"), 
  verbose = FALSE
)

which(outliers)

fai_df <- fai_imputed[-92, ]

mvn(data = fai_df[, ordinal_vars], mvnTest = "royston")


# CFA / ESEM --------------------------------------------------------------

ordinal_vars <-
  c(
    "fai1", "fai2", "fai3", "fai4", "fai5", "fai6", "fai7", "fai8",
    "fai9", "fai10", "fai11", "fai12", "fai13", "fai14", "fai15",
    "fai16", "fai17", "fai18", "fai19", "fai20", "fai21", "fai22",
    "fai23", "fai24", "fai26", "fai27", "fai28", "fai29"
  )

fai_model <- "
  charac  =~ NA*fai1 + fai2 + fai3 + fai4
  caregiving =~ NA*fai5 + fai6 + fai7 + fai8
  cure   =~ NA*fai9 + fai10 + fai11 + fai12 + fai13 + fai14 + fai15
  intra =~ NA*fai16 + fai17 + fai18 + fai19 + fai20
  support =~ NA*fai21 + fai22 + fai23 + fai24
  hyper =~ NA*fai26 + fai27 + fai28 + fai29
"

fai_df[] <- lapply(fai_df, function(x) as.numeric(as.character(x)))

# CFA for continuous data
fit1 <- cfa(
  fai_model,
  data = fai_df, 
  std.lv = TRUE
)

summary(fit1, fit.measures = TRUE, standardized = TRUE)

# CFA for ordinal data
fit2 <- cfa(
  fai_model,
  data = fai_df, 
  std.lv = TRUE,
  ordered = ordinal_vars
)

summary(fit2, fit.measures = TRUE, standardized = TRUE)


# Define the ESEM model with target factors
fai_esem <- '
  # EFA block with target factors and cross-loadings
  efa("target")*charac +
  efa("target")*caregiving +
  efa("target")*cure +
  efa("target")*intra +
  efa("target")*support +
  efa("target")*hyper =~
    # Primary factor loadings
    fai1 + fai2 + fai3 + fai4 +           # charac items
    fai5 + fai6 + fai7 + fai8 +           # caregiving items
    fai9 + fai10 + fai11 + fai12 +
    fai13 + fai14 + fai15 +                # cure items
    fai16 + fai17 + fai18 + fai19 + fai20 + # intra items
    fai21 + fai22 + fai23 + fai24 +        # support items
    fai26 + fai27 + fai28 + fai29          # hyper items

  # Structural model (optional)
  # Define relationships between latent variables
  caregiving ~ charac
  cure ~ caregiving
  intra ~ cure
  support ~ intra
  hyper ~ support
'

# Fit the ESEM model
# Note: Replace 'your_data' with your actual dataset
fit_esem <- sem(fai_esem,
  data = fai_df,
  estimator = "MLR", # Maximum Likelihood with Robust standard errors
  rotation = "geomin"
) # Geomin rotation for target factors

# Full model summary
summary(fit_esem,
  standardized = TRUE, # Show standardized estimates
  fit.measures = TRUE, # Include model fit indices
  rsquare = TRUE
) # Include R-squared values

# Extract specific fit measures
fitMeasures(
  fit_esem,
  c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "srmr")
)

# Robust CFI = 0.951
# Robust TLI = 0.915
# SRMR Scaled = 0.025
# RMSEA Scaled = 0.067


rf_model <- randomForest(
  presenza_malattia ~ fai_child_charact + fai_caregiving +
    fai_cure + fai_intrapsych + fai_support + fai_hyper,
  data = fai_clean_complete,
  ntree = 100,
  importance = TRUE
)
print(rf_model)

importance(rf_model)

t.test(fai_child_charact ~ presenza_malattia, data = fai_clean_complete)
t.test(fai_caregiving ~ presenza_malattia, data = fai_clean_complete)
t.test(fai_cure ~ presenza_malattia, data = fai_clean_complete)
t.test(fai_intrapsych ~ presenza_malattia, data = fai_clean_complete)
t.test(fai_support ~ presenza_malattia, data = fai_clean_complete)
t.test(fai_hyper ~ presenza_malattia, data = fai_clean_complete)


fm <- glm(
  altri_problemi ~ fai_child_charact + fai_caregiving + fai_cure +
    fai_intrapsych + fai_support + fai_hyper,
  family = binomial(link = "logit"),
  data = fai_clean_complete
)
summary(fm)

fm <- glm(
  quante_volte_ricovero ~ fai_child_charact + fai_caregiving + fai_cure +
    fai_intrapsych + fai_support + fai_hyper,
  # family=binomial(link='logit'),
  data = fai_clean_complete
)
summary(fm)


fm <- glm(
  diffolt_psichiatriche ~ fai_child_charact + fai_caregiving + fai_cure +
    fai_intrapsych + fai_support + fai_hyper,
  family = binomial(link = "logit"),
  data = fai_clean_complete
)
summary(fm)


fm <- lm(
  fai_caregiving ~ dass_de_sc + dass_ax_sc + dass_str_sc,
  # family=binomial(link='logit'),
  data = fai_clean_complete
)
summary(fm)


dd <- fai_clean_complete |>
  dplyr::select(
    dplyr::starts_with("dass"),
    dplyr::starts_with("fai"),
    dplyr::starts_with("cope")
  )

cor(dd) |> round(2)

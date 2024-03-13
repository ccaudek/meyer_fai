# Script name: 10_item_selection.R
# Project: FAI
# Script purpose: item selection
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Wed Jan 27 08:04:19 2021
# Last Modified Date: Sat May  8 07:11:03 2021
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
})

library("here")

# Increase max print
options(max.print = .Machine$integer.max)

source(here("libraries", "fai_fnct.R"))

# Read data
fai_s <- read_xlsx(here("data", "raw", "FAI_TOT_2020.xlsx"), col_names = TRUE)

# Select demographic information
demo_info <- fai_s[, 1:50]

new_demo_info_names <- c(
  "id1", "id2", "date_code", "relationship", "child_age", "child_sex", "child_birthdate",
  "child_nationality", "child_nationality_code", "child_birth_place", 
  "child_birth_place_code", "brothers", "sisters", "has_chronic_disease", 
  "chronic_disease", "child_other_problems", "child_other_chronic_disease",
  "child_psychological_problems", "hospitalization", "hospitalization_number",
  "has_emergency_care", "number_emergency_care", "mother_age", "mother_nationality",
  "mother_nationality_code", "mother_marital_status", "mother_education",
  "mother_job", "mother_job_code", "mother_194", "father_age", "father_nationality",
  "father_nationality_code", "father_marital_status", "father_education",
  "father_job", "father_job_code", "father_194", "address", "address_other_region",
  "social_services", "death_loved_one", "divorce", "bad_health", "job_change",
  "low_income", "change_address", "change_city", "other", "other_code"
)

names(demo_info) <- new_demo_info_names
glimpse(demo_info)

hist(demo_info$child_age)
table(demo_info$child_sex)
hist(demo_info$brothers)
hist(demo_info$sisters)
table(demo_info$has_chronic_disease)
table(demo_info$chronic_disease) # split into meaningful categories
table(demo_info$child_other_problems) 
table(demo_info$child_psychological_problems) # meaning?
table(demo_info$hospitalization) # meaning?
table(demo_info$hospitalization_number)
table(demo_info$has_emergency_care)
table(demo_info$number_emergency_care)

x <- as.character(demo_info$mother_age)
xx <- ifelse(x == "mancano le informazioni sulla madre", NA, x)
xxx <- as.numeric(as.character(xx))
demo_info$mother_age <- xxx
hist(demo_info$mother_age)
table(demo_info$mother_nationality)
table(demo_info$mother_marital_status) # meaning?
table(demo_info$mother_education) # meaning?
table(demo_info$mother_job) # meaning?

table(demo_info$mother_194)

table(demo_info$father_age)
x <- as.character(demo_info$father_age)
xx <- ifelse(x == "deceduto" | x == "non ci sono informazioni sul padre" |
               x == "Si oppone alla trasmissione delle informazioni sul padre", NA, x)
xxx <- as.numeric(as.character(xx))
demo_info$father_age <- xxx
hist(demo_info$father_age)
table(demo_info$father_education) # meaning?

table(demo_info$social_services) # meaning?
table(demo_info$social_services) # meaning?
table(demo_info$death_loved_one) # meaning?
table(demo_info$divorce) # meaning?
table(demo_info$bad_health) # meaning?
table(demo_info$low_income) # meaning?


# Use demo id1 to classify cases into death-risk or not
death_risk_categories <- c(
  "FC", "MM", "NEUROLOGIA", "NEURO-ONCO", "ONCO", "RIA"
)

demo_info$death_risk <- ifelse(
  demo_info$id1 %in% death_risk_categories, 1, 0
)
table(demo_info$death_risk)


# Select columns with only items
items <- fai_s[, 51:247]
hist(rowSums(items, na.rm = TRUE))

# Change items' name
item_names <- paste("i", 1:ncol(items), sep = "")
colnames(items) <- item_names

# Remove items with too many NAs.
# Count NAs in each column
n_nas <- sapply(items, function(x) sum(is.na(x)))
n_nas
hist(n_nas)
bad_items <- names(n_nas[n_nas > 100])
bad_items

# Remove items with more than 100 NAs.
new_data <- items %>% 
  dplyr::select(!all_of(bad_items))
dim(new_data)
# removed 15 items

mydata <- bind_cols(demo_info, new_data)

# add subject ID
mydata$subj_id <- as.factor(1:nrow(mydata))

# Select only items' responses
d_num <- mydata %>% 
  dplyr::select(starts_with("i")) %>% 
  dplyr::select_if(is.numeric)


# Remove outliers

# mahanobis distance
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


# longstring 
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


# irv
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


# person total correlation

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
)

d_clean <- mydata[!mydata$subj_id %in% bad_ids, ]






# Multiple imputation

# Imputing missing data with miceRanger. miceRanger performs 
# Multiple Imputation by Chained Equations (MICE) with random forests.
miceObj <- miceRanger(
  d_clean,
  m = 1,
  returnModels = TRUE,
  verbose = TRUE
)

# Create complete dataframe.
dataList <- completeData(miceObj)
complete_data <- dataList[[1]]
dim(complete_data)

d_num <- dplyr::select_if(d_clean, is.numeric)


imp <- mice::mice(d_num, method = "norm.predict", m = 1) 
df <- round(complete(imp))


# imputed_data <- mice(
#   items, 
#   m = 1, 
#   maxit = 1, 
#   method = 'pmm', 
#   seed = 123
# )
# complete_data <- complete(imputed_data, 1)
# dim(complete_data)



# AREA 1: Caratteristiche bambino ----

source(here("scripts", "areas", "area_1.R"))


# First subscale: items names.
item_subscale <- c(
  "i49", "i175", "i106", "i60", "i124", "i86", "i152", "i1", "i47", 
  "i121", "i57", "i167", "i91", "i99", "i135", "i63", "i168", "i5", 
  "i132", "i85", "i81", "i83"
)

# Select only the items of this subscale.
subscale_data <- complete_data %>% 
  dplyr::select(all_of(item_subscale))
dim(subscale_data)

# (1) Check for poor item fit (Infit, Outfit, RMSD), which flags 
# items which generate scores that are relatively poorly predicted 
# by the IRT model.

# Fit 2PL IRT Generalized Partial Credit Model
subscale_2pl <- TAM::tam.mml.2pl(
  subscale_data,
  irtmodel = "GPCM",
  control = list(Msteps = 10, QMC = FALSE, snodes = 0,
                 convD = .0001, conv = .00001, convM = .00001)
)
summary(subscale_2pl)

subscale_fit <- msq.itemfit(subscale_2pl) # mean square infit, outfit
subscale_fit$itemfit

outfit <- subscale_fit$itemfit[, c(1, 3)]
outfit$delta <- abs(outfit$Outfit - 1)
outfit %>% 
  arrange(delta)
# the last one item are bad; the others are good

subscale_rmsd <- IRT.itemfit(subscale_2pl) # RMSD item fit
subscale_rmsd$RMSD %>% 
  arrange(Group1)
# perhaps the last two items are bad; the others are good

# (2) Check for local dependence (aQ3), which impairs unidimensionality.
subscale_res <- TAM::tam.modelfit(subscale_2pl)
subscale_res$Q3_summary
# A cut-off of |.25| is used to flag the most notably dependent item pairs.
round(subscale_res$aQ3.matr, 3)

# (3) Look for unreasonable difficulty levels (IRT b), which are either 
# too hard or too easy for the target population.
summary(subscale_2pl)
# i1, i135, i81: beta big or low

# (4) Look for weak slope/discrimination values (IRT a), which indicates 
# that an item provides relatively little information for reliably 
# rank-ordering participants.
summary(subscale_2pl)
# i1, i86, i47, i167, i99, i132, i85,

# To shorten the original item pool we selected the items that 
# offered more information by considering the shape of each item 
# information function, which display the amount of item information
# along the latent trait. 
mod <- mirt(
  data = subscale_data,
  1,
  itemtype = "graded",
  SE = TRUE,
  verbose = FALSE
)

n_item <- length(names(subscale_data))
plot(
  mod,
  type = 'trace',
  which.items = 1:n_item,
  main = "",
  par.settings = simpleTheme(lty = 1:4, lwd = 2),
  auto.key = list(points = FALSE, lines = TRUE, columns = 4)
)

plot(
  mod,
  type = 'infotrace',
  which.items = 1:n_item,
  main = "",
  par.settings = simpleTheme(lwd = 2)
)

# we selected the items that conveyed higher information along 
# the trait continuum 
good_items <- c("i124", "i106", "i60", "i49",  "i83") # "i175",
# When considering the items content, it can be noted that the 
# retained items addressed the key features of...
good_data <- subscale_data %>% 
  dplyr::select(
    all_of(good_items)
  )
dim(good_data)

# We also checked the fit indexes of a CFA model.
one_factor_model <-  '
  F1 =~ i124 + i106 + i60 + i49 + i83
'

fit <- lavaan:::cfa(
  one_factor_model,
  data = good_data,
  ordered = names(good_data),
  std.lv = TRUE
)

# summary(fit, standardized = TRUE)
fitMeasures(
  fit, c("chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
    "rmsea", "rmsea.scaled", "srmr")
)
# The fit measures are excellent.


# Now we redo the fit of the IRT model by considering only the
# selected items.

# TAM IRT Models ----
# 0-4 items, as required by TAM

# Rasch model
good_items_r <- TAM::tam.mml(
  good_data,
  irtmodel = "PCM",
  control = list(Msteps = 10, QMC = FALSE, snodes = 0, 
                 convD = .0001, conv = .00001, convM = .00001)
)
# summary(good_items)

# 2PL IRT Generalized Partial Credit Model
good_items_2pl <- TAM::tam.mml.2pl(
  good_data,
  irtmodel = "GPCM",
  control = list(Msteps = 10, QMC = FALSE, snodes = 0,
                 convD = .0001, conv = .00001, convM = .00001)
)
summary(good_items_2pl)


# Item & person fit 

# For infit and outfit, a value of 1 indicates there is a perfect 
# fit but values less than 0.70 and greater than 1.30 are termed 
# misfitting and over fitting, respectively.
good_items_fit <- msq.itemfit(good_items_2pl) # mean square infit, outfit
good_items_fit$itemfit
# RMSD values greater than .50 indicate “medium” misfit and are 
# flagged with an asterisk. 
good_items_rmsd <- IRT.itemfit(good_items_2pl) # RMSD item fit
good_items_rmsd$RMSD
# summary(good_items_fit)
# summary(good_items_rmsd)

# Information Criteria to evaluate model fit
anova(good_items_r, good_items_2pl)
good_items_r$ic
good_items_2pl$ic

# local dependence: aQ3 correlations ----
good_items_res <- TAM::tam.modelfit(good_items_2pl)
good_items_res$Q3_summary
# subscale_res$Q3.matr

# Local dependence was quantified using the adjusted Q3 
# (aQ3; Marais, 2013) statistic, a bias-corrected form of the 
# traditional Q3 statistic (Yen, 1984). A cut-off of |.25| was 
# used to flag the most notably dependent item pairs 
# (Christensen et al., 2017).
round(good_items_res$aQ3.matr, 3)
# There are two correlations which are slightly above 0.25.

# DIF via lordif----

# This is just a test that now is useless.
demo_info$severe_illness <- ifelse(demo_info$`LESIONE/MALATTIA GRAVE` == 1, 1, 0)
demo_info$first_aid <- ifelse(demo_info$`NUMERO ACCESSI PS` == 0, 0, 1)

# good_items_dif <- lordif(
#   good_data
#   , group = demo_info$first_aid
#   , model = "GPCM" # a generalized partial credit model
#   , criterion = "R2"
#   , pseudo.R2 = "McFadden" # this is the criterion
#   , R2.change =.01 # this is the % R2 to use as a flagging threshold
#   , maxIter = 500
#   , minCell = 10
# )
# good_items_dif

effectsize::cohens_d(rowSums(good_data), demo_info$severe_illness)
effectsize::cohens_d(rowSums(good_data), demo_info$first_aid)


# plot(demo_info$`NUMERO ACCESSI PS`, rowSums(good_data))
# m <- lm(rowSums(good_data) ~ demo_info$`NUMERO ACCESSI PS`)
# abline(m)
# summary(m)


psych::describe(good_data)

psych::alpha(good_data)

# parallel analysis
spar <- fa.parallel(
  good_data, fm="ml", fa="fa", sim=FALSE,
  error.bars=TRUE, se.bars=FALSE, n.iter=100
)

eigen(cov(good_data))$values[1] / eigen(cov(good_data))$values[2]

# Distribution of scale responses for all 22 items in the Area  
# "Caratteristiche del bambino".
n_cols <- length(good_items)
plot_ridges(good_data, ncolumns = n_cols)


plot_item_difficulty(good_items_2pl, 5) 
  


# -------------------------------------------------------------------
# AREA 2: Richieste di caregiving

item_subscale <- c(
  "i164", "i180", "i154", "i162", "i105", "i40", "i196", "i8", 
  "i134", "i131", "i43", "i187", "i136", "i54", "i181", "i50", 
  "i48", "i25", "i133", "i11", "i33", "i129", "i169", "i46", 
  "i174", "i73", "i127", "i39", "i112", "i103", "i51", "i53", 
  "i58", "i32")


# (1) Check for poor item fit (Infit, Outfit, RMSD)

# outfit
get_2pl_indices(subscale_data)[[1]]

# infit
get_2pl_indices(subscale_data)[[2]]

# rmsd
get_2pl_indices(subscale_data)[[3]]

# (2) Check for local dependence (aQ3)

local_dependence(subscale_data)

# (3) Look for unreasonable difficulty levels (IRT b)

# alpha
irt_ab(subscale_data)[[1]]

# beta
irt_ab(subscale_data)[[2]]

# IIF
plot_iif(subscale_data) 

# selected items that conveyed higher information along 
# the trait continuum 
good_items <- c("i25", "i105", "i129", "i133") 

subscale_data <- complete_data %>% 
  dplyr::select(all_of(good_items))
dim(subscale_data)


# We also checked the fit indeces of a CFA model.
one_factor_model <-  '
  F1 =~ i25 + i105 + i129 + i133
'

fit <- lavaan:::cfa(
  one_factor_model,
  data = subscale_data,
  ordered = names(good_data),
  std.lv = TRUE
)

fitMeasures(
  fit, c("chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
         "rmsea", "rmsea.scaled", "srmr")
)

gof_indices_cfa(good_data, one_factor_model)

# outfit
get_2pl_indices(good_data)[[1]]

# infit
get_2pl_indices(good_data)[[2]]

# rmsd
get_2pl_indices(good_data)[[3]]

# (2) Check for local dependence (aQ3)

local_dependence(good_data)

# (3) Look for unreasonable difficulty levels (IRT b)

# alpha
irt_ab(good_data)[[1]]

# beta
irt_ab(good_data)[[2]]

# IIF
plot_iif(good_data) 

# TIF
plot_tif(good_data)
# plot_tif(subscale_data)

# SE
plot_se(good_data)


# -------------------------------------------------------------------
# AREA 3: Cura formale

item_cura <- c("i156", "i67", "i176", "i28", "i149", 
               "i35", "i192", "i87", "i143", "i90", "i160", "i69", 
               "i170", "i79", "i24", "i76", "i114", "i74", "i92",
               "i163")
new_data <- complete_data[, (names(complete_data) %in% item_cura)]

n_item <- length(names(new_data))
mod <- mirt(data=new_data, 1, itemtype="graded", SE=TRUE, verbose=FALSE)

n_item <- length(names(new_data))
plot(mod, type = 'trace', which.items = 1:n_item, 
     main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4)) 

plot(mod, type = 'infotrace', which.items = 1:n_item, 
     main = "", par.settings = simpleTheme(lwd=2))


good_items <- c("i192", "i143", "i156", "i87", "i79", "i69", "i92")
good_data <- new_data[, names(new_data) %in% good_items]

onef_model <-  '
  F1 =~ i192 + i143 + i156 + i87 + i79 + i69 + i92
'

fit <- lavaan:::cfa(
  onef_model, 
  data = good_data,  
  ordered = names(good_data),
  std.lv = TRUE
)

# summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitMeasures(
  fit, 
  c("chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
    "rmsea", "rmsea.scaled", "srmr")
)

# Rasch model
rref <- TAM::tam.mml(
  good_data, irtmodel = "PCM", 
  control = list(Msteps=10, QMC=FALSE, snodes=0, convD=.0001, 
                 conv=.00001, convM=.00001)
)

# 2PL IRT Generalized Partial Credit Model
ref2pl <- TAM::tam.mml.2pl(
  good_data, irtmodel = "GPCM", 
  control = list(Msteps=10, QMC=FALSE, snodes=0,
                 convD=.0001, conv=.00001, convM=.00001)
)

summary(ref2pl)

anova(rref, ref2pl)
rref$ic
ref2pl$ic

subscale_res <- TAM::tam.modelfit(rref)
subscale_res$Q3_summary

subscale_res$Q3.matr

round(subscale_res$aQ3.matr, 3)

psych::describe(good_data)

psych::alpha(good_data)

# parallel analysis
spar <- fa.parallel(
  good_data, fm="ml", fa="fa", sim=FALSE,
  error.bars=TRUE, se.bars=FALSE, n.iter=100
)

eigen(cov(good_data))$values[1] / eigen(cov(good_data))$values[2]



# -------------------------------------------------------------------
# AREA 4: Fattori intrapsichici

item_intrapsi <- c("i177"
, "i191"
, "i122"
, "i62"
, "i104"
, "i42"
, "i111"
, "i64"
, "i34"
, "i75"
, "i65"
, "i108"
, "i119"
, "i138"
, "i116"
, "i20"
, "i145"
, "i59"
, "i118"
, "i173"
, "i23"
, "i45"
, "i179"
, "i41"
, "i139"
, "i4"
, "i56"
, "i166"
, "i193"
, "i83"
, "i21"
, "i93")


new_data <- complete_data[, (names(complete_data) %in% item_intrapsi)]

n_item <- length(names(new_data))
mod <- mirt(data=new_data, 1, itemtype="graded", SE=TRUE, verbose=FALSE)

n_item <- length(names(new_data))
plot(mod, type = 'trace', which.items = 1:n_item, 
     main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4)) 

plot(mod, type = 'infotrace', which.items = 1:n_item, 
     main = "", par.settings = simpleTheme(lwd=2))


good_items <- c("i177", "i191", "i62", "i64", "i65")
good_data <- new_data[, names(new_data) %in% good_items]

onef_model <-  '
  F1 =~ i177 + i191 + i62 + i64 + i65 
'

fit <- lavaan:::cfa(
  onef_model, 
  data = good_data,  
  ordered = names(good_data),
  std.lv = TRUE
)

# summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitMeasures(
  fit, 
  c("chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
    "rmsea", "rmsea.scaled", "srmr")
)

# Rasch model
rref <- TAM::tam.mml(
  good_data, irtmodel = "PCM", 
  control = list(Msteps=10, QMC=FALSE, snodes=0, convD=.0001, 
                 conv=.00001, convM=.00001)
)

# 2PL IRT Generalized Partial Credit Model
ref2pl <- TAM::tam.mml.2pl(
  good_data, irtmodel = "GPCM", 
  control = list(Msteps=10, QMC=FALSE, snodes=0,
                 convD=.0001, conv=.00001, convM=.00001)
)

summary(ref2pl)

anova(rref, ref2pl)
rref$ic
ref2pl$ic

subscale_res <- TAM::tam.modelfit(rref)
subscale_res$Q3_summary

subscale_res$Q3.matr

round(subscale_res$aQ3.matr, 3)

psych::describe(good_data)

psych::alpha(good_data)

# parallel analysis
spar <- fa.parallel(
  good_data, fm="ml", fa="fa", sim=FALSE,
  error.bars=TRUE, se.bars=FALSE, n.iter=100
)

eigen(cov(good_data))$values[1] / eigen(cov(good_data))$values[2]


# -------------------------------------------------------------------
# AREA 5: Coping

item_coping <- c(
"i98"
, "i125"
, "i186"
, "i130"
, "i44"
, "i172"
, "i195"
, "i66"
, "i190"
, "i18"
, "i94"
, "i80"
, "i117"
, "i100"
, "i38"
, "i36"
, "i97"
, "i197"
, "i189"
, "i185"
, "i88"
, "i107"
, "i30"
, "i19"
, "i161"
, "i128"
, "i22"
, "i12"
, "i52"
, "i95"
, "i68"
, "i148"
, "i140"
, "i71"
, "i72"
, "i142"
, "i70"
, "i141"
, "i15"
, "i115"
, "i120"
, "i7"
, "i102"
, "i178"
, "i16"
, "i89"
, "i10"
, "i6"
, "i155"
, "i82"
, "i113"
, "i3"
, "i146"
, "i29"
, "i157"
, "i26"
, "i61"
)

 
new_data <- complete_data[, (names(complete_data) %in% item_coping)]

n_item <- length(names(new_data))
mod <- mirt(data=new_data, 1, itemtype="graded", SE=TRUE, verbose=FALSE)

n_item <- length(names(new_data))
plot(mod, type = 'trace', which.items = 1:n_item, 
     main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4)) 

plot(mod, type = 'infotrace', which.items = 1:n_item, 
     main = "", par.settings = simpleTheme(lwd=2))


good_items <- c("i195", "i161", "i178", 
                "i128", "i115", "i66", "i7")
good_data <- new_data[, names(new_data) %in% good_items]

onef_model <-  '
  F1 =~ i195 + i161 + i178 + i128 + i115 + i66 + i7
'

fit <- lavaan:::cfa(
  onef_model, 
  data = good_data,  
  ordered = names(good_data),
  std.lv = TRUE
)

# summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitMeasures(
  fit, 
  c("chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
    "rmsea", "rmsea.scaled", "srmr")
)

# Rasch model
rref <- TAM::tam.mml(
  good_data, irtmodel = "PCM", 
  control = list(Msteps=10, QMC=FALSE, snodes=0, convD=.0001, 
                 conv=.00001, convM=.00001)
)

# 2PL IRT Generalized Partial Credit Model
ref2pl <- TAM::tam.mml.2pl(
  good_data, irtmodel = "GPCM", 
  control = list(Msteps=10, QMC=FALSE, snodes=0,
                 convD=.0001, conv=.00001, convM=.00001)
)

summary(ref2pl)

anova(rref, ref2pl)
rref$ic
ref2pl$ic

subscale_res <- TAM::tam.modelfit(rref)
subscale_res$Q3_summary

subscale_res$Q3.matr

round(subscale_res$aQ3.matr, 3)

psych::describe(good_data)

psych::alpha(good_data)

# parallel analysis
spar <- fa.parallel(
  good_data, fm="ml", fa="fa", sim=FALSE,
  error.bars=TRUE, se.bars=FALSE, n.iter=100
)

eigen(cov(good_data))$values[1] / eigen(cov(good_data))$values[2]




# -------------------------------------------------------------------
# AREA 6: Iperprotezione

item_iperprot <- c("i9", "i153", "i2", "i137", "i96", "i165", 
                    "i194", "i13", "i14", "i101", "i144", "i31", "i151", 
                    "i159", "i182", "i27")

new_data <- complete_data[, (names(complete_data) %in% item_iperprot)]

n_item <- length(names(new_data))
mod <- mirt(data=new_data, 1, itemtype="graded", SE=TRUE, verbose=FALSE)

n_item <- length(names(new_data))
plot(mod, type = 'trace', which.items = 1:n_item, 
     main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4)) 

plot(mod, type = 'infotrace', which.items = 1:n_item, 
     main = "", par.settings = simpleTheme(lwd=2))

good_items <- c("i159", "i96", "i137", "i101", "i31")
good_data <- new_data[, names(new_data) %in% good_items]

onef_model <-  '
  F1 =~ i159 + i96 + i137 + i101 + i31
'

fit <- lavaan:::cfa(
  onef_model, 
  data = good_data,  
  ordered = names(good_data),
  std.lv = TRUE
)

# summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitMeasures(
  fit, 
  c("chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
    "rmsea", "rmsea.scaled", "srmr")
)

# Rasch model
rref <- TAM::tam.mml(
  good_data, irtmodel = "PCM", 
  control = list(Msteps=10, QMC=FALSE, snodes=0, convD=.0001, 
                 conv=.00001, convM=.00001)
)

# 2PL IRT Generalized Partial Credit Model
ref2pl <- TAM::tam.mml.2pl(
  good_data, irtmodel = "GPCM", 
  control = list(Msteps=10, QMC=FALSE, snodes=0,
                 convD=.0001, conv=.00001, convM=.00001)
)

summary(ref2pl)

anova(rref, ref2pl)
rref$ic
ref2pl$ic

subscale_res <- TAM::tam.modelfit(rref)
subscale_res$Q3_summary

subscale_res$Q3.matr

round(subscale_res$aQ3.matr, 3)

psych::describe(good_data)

psych::alpha(good_data)

# parallel analysis
spar <- fa.parallel(
  good_data, fm="ml", fa="fa", sim=FALSE,
  error.bars=TRUE, se.bars=FALSE, n.iter=100
)

eigen(cov(good_data))$values[1] / eigen(cov(good_data))$values[2]


# -------------------------------------------------------------------
# AREA 7: Fratelli

item_cura <- c("i188", "i17", "i184", "i147", "i109", "i37", 
               "i171", "i78", "i126", "i84", "i55", "i110", "i123", 
               "i150", "i158")
new_data <- complete_data[, (names(complete_data) %in% item_cura)]

n_item <- length(names(new_data))
mod <- mirt(data=new_data, 1, itemtype="graded", SE=TRUE, verbose=FALSE)

n_item <- length(names(new_data))
plot(mod, type = 'trace', which.items = 1:n_item, 
     main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4)) 

plot(mod, type = 'infotrace', which.items = 1:n_item, 
     main = "", par.settings = simpleTheme(lwd=2))

good_items <- c("i171", "i126", "i17", "i147", "i110")
good_data <- new_data[, names(new_data) %in% good_items]

onef_model <-  '
  F1 =~ i171 + i126 + i17 + i147 + i110
'

fit <- lavaan:::cfa(
  onef_model, 
  data = good_data,  
  ordered = names(good_data),
  std.lv = TRUE
)

# summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitMeasures(
  fit, 
  c("chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
    "rmsea", "rmsea.scaled", "srmr")
)

# Rasch model
rref <- TAM::tam.mml(
  good_data, irtmodel = "PCM", 
  control = list(Msteps=10, QMC=FALSE, snodes=0, convD=.0001, 
                 conv=.00001, convM=.00001)
)

# 2PL IRT Generalized Partial Credit Model
ref2pl <- TAM::tam.mml.2pl(
  good_data, irtmodel = "GPCM", 
  control = list(Msteps=10, QMC=FALSE, snodes=0,
                 convD=.0001, conv=.00001, convM=.00001)
)

summary(ref2pl)

anova(rref, ref2pl)
rref$ic
ref2pl$ic

subscale_res <- TAM::tam.modelfit(rref)
subscale_res$Q3_summary

subscale_res$Q3.matr

round(subscale_res$aQ3.matr, 3)

psych::describe(good_data)

psych::alpha(good_data)

# parallel analysis
spar <- fa.parallel(
  good_data, fm="ml", fa="fa", sim=FALSE,
  error.bars=TRUE, se.bars=FALSE, n.iter=100
)

eigen(cov(good_data))$values[1] / eigen(cov(good_data))$values[2]


# -------------------------------------------------------------------

# 

selected_items <- c(
  "i124", "i106", "i60", "i49",  "i83",
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

final_data <- complete_data %>% 
  dplyr::select(selected_items)


fai_model <-  '
  F1 =~ i124 + i106 + i60 + i49 + i83
  F2 =~ i25 + i129 + i105 + i133
  F3 =~ i192 + i143 + i156 + i87 + i79 + i69 + i92
  F4 =~ i177 + i191 + i62 + i64 + i119 
  F5 =~ i195 + i161 + i178 + i128 + i115 + i66 + i7
  F6 =~ i159 + i96 + i137 + i101 + i151
'

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


# summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitMeasures(
  fit, 
  c("chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
    "rmsea", "rmsea.scaled", "srmr")
)

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
  kaiser$MSA <0.50 ~ "Unacceptable"
)

Bart_KMO <- data.frame(
  KMO = round(kaiser$MSA, 3), 
  Interpretation_KMO = interpretation_KMO,
  Bartlett_Chi = bart$chisq,
  Bartlett_df = bart$df,
  Bartlett_sig = sprintf("%.3f",bart$p.value)
)
Bart_KMO

#Scree Plot for determine number of factors
eigenvalues <- nFactors::eigenComputes(x = final_data) 
eigen_for_graph <- data.frame(item_number = 1:ncol(final_data), eigenvalues) 

scree_plot <- ggplot(data = eigen_for_graph) +
  geom_point(aes(x = item_number, y = eigenvalues )) + 
  geom_line(aes(x = item_number, y = eigenvalues )) + 
  xlab("Factor Number") +
  ylab ("Eigenvalues") +
  papaja::theme_apa() +
  scale_x_continuous(breaks = seq(from = 1, to = ncol(final_data), by = 1))
scree_plot

#MAP analysis for examine number of dimensions
map_analysis <- psych::vss(final_data, n = (ncol(final_data) - 1)) 
map_factors <- which(map_analysis$map == min(map_analysis$map))
#Parallel analysis for examine number of dimensions
#Conduct Parallel analysis with Pearson Correlation Matrix 
PA_pearson <- psych::fa.parallel(final_data, fa = "both", cor = "cor")
#Conduct Parallel Analysis with Polychoric Correlation Matrix
PA_poly <- psych::fa.parallel(final_data, fa = "both", cor = "poly")
results_factor_retentation <- list(MAP_Result = map_factors, Parallel_Analysis_Pearson = PA_pearson$nfact,
                                   Parallel_Analysis_Polychoric = PA_poly$nfact,
                                   Scree_Plot = "Look at the Plots Section for Scree Plot", 
                                   scree_plot)

results_factor_retentation

alpha(final_data)
splitHalf(final_data)

scree(final_data, factors = FALSE)

tot_score <- rowSums(final_data)
plot(density(tot_score, na.rm = TRUE), 
     main = "Total score")
describe(final_data)
error.dots(final_data)
error.bars(final_data)





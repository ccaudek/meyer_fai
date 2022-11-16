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
})

library("here")

# Increase max print
options(max.print = .Machine$integer.max)

source(here("libraries", "fai_funs.R"))

# Read data
fai_s <- read_xlsx(
  here("data", "raw", "FAI_TOT_2020_corrected.xlsx"), 
  col_names = TRUE
)

demo_info <- recode_demo_info(fai_s)


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

d_clean$death_risk_f <- factor(d_clean$death_risk)
d_clean$has_emergency_care <- ifelse(
  d_clean$has_emergency_care == 0, NA, (d_clean$has_emergency_care)
)
d_clean$has_emergency_care_f <- factor(d_clean$has_emergency_care)


# Multiple imputation

temp <- d_clean %>% 
  dplyr::select(starts_with("i")) %>% 
  dplyr::select_if(is.numeric)

imp <- mice::mice(temp, method = "norm.predict", m = 1) 
df <- round(complete(imp))



# AREA 1: Caratteristiche bambino ----

source(here("scripts", "areas", "area_1.R"))

# severity of pathology
severity_items <- c("i124", "i106", "i60", "i49",  "i83")







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





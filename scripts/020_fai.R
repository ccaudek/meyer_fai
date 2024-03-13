# Script name: 020_fai.R
# Project: project
# Script purpose: selected items
# @author: Corrado Caudek <corrado.caudek@unifi.it>
# Date Created: Fri Nov 18 13:48:49 2022
# Last Modified Date: Fri Nov 18 13:48:49 2022
# 
# Notes: 


library("here")
library("tidyverse")
library("TAM")
library("mirt")
library("lavaan")
library("mokken")
library("psych")
library("gridExtra")
library("grid")
library("semTools")
library("ids")

source(here::here("functions", "fai_funs.R"))

df_tot <- readRDS(
  here("data", "processed", "fai_2022_11_20.rds")
)
# Add random idx for each participant.
df_tot$idx <- ids::random_id(nrow(df_tot), 4)

# Remove carless responding participants.
temp <- df_tot |> 
  dplyr::filter(FLAG == "keep")
temp$FLAG <- NULL

good_participants <- temp$idx

# All items were first examined for normality, given recommendations that 
# item skewness values should not exceed 2 to 3 and that kurtosis values 
# should not exceed 7 to 8 (Curran et al., 1996; Finney & DiStefano, 2006).
# Get only FAI items.
items_df <- temp %>% 
  dplyr::select(starts_with("FAI_"))

items_stats <- psych::describe(items_df)

items_skew_kurt_bad <- items_stats |> 
  dplyr::filter(skew > 2.5 | kurtosis > 7.5) |> 
  row.names()

df <- items_df |> 
  dplyr::select(!any_of(items_skew_kurt_bad))

area1 <- c("FAI_124", "FAI_106" , "FAI_60"  , "FAI_49")
area2 <- c("FAI_129", "FAI_133" , "FAI_25"  , "FAI_103")
area3 <- c("FAI_87",  "FAI_79"  , "FAI_143" , "FAI_192" , "FAI_156", "FAI_90", "FAI_114")
area4 <- c("FAI_111", "FAI_104" , "FAI_42"  , "FAI_119" , "FAI_93")
area5 <- c("FAI_128", "FAI_115" , "FAI_7"   , "FAI_155" , "FAI_195", "FAI_80", "FAI_36", "FAI_16")
area6 <- c("FAI_101", "FAI_159" , "FAI_137" , "FAI_96")

# Area 2 items names.
item_subscales <- c(area1, area2, area3, area4, area5, area6)

# Select only the items of this subscale.
fai_data <- df %>% 
  dplyr::select(all_of(item_subscales))
dim(fai_data)


# =========================================================================== #
#                       Step 4: Factor analysis                               #
# =========================================================================== #

# Test sampling adequacy
psych::KMO(fai_data)
psych::cortest.bartlett(fai_data)

# Factor analysis via parallel analysis
fa.parallel(fai_data, cor = "poly")

# Very simple structure analysis
vss(fai_data, 3)

# SEM
fai_model <-  '
  F1 =~ FAI_124 + FAI_106 + FAI_60 + FAI_49
  F2 =~ FAI_129 + FAI_133 + FAI_25 + FAI_103
  F3 =~ FAI_87 + FAI_79 + FAI_143 + FAI_192 + FAI_156 + FAI_90 + FAI_114
  F4 =~ FAI_111 + FAI_104 + FAI_42 + FAI_119 + FAI_93
  F5 =~ FAI_128 + FAI_115 + FAI_7 + FAI_195 + FAI_80 
  F6 =~ FAI_101 + FAI_159 + FAI_137 + FAI_96
'

fai_model <-  '
  F1 =~ FAI_124 + FAI_106 + FAI_60 + FAI_49
  F2 =~ FAI_129 + FAI_133 + FAI_25 + FAI_103
  F3 =~ FAI_87 + FAI_79 + FAI_143 + FAI_192 
  F4 =~ FAI_111 + FAI_104 + FAI_42 + FAI_119
  F5 =~ FAI_128 + FAI_115 + FAI_7 + FAI_155 
  F6 =~ FAI_101 + FAI_159 + FAI_137 + FAI_96
'

fit <- lavaan:::cfa(
  fai_model,
  data = fai_data,
  ordered = names(fai_data),
  std.lv = TRUE
)

# summary(fit, standardized = TRUE)
fitMeasures(
  fit, c("chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
         "rmsea", "rmsea.scaled", "srmr")
)
# The fit measures are excellent.


# Model summary
summary(fit, standardized = T, fit.measures = T)

# Coefficients only
coef(fit)

# CFA diagram from psych package
lavaan.diagram(fit, errors = T)

# Correlations matrix
qgraph::qgraph(cor(fai_data), layout = "spring", labels = colnames(fai_data))

# Partial correlations matrix
qgraph::qgraph(cor(fai_data), layout = "spring", labels = colnames(fai_data), graph = "pcor") 

eigen(cov(fai_data))$values[1] / eigen(cov(fai_data))$values[2]

psych::describe(fai_data)
psych::alpha(fai_data)

# parallel analysis
spar <- fa.parallel(
  fai_data, fm="ml", fa="fa", sim=FALSE,
  error.bars=TRUE, se.bars=FALSE, n.iter=100
)


# =========================================================================== #
#           Step 5: Classical Test Theory (CTT)                               #
# =========================================================================== #

# Calculate Cronbach alpha, beta, omega, and split half reliability
summary(psych::alpha(fai_data))
splitHalf(fai_data)
reliability(fit)

# Find Cronbach alpha if an item is dropped 
sjPlot::tab_itemscale(fai_data)


# =========================================================================== #
#           Step 6: Total (sub)scale scores                                   #
# =========================================================================== #

# Average scale scores
kept_items$area2_mean <- rowMeans(fai_data)

# Scale mean, SD, skewness, kurtosis
psych::describe(fai_data)
Hmisc::describe(fai_data)
skimr::skim(fai_data)

# Examine frequencies & other descriptives
sjPlot::tab_itemscale(fai_data)


#############################################################################

demo_info <- temp[, 1:54]

fai_df <- bind_cols(demo_info, fai_data)


a1_df <- fai_df %>% 
  dplyr::select(
    all_of(c("FAI_124", "FAI_106", "FAI_60", "FAI_49"))
  ) %>% 
  mutate(
    fai_a1 = rowSums(.)
  )

a2_df <- fai_df %>% 
  dplyr::select(
    all_of(c("FAI_129", "FAI_133", "FAI_25", "FAI_103"))
  ) %>% 
  mutate(
    fai_a2 = rowSums(.)
  )

a3_df <- fai_df %>% 
  dplyr::select(
    all_of(c("FAI_87", "FAI_79", "FAI_143", "FAI_192"))
  ) %>% 
  mutate(
    fai_a3 = rowSums(.)
  )

a4_df <- fai_df %>% 
  dplyr::select(
    all_of(c("FAI_111", "FAI_104", "FAI_42", "FAI_119"))
  ) %>% 
  mutate(
    fai_a4 = rowSums(.)
  )

a5_df <- fai_df %>% 
  dplyr::select(
    all_of(c("FAI_128", "FAI_115", "FAI_7", "FAI_155"))
  ) %>% 
  mutate(
    fai_a5 = rowSums(.)
  )

a6_df <- fai_df %>% 
  dplyr::select(
    all_of(c("FAI_101", "FAI_159", "FAI_137", "FAI_96"))
  ) %>% 
  mutate(
    fai_a6 = rowSums(.)
  )

fai_df$fai_a1 <- a1_df$fai_a1
fai_df$fai_a2 <- a2_df$fai_a2
fai_df$fai_a3 <- a3_df$fai_a3
fai_df$fai_a4 <- a4_df$fai_a4
fai_df$fai_a5 <- a5_df$fai_a5
fai_df$fai_a6 <- a6_df$fai_a6

fai_df$fai_ts <- with(
  fai_df,
  fai_a1 + fai_a2 + fai_a3 + fai_a4 + fai_a5 + fai_a6
)

fai_df %>% 
  group_by(death_risk) %>% 
  summarize(
    fai_ts = mean(fai_ts),
    n = n()
  )

fm <- lm(fai_ts ~ mother_marital_status + father_marital_status +
           child_age + has_chronic_disease +
           has_other_problems + has_other_chronic_disease +
           has_psychological_problems + hospitalization_number + 
           emergency_care_number + mother_education +
           father_education + 
           is_mother_italian + is_father_italian + is_child_italian +
           is_mother_working + 
           is_father_working +
           siblings + life_events + death_risk,
         data = fai_df)

fm <- lm(fai_ts ~ mother_marital_status + father_marital_status +
           child_age + has_chronic_disease +
           has_other_problems +
           has_psychological_problems + hospitalization_number + 
           emergency_care_number + 
           is_mother_italian + is_father_italian + is_child_italian +
           is_mother_working + 
           is_father_working +
           death_risk,
         data = fai_df)

car::Anova(fm)
summary(fm)


effectsize::cohens_d(fai_ts ~ death_risk, data = fai_df)
effectsize::cohens_d(fai_ts ~ has_other_problems, data = fai_df)
effectsize::cohens_d(fai_ts ~ is_father_italian, data = fai_df)
effectsize::cohens_d(fai_ts ~ is_mother_working, data = fai_df)

mymodel <- lm(fai_ts ~  has_other_problems +
                        has_psychological_problems + hospitalization_number + 
                        emergency_care_number + 
                        is_mother_italian + is_father_italian + is_child_italian +
                        is_mother_working + 
                        is_father_working +
                        death_risk, 
                      data = fai_df)


relative_importance <- relaimpo::calc.relimp(mymodel, type="lmg")$lmg

df = data.frame(
  variable=names(relative_importance),
  importance=round(c(relative_importance) * 100,2)
)

ggplot(df, aes(x = reorder(variable, -importance), y = importance)) +
  geom_col(fill = "deepskyblue4") + 
  geom_text(aes(label=importance), vjust=.3, hjust=1.2, size=3, color="white")+
  coord_flip() +
  labs(title = "Relative importance of variables") +
  theme_classic(base_size = 16)


fm1 <- lm(fai_a1 ~ mother_marital_status + father_marital_status +
           child_age + has_chronic_disease +
           has_other_problems + has_other_chronic_disease +
           has_psychological_problems + hospitalization_number + 
           emergency_care_number + mother_education +
           father_education + 
           is_mother_italian + is_father_italian + is_child_italian +
           is_mother_working + 
           is_father_working +
           siblings + life_events + death_risk,
         data = fai_df)

summary(fm1)

fm2 <- lm(fai_a2 ~ mother_marital_status + father_marital_status +
            child_age + has_chronic_disease +
            has_other_problems + has_other_chronic_disease +
            has_psychological_problems + hospitalization_number + 
            emergency_care_number + mother_education +
            father_education + 
            is_mother_italian + is_father_italian + is_child_italian +
            is_mother_working + 
            is_father_working +
            siblings + life_events + death_risk,
          data = fai_df)

summary(fm2)

fm3 <- lm(fai_a3 ~ mother_marital_status + father_marital_status +
            child_age + has_chronic_disease +
            has_other_problems + has_other_chronic_disease +
            has_psychological_problems + hospitalization_number + 
            emergency_care_number + mother_education +
            father_education + 
            is_mother_italian + is_father_italian + is_child_italian +
            is_mother_working + 
            is_father_working +
            siblings + life_events + death_risk,
          data = fai_df)

summary(fm3)

fm4 <- lm(fai_a4 ~ mother_marital_status + father_marital_status +
            child_age + has_chronic_disease +
            has_other_problems + has_other_chronic_disease +
            has_psychological_problems + hospitalization_number + 
            emergency_care_number + mother_education +
            father_education + 
            is_mother_italian + is_father_italian + is_child_italian +
            is_mother_working + 
            is_father_working +
            siblings + life_events + death_risk,
          data = fai_df)

summary(fm4)

fm5 <- lm(fai_a5 ~ mother_marital_status + father_marital_status +
            child_age + has_chronic_disease +
            has_other_problems + has_other_chronic_disease +
            has_psychological_problems + hospitalization_number + 
            emergency_care_number + mother_education +
            father_education + 
            is_mother_italian + is_father_italian + is_child_italian +
            is_mother_working + 
            is_father_working +
            siblings + life_events + death_risk,
          data = fai_df)

summary(fm5)

fm6 <- lm(fai_a6 ~ mother_marital_status + father_marital_status +
            child_age + has_chronic_disease +
            has_other_problems + has_other_chronic_disease +
            has_psychological_problems + hospitalization_number + 
            emergency_care_number + mother_education +
            father_education + 
            is_mother_italian + is_father_italian + is_child_italian +
            is_mother_working + 
            is_father_working +
            siblings + life_events + death_risk,
          data = fai_df)

summary(fm6)


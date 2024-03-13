## ------------------------------------------------------------------
## 20_lpa.R
## 
## Project: FAI Careggi
## Purpose: Latent Profile Analysis
## Author: Corrado Caudek
## Date: "Wed Dec 23 07:49:52 2020"
## ------------------------------------------------------------------

library("tidyverse")
library("tidyr")
library("psych")
library("papaja")
library("khroma")
library("mice")
library("lavaan")
library("tidyLPA")
library("readxl")
library("careless")
library("mclust")
library("here")
library("reshape2")


options(max.print=1000000)


# Read data ----
fai <- read_xlsx(here("data", "raw", "FAI_TOT_2020.xlsx"), col_names = TRUE)

df_items <- fai %>%
  select(starts_with("FAI_")) %>%
  single_imputation() %>%
  data.frame()

df_others <- fai %>%
  select(-starts_with("FAI_")) %>%
  data.frame()

df <- cbind(df_others, df_items)




tot_score <- rowSums(df_items)

fm <- lm(log(tot_score) ~ factor(group))
summary(fm)
car::Anova(fm)

dat <- data.frame(group, tot_score) 
dat$group <- factor(dat$group)

dat %>% 
  ggplot(aes(x=group, y=log(tot_score), fill=group)) + 
  geom_boxplot()



# df_items <- fai %>%
#   select(starts_with("FAI_")) %>%
#   single_imputation() %>%
#   data.frame() %>% 
#   mutate(md = outlier(., plot = FALSE))
# 
# cutoff <- (qchisq(p = 1 - .001, df = ncol(df_items)))
# 
# df_clean <- df_items %>%
#   filter(md < cutoff) %>%
#   select(-md)
# 
# df_clustering <- df_clean %>%
#   na.omit() %>%
#   mutate_all(list(scale))



# -------------------------------------------------------------------

# AREA 1: Caratteristiche bambino
selected_items <- c(
  "FAI_49", "FAI_175", "FAI_106", "FAI_60", "FAI_124",
  "FAI_86", "FAI_152", "FAI_1", "FAI_47", "FAI_121",
  "FAI_57", "FAI_167", "FAI_91", "FAI_99", "FAI_135",
  "FAI_63", "FAI_168", "FAI_5", "FAI_132",
  "FAI_85", "FAI_81", "FAI_83"
)


# AREA 2: Richieste di caregiving
selected_items <- c(
  "FAI_164",
  "FAI_180",
  "FAI_154",
  "FAI_162",
  "FAI_105",
  "FAI_40",
  "FAI_196",
  "FAI_8",
  "FAI_134",
  "FAI_131",
  "FAI_43",
  "FAI_187",
  "FAI_136",
  "FAI_54",
  "FAI_181",
  "FAI_50",
  "FAI_48",
  "FAI_25",
  "FAI_133",
  "FAI_11",
  "FAI_33",
  "FAI_129",
  "FAI_169",
  "FAI_46",
  "FAI_174",
  "FAI_73",
  "FAI_127",
  "FAI_39",
  "FAI_112",
  "FAI_103",
  "FAI_51",
  "FAI_53",
  "FAI_58",
  "FAI_32"
)


# AREA 3: Cura formale

selected_items <- c(
  "FAI_156", 
  "FAI_67", 
  "FAI_176", 
  "FAI_28", 
  "FAI_149", 
  "FAI_35", 
  "FAI_192", 
  "FAI_87", 
  "FAI_143", 
  "FAI_90", 
  "FAI_160", 
  "FAI_69", 
  "FAI_170", 
  "FAI_79", 
  "FAI_24", 
  "FAI_76", 
  "FAI_114", 
  "FAI_74", 
  "FAI_92",
  "FAI_163"
)


# AREA 4: fattori intrapsichici
selected_items <- c(
  "FAI_177",
  "FAI_191",
  "FAI_122",
  "FAI_62",
  "FAI_104",
  "FAI_42",
  "FAI_111",
  "FAI_64",
  "FAI_34",
  "FAI_75",
  "FAI_65",
  "FAI_108",
  "FAI_119",
  "FAI_138",
  "FAI_116",
  "FAI_20",
  "FAI_145",
  "FAI_59",
  "FAI_118",
  "FAI_173",
  "FAI_23",
  "FAI_45",
  "FAI_179",
  "FAI_41",
  "FAI_139",
  "FAI_4",
  "FAI_56",
  "FAI_166",
  "FAI_193",
  "FAI_83",
  "FAI_21",
  "FAI_93"
)


# AREA 5: Coping
selected_items <- c(
    "FAI_98"
  , "FAI_125"
  , "FAI_186"
  , "FAI_130"
  , "FAI_44"
  , "FAI_172"
  , "FAI_195"
  , "FAI_66"
  , "FAI_190"
  , "FAI_18"
  , "FAI_94"
  , "FAI_80"
  , "FAI_117"
  , "FAI_100"
  , "FAI_38"
  , "FAI_36"
  , "FAI_97"
  , "FAI_197"
  , "FAI_189"
  , "FAI_185"
  , "FAI_88"
  , "FAI_107"
  , "FAI_30"
  , "FAI_19"
  , "FAI_161"
  , "FAI_128"
  , "FAI_22"
  , "FAI_12"
  , "FAI_52"
  , "FAI_95"
  , "FAI_68"
  , "FAI_148"
  , "FAI_140"
  , "FAI_71"
  , "FAI_72"
  , "FAI_142"
  , "FAI_70"
  , "FAI_141"
  , "FAI_15"
  , "FAI_115"
  , "FAI_120"
  , "FAI_7"
  , "FAI_102"
  , "FAI_178"
  , "FAI_16"
  , "FAI_89"
  , "FAI_10"
  , "FAI_6"
  , "FAI_155"
  , "FAI_82"
  , "FAI_113"
  , "FAI_3"
  , "FAI_146"
  , "FAI_29"
  , "FAI_157"
  , "FAI_26"
  , "FAI_61"
)


# AREA 6: Iperprotezione
selected_items <- c(
  "FAI_9", 
  "FAI_153", 
  "FAI_2", 
  "FAI_137", 
  "FAI_96", 
  "FAI_165", 
  "FAI_194", 
  "FAI_13", 
  "FAI_14", 
  "FAI_101", 
  "FAI_144", 
  "FAI_31", 
  "FAI_151", 
  "FAI_159", 
  "FAI_182", 
  "FAI_27"
  )



# -------------------------------------------------------
# Gaussian finite mixture modeling fitted by EM algorithm 
# -------------------------------------------------------

df_area <- fai %>%
  select(all_of(selected_items)) %>%
  data.frame() 

imputed_data <- mice(
  df_area, 
  m = 1, 
  maxit = 1, 
  method = 'pmm', 
  seed = 123
)

df_complete <- complete(imputed_data, 1)
dim(df_complete)

df_clustering <- df_complete %>%
    na.omit() %>%
    mutate_all(list(scale))


# df_area <- fai %>%
#   select(all_of(selected_items)) %>%
#   data.frame() %>% 
#   mutate(md = outlier(., plot = FALSE))
#   
# cutoff <- (qchisq(p = 1 - .001, df = ncol(df_area)))
# 
# df_clean <- df_area %>%
#   filter(md < cutoff) %>%
#   select(-md)
# 
# df_clustering <- df_clean %>%
#   na.omit() %>%
#   mutate_all(list(scale))

BIC <- mclustBIC(df_clustering)
plot(BIC)
summary(BIC)
mod1 <- Mclust(df_clustering, modelNames = "VEI", G = 5, x = BIC)
summary(mod1)

ICL <- mclustICL(df_clustering)
plot(ICL)
summary(ICL)

mclustBootstrapLRT(df_clustering, modelName = "EEI")


mod1 <- Mclust(df_clustering, modelNames = "VEI", G = 5, x = BIC)
summary(mod1)

means <- data.frame(mod1$parameters$mean, stringsAsFactors = FALSE) %>%
  rownames_to_column() %>%
  rename(Interest = rowname) %>%
  melt(id.vars = "Interest", variable.name = "Profile", value.name = "Mean") %>%
  mutate(Mean = round(Mean, 2),
         Mean = ifelse(Mean > 1, 1, Mean))


means %>%
  ggplot(aes(Interest, Mean, group = Profile, color = Profile)) +
  geom_point(size = 2.25) +
  geom_line(size = 1.25) +
  scale_x_discrete(limits = all_of(selected_items)) +
  labs(x = NULL, y = "Standardized mean") +
  theme_apa() +
  #khroma::scale_colour_bright() +
  scale_colour_okabeito() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top") 
  


group <- mod1$class


table(group, df_others$LESIONE.MALATTIA.GRAVE)

chisq.test(group, df_others$LESIONE.MALATTIA.GRAVE)










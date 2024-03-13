# Script name: 015_area_5.R
# Project: project
# Script purpose: AREA 5: Coping
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

df_tot <- readRDS(
  here("data", "processed", "fai_2022_11_18.rds")
)

temp <- df_tot |> 
  dplyr::filter(FLAG == "keep")
temp$FLAG <- NULL

# All items were first examined for normality, given recommendations that 
# item skewness values should not exceed 2 to 3 and that kurtosis values 
# should not exceed 7 to 8 (Curran et al., 1996; Finney & DiStefano, 2006).
items_stats <- psych::describe(temp)

items_skew_kurt_bad <- items_stats |> 
  dplyr::filter(skew > 2.5 | kurtosis > 7.5) |> 
  row.names()

df <- temp |> 
  dplyr::select(!any_of(items_skew_kurt_bad))

# Area 2 items names.
item_subscale <- c(
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

# Keep only items without extreme skewness and kurtosis.
names_item_subscale_clean <- setdiff(item_subscale, items_skew_kurt_bad)

# Select subscale items without extreme skewness and kurtosis.
subscale_data <- df %>% 
  dplyr::select(all_of(names_item_subscale_clean))
dim(subscale_data)


# =========================================================================== #
#                     STEP 1: DESCRIPTIVE STATISTICS                          #
# =========================================================================== #

sjPlot::tab_itemscale(
  subscale_data[, 1:ncol(subscale_data)], 
  show.kurtosis = T, show.corr.matrix = T
)

# Write a function that creates a histograms for column 'x'
many_plots <- function(x) {
  g <- ggplot(df1, aes(subscale_data[, x])) +
    geom_bar(color='grey', fill='white', size = 1) +
    #coord_cartesian(xlim = c(0:7)) + 
    theme_classic() +
    geom_vline(
      aes(xintercept = mean(subscale_data[,x])), size = 1.5, 
      linetype = 'dashed', col = 'chartreuse1') +
    xlab(paste("Score |", "Item:", x)) 
  #scale_x_discrete(limits = c(0:5))
  print(g)
}

# Create an empty container list
plot_list <- list()
n_items <- ncol(subscale_data)
# Fill the container with plots
for(i in 1:n_items) {
  p <- many_plots(i)
  plot_list[[i]] <- p
}

# Save a figure with all plots
plots_scores <- gridExtra::grid.arrange(
  grobs = plot_list[1:n_items],
  ncol = 6,
  nrow = 6,
  bottom = textGrob(
    'Note: Dashed line depicts means',
    hjust = -2,
    gp = gpar(fontsize = 15, fontface = 'italic')
  )
)


# # Plot of the frequency distribution of the responses for each item.
# for(n in item_subscale) {
#   distr <- table(subscale_data[,n])
#   barplot(distr,  
#           main=n, 
#           col=gray.colors(20), 
#           ylab = "Number of respondents", 
#           xlab = "Response (1=Negative, 7=Positive)");
# }


bluesqs <- cor(subscale_data, method = "spearman")
# heat plot of correlations matrix 
# uncomment the png & devoff lines if you want to save as png in the working directory
# png('corplot.png')
corrplot::corrplot(bluesqs, method = 'ellipse', order = 'AOE', type = 'upper')

# check outliers in item sets
# d2mydata <- outliers::outlier(subscale_data)


# =========================================================================== #
#              Step 2: NON-PARAMETRIC ITEM RESPONSE THEORY (NIRT)             #
# =========================================================================== #

# Item properties - H coefficients and aisp

# Calculate H coefficients
coefs.h <- coefH(subscale_data)
coefs.h$Hi
coefs.h$H

# Save a list with H coefficients for each item and for the whole scale
coefs.h.all <- list(coefs.h[[2]], coefs.h[[3]])

# Automated Item Selection Procedure (AISP) for Mokken Scale Analysis.
# Remove items below aisp threshold of 0.3.
aisp.lb <- mokken::aisp(subscale_data, lowerbound = 0.3)
kept_items_1 <- subscale_data[, aisp.lb == 1]
names(kept_items_1)
coefH(kept_items_1)


# Check monotonicity, IIO, and local independence 

# Examine the monotonicity.
mc <- check.monotonicity(kept_items_1)
summary(mc)
plot(mc)

# Check of Invariant Item Ordering (IIO).
iio.list2 <- check.iio(kept_items_1, method="MIIO")
summary(iio.list2)
# Remove items violating IIO for the scale.
kept_items_2 <- 
  kept_items_1[
    , is.na(charmatch(dimnames(kept_items_1)[[2]], 
                      names(iio.list2$items.removed)))]
summary(check.iio(kept_items_2))

# Check local independence (conditional association)
ca <- mokken::check.ca(kept_items_2, TRUE)
# Subcomponents correspond to the iteration. The first subcomponent refers to 
# the situation with all items in the test, the second subcomponent refers to 
# the sitution with the worst item deleted, the third subcomponent refers to 
# the sitution with the two worst items deleted, etc.
ca$InScale
ca$Index
ca$Flagged

# Slect items satisfying local independence.
names_kept_loc_ind_items <- names(kept_items_2[ca$InScale[[6]]])

# Select items according to local dependence.
kept_items_3 <- kept_items_2 %>% 
  dplyr::select(all_of(names_kept_loc_ind_items))
dim(kept_items_3)


# # Outliers
# xPlus   <- rowSums(good_items)
# gPlus   <- mokken::check.errors(good_items)$Gplus
# hist(gPlus)
# oPlus   <- mokken::check.errors(good_items, TRUE, TRUE)$Oplus
# # cor(cbind(oPlus, gPlus, xPlus))
# 
# Q3 <- summary(gPlus)[[5]]
# IQR <- Q3 - summary(gPlus)[[2]]
# outlier <- gPlus > Q3 + 1.5 * IQR 


# =========================================================================== #
#              Step 3: PARAMETRIC ITEM RESPONSE THEORY (PIRT)                 #
# =========================================================================== #

# (1) Check for poor item fit (Infit, Outfit, RMSD), which flags 
# items which generate scores that are relatively poorly predicted 
# by the IRT model.

# Fit 2PL IRT Generalized Partial Credit Model
subscale_2pl <- TAM::tam.mml.2pl(
  kept_items_3,
  irtmodel = "GPCM",
  control = list(
    Msteps = 10, QMC = FALSE, snodes = 0,
    convD = .0001, conv = .00001, convM = .00001
  )
)
summary(subscale_2pl)

subscale_fit <- msq.itemfit(subscale_2pl) # mean square infit, outfit
subscale_fit$itemfit

outfit <- subscale_fit$itemfit[, c(1, 3)]
outfit$delta <- abs(outfit$Outfit - 1)
outfit %>% 
  arrange(delta)

subscale_rmsd <- IRT.itemfit(subscale_2pl) # RMSD item fit
subscale_rmsd$RMSD %>% 
  arrange(Group1)

# (2) Check for local dependence (aQ3), which impairs unidimensionality.
subscale_res <- TAM::tam.modelfit(subscale_2pl)
subscale_res$Q3_summary
# A cut-off of |.25| is used to flag the most notably dependent item pairs.
round(subscale_res$aQ3.matr, 3)

# (3) Look for unreasonable difficulty levels (IRT b), which are either 
# too hard or too easy for the target population.
summary(subscale_2pl)

# (4) Look for weak slope/discrimination values (IRT a), which indicates 
# that an item provides relatively little information for reliably 
# rank-ordering participants.
summary(subscale_2pl)

# To shorten the original item pool we selected the items that 
# offered more information by considering the shape of each item 
# information function, which display the amount of item information
# along the latent trait. 
mod <- mirt(
  data = kept_items_3,
  1,
  itemtype = "graded",
  SE = TRUE,
  verbose = FALSE
)

n_item <- length(names(kept_items_3))
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

# Selected the items that conveyed higher information along the trait 
# continuum.
names_kept_items <- c("FAI_128", "FAI_115", "FAI_7", "FAI_155", "FAI_195") 
# When considering the items content, it can be noted that the
# retained items addressed the key features of...
kept_items <- subscale_data |> 
  dplyr::select(
    all_of(names_kept_items)
  )
dim(kept_items)

# TAM IRT Models ----
# 0-4 items, as required by TAM

# Rasch model
good_items_r <- TAM::tam.mml(
  kept_items,
  irtmodel = "PCM",
  control = list(Msteps = 10, QMC = FALSE, snodes = 0, 
                 convD = .0001, conv = .00001, convM = .00001)
)
# summary(good_data)

# 2PL IRT Generalized Partial Credit Model
good_items_2pl <- TAM::tam.mml.2pl(
  kept_items,
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

# effectsize::cohens_d(rowSums(good_data), d_clean$death_risk_f)

# temp <- data.frame(area1 = rowSums(good_data), d_clean$has_emergency_care_f) %>% 
#   drop_na()
# effectsize::cohens_d(temp$area1, temp$has_emergency_care_f)


# effectsize::cohens_d(rowSums(good_data), demo_info$first_aid)
# plot(demo_info$`NUMERO ACCESSI PS`, rowSums(good_data))
# m <- lm(rowSums(good_data) ~ demo_info$`NUMERO ACCESSI PS`)
# abline(m)
# summary(m)


# =========================================================================== #
#                       Step 4: Factor analysis                               #
# =========================================================================== #

# Test sampling adequacy
psych::KMO(kept_items)
psych::cortest.bartlett(kept_items)

# Factor analysis via parallel analysis
fa.parallel(kept_items, cor = "poly")

# Very simple structure analysis
vss(kept_items, 3)

# Factor analysis
fa(kept_items, nfactors = 1, fm = "minres", n.iter=100)
fa(kept_items, nfactors = 1, fm = "pa", n.iter=100)

# Plot the fa solution
plot(fa(kept_items, nfactors = 1, fm = "minres"))

# Plot diagram fa solution
fa.diagram(fa(kept_items, nfactors = 1, fm = "minres"))

# Factor Miner package PCA
FactoMineR::PCA(kept_items)$eig

# Principal component analysis
principal(kept_items, 1, rotate = "varimax")

# Hierarchical cluster analysis using ICLUST (groups items)
iclust(kept_items, title = "ICLUST using Pearson correlations")

# SEM
# We also checked the fit indexes of a CFA model.
# FAI_87", "FAI_79", "FAI_143", "FAI_192", "FAI_156
one_factor_model <-  'F =~ FAI_128 + FAI_115 + FAI_7 + FAI_155 + FAI_195'

fit <- lavaan:::cfa(
  one_factor_model,
  data = kept_items,
  ordered = names(kept_items),
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
qgraph::qgraph(cor(kept_items), layout = "spring", labels = colnames(last_items))

# Partial correlations matrix
qgraph::qgraph(cor(kept_items), layout = "spring", labels = colnames(last_items), graph = "pcor") 

eigen(cov(kept_items))$values[1] / eigen(cov(kept_items))$values[2]

psych::describe(kept_items)
psych::alpha(kept_items)

# parallel analysis
spar <- fa.parallel(
  kept_items, fm="ml", fa="fa", sim=FALSE,
  error.bars=TRUE, se.bars=FALSE, n.iter=100
)


# =========================================================================== #
#           Step 5: Classical Test Theory (CTT)                               #
# =========================================================================== #

# Calculate Cronbach alpha, beta, omega, and split half reliability
summary(psych::alpha(kept_items))
splitHalf(kept_items)
reliability(fit)

# Find Cronbach alpha if an item is dropped 
sjPlot::tab_itemscale(kept_items)


# =========================================================================== #
#           Step 6: Total (sub)scale scores                                   #
# =========================================================================== #

# Average scale scores
kept_items$area2_mean <- rowMeans(kept_items)

# Scale mean, SD, skewness, kurtosis
psych::describe(kept_items)
Hmisc::describe(kept_items)
skimr::skim(kept_items)

# Examine frequencies & other descriptives
sjPlot::tab_itemscale(kept_items)





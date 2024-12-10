# Overview ----------------------------------------------------------------
# Associated project: Meyer FAI scale
# Script purpose: Select items Area 2
#
# Written by: Corrado Caudek (corrado.caudek@unifi.it)
# Version: Fri Nov 18 13:48:49 2022
# Last update: Thu Oct 24 11:42:26 2024
# Status: In progress
# Notes: 


# Load necessary libraries ------------------------------------------------

if (!requireNamespace("pacman")) install.packages("pacman")

pacman::p_load(
  here, tidyverse, TAM, mirt, lavaan, mokken, psych,
  gridExtra, grid, semTools
)

# Theme settings
# theme_set(bayesplot::theme_default(base_size = 18, base_family = "sans"))


# Import data -------------------------------------------------------------

df_tot <- readRDS(
  here("data", "processed", "fai_2022_11_20.rds")
)


# Data wrangling ----------------------------------------------------------

# Careless responding
# Filter for rows with FLAG == "keep" and remove the FLAG column
temp <- df_tot |> 
  dplyr::filter(FLAG == "keep")
temp$FLAG <- NULL


# Descriptive statistics for the items ------------------------------------

# Compute skewness and kurtosis
items_stats <- psych::describe(temp) 

# Plot histograms for skewness and kurtosis
hist(items_stats$skew, main = "Histogram of Skewness", xlab = "Skewness")
hist(items_stats$kurtosis, main = "Histogram of Kurtosis", xlab = "Kurtosis")

# Identify items with high skewness or kurtosis (absolute values)
items_skew_kurt_bad <- rownames(items_stats)[abs(items_stats$skew) > 2.5 | abs(items_stats$kurtosis) > 7.5]

# Remove NA values from items_skew_kurt_bad
items_skew_kurt_bad <- na.omit(items_skew_kurt_bad)

# Select columns excluding the problematic items
df_clean <- temp %>%
  dplyr::select(-dplyr::all_of(items_skew_kurt_bad))

# Check the number of removed items (columns)
num_removed <- ncol(temp) - ncol(df_clean)
cat("Number of items removed:", num_removed)

# Output summary statistics for the cleaned data (skewness and kurtosis)
items_stats_clean <- psych::describe(df_clean) 
summary(items_stats_clean$skew)
summary(items_stats_clean$kurtosis)

# Define a subscale of items
item_subscale <- c(
  "FAI_164", "FAI_180", "FAI_154", "FAI_162", "FAI_105", "FAI_40", "FAI_196",   
  "FAI_8", "FAI_134", "FAI_131", "FAI_43",  "FAI_187", "FAI_136", "FAI_54",  
  "FAI_181", "FAI_50", "FAI_48",  "FAI_25",  "FAI_133", "FAI_11",  "FAI_33",  
  "FAI_129", "FAI_169", "FAI_46", "FAI_174", "FAI_73",  "FAI_127", "FAI_39",  
  "FAI_112", "FAI_103", "FAI_51",  "FAI_53", "FAI_58",  "FAI_32"
)

# Select items that are part of the subscale and not problematic
names_item_subscale_clean <- setdiff(item_subscale, items_skew_kurt_bad)
subscale_data <- df_clean %>%
  dplyr::select(all_of(names_item_subscale_clean))

# Check the dimensions of the resulting data
dim(subscale_data)

# Function to create bar plots for each item
many_plots <- function(x) {
  item_data <- subscale_data[[x]]
  g <- ggplot(subscale_data, aes(x = item_data)) +
    geom_bar(color = 'grey', fill = 'white', size = 1) +
    geom_vline(
      aes(xintercept = mean(item_data, na.rm = TRUE)), 
      size = 1.5, linetype = 'dashed', col = 'chartreuse1'
    ) +
    xlab(paste("Score |", "Item:", x)) 
  print(g)
}

# Create list of plots for each item
plot_list <- list()
n_items <- ncol(subscale_data)
for(i in 1:n_items) {
  p <- many_plots(names(subscale_data)[i])
  plot_list[[i]] <- p
}

# Arrange the plots in a grid
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

# Remove FAI_174, FAI_53, FAI_32
item_subscale <- c(
  "FAI_164", "FAI_180", "FAI_154", "FAI_162", "FAI_105", "FAI_40", "FAI_196",   
  "FAI_8", "FAI_134", "FAI_131", "FAI_43",  "FAI_187", "FAI_136", "FAI_54",  
  "FAI_181", "FAI_50", "FAI_48",  "FAI_25",  "FAI_133", "FAI_11",  "FAI_33",  
  "FAI_129", "FAI_169", "FAI_46", "FAI_73",  "FAI_127", "FAI_39",  
  "FAI_112", "FAI_103", "FAI_51",  "FAI_58"
)

# # Plot of the frequency distribution of the responses for each item.
for(n in item_subscale) {
  distr <- table(subscale_data[,n])
  barplot(distr,
          main=n,
          col=gray.colors(20),
          ylab = "Number of respondents",
          xlab = "Response (1=Negative, 7=Positive)");
}


bluesqs <- cor(subscale_data, method = "spearman")
# heat plot of correlations matrix 
# uncomment the png & devoff lines if you want to save as png in the working directory
# png('corplot.png')
corrplot::corrplot(bluesqs, method = 'ellipse', order = 'AOE', type = 'upper')


# Remove outliers ---------------------------------------------------------

# check outliers in item sets
outliers_data <- outliers::outlier(subscale_data)
# Remove outliers
subscale_data_clean <- subscale_data[!rownames(subscale_data) %in% outliers_data, ]
subscale_data <- subscale_data_clean


# =========================================================================== #
#              Step 2: NON-PARAMETRIC ITEM RESPONSE THEORY (NIRT)             #
# =========================================================================== #

# Item properties ---------------------------------------------------------
# H coefficients and Automated Item Selection Procedure (AISP)

## Step 1: Calculate H coefficients for the subscale data -----------------

# coefH() calculates Loevinger's H coefficients, which are used to measure the 
# scalability of items in a Mokken scale. The H coefficient indicates how well 
# an item relates to the latent construct that the scale is intended to measure.
# Higher values of H indicate stronger relationships. 
# Typically, the following thresholds are used:
#   - H < 0.3: Items are considered weakly scalable and should be removed.
#   - H between 0.3 and 0.4: Items are considered moderate in scalability.
#   - H > 0.5: Items are considered strongly scalable and highly reliable.
coefs.h <- coefH(subscale_data)

# Display item-level (Hi) scalability coefficients
# coefs.h$Hi provides the H coefficient for each individual item.
# This value tells us how well each specific item contributes to the overall scale.
coefs.h$Hi

# Display overall scalability coefficient (H) for the entire scale
# coefs.h$H provides the overall H coefficient for the entire scale, indicating the scalability of the full set of items.
# An overall H coefficient above 0.3 is generally considered acceptable for the scale.
coefs.h$H

# Save a list with H coefficients for individual items and the overall scale
# Storing these coefficients allows for further examination and comparison of item scalability.
coefs.h.all <- list(coefs.h$Hi, coefs.h$H)


## Step 2: Automated Item Selection Procedure (AISP) ----------------------

# AISP is a method for selecting items based on their scalability (H coefficient).
# We remove items that have a scalability coefficient (H) lower than 0.3, as 
# these items are weakly scalable.
# The threshold of 0.3 is widely accepted as the minimum for an item to be 
# considered as part of a valid scale.
aisp.lb <- mokken::aisp(subscale_data, lowerbound = 0.3)

# Print the structure of the result to see which items were kept
# Items with H >= 0.3 are retained, while those with H < 0.3 are removed.
print(aisp.lb)

# Convert the result of AISP (aisp.lb) to a logical vector
# Items that have an AISP result of '1' are kept, meaning their H coefficient is >= 0.3.
kept_items_1 <- subscale_data[, as.numeric(aisp.lb) == 1]

# Display the retained items
# These items passed the threshold for acceptable scalability.
print(kept_items_1)


## Step 3: Verify and calculate H coefficients for the retained items -----

# After removing items with low scalability, we now recalculate the H coefficients for the retained items.
# This step ensures that the remaining items still form a coherent and scalable Mokken scale.
names(kept_items_1)
# [1] "FAI_164" "FAI_105" "FAI_196" "FAI_134" "FAI_50"  "FAI_48"  "FAI_25"  "FAI_133"
# [9] "FAI_11"  "FAI_33"  "FAI_129" "FAI_169" "FAI_46"  "FAI_127" "FAI_39"  "FAI_112"
# [17] "FAI_103" "FAI_51"  "FAI_58" 

# Recalculate the H coefficients for the remaining items after AISP.
# The new item-level (Hi) and overall scale (H) coefficients are computed to validate the retained items' scalability.
coefH(kept_items_1)


# Check monotonicity, Invariant Item Ordering (IIO), and local independence

## Step 1: Examine the monotonicity of items ------------------------------

# Monotonicity refers to the requirement that the probability of a higher response category should increase 
# as the latent trait increases. This is a fundamental assumption of the Mokken scale model.
# `check.monotonicity()` checks whether the items in the scale satisfy this condition.
mc <- check.monotonicity(kept_items_1)

# Summary of monotonicity results:
# The summary shows which items violate monotonicity (if any), and how severe the violations are.
# Items that violate monotonicity should be examined carefully, as they may not align well with the latent construct.
summary(mc)

# Plot the monotonicity check results:
# This plot visualizes the monotonicity of items. A well-behaved item should show increasing probability
# for higher response categories as the latent trait increases.
plot(mc)


## Step 2: Check Invariant Item Ordering (IIO) ----------------------------

# IIO is the property that the relative ordering of item difficulties should 
# remain the same across levels of the latent trait.
# This means that if Item A is more difficult than Item B at one level of the 
# latent trait, it should remain more difficult at other levels.
# `check.iio()` tests for IIO violations. The "MIIO" method (Monotone Invariant 
# Item Ordering) is commonly used for this purpose.
iio.list2 <- check.iio(kept_items_1, method = "MIIO")

# The summary shows whether any items violate the IIO assumption. Items that 
# violate IIO may be problematic for measuring the latent construct consistently 
# across the range of trait values.
summary(iio.list2)


## Step 3: Remove items violating IIO -------------------------------------

# Items violating IIO are identified and removed to ensure the remaining items 
# conform to the IIO property.
# `iio.list2$items.removed` contains the names of items that violated IIO and 
# should be excluded.
kept_items_2 <- kept_items_1[
  , is.na(charmatch(dimnames(kept_items_1)[[2]], names(iio.list2$items.removed)))]

# Re-check IIO after removing problematic items. This ensures that the remaining 
# items satisfy the IIO assumption.
summary(check.iio(kept_items_2))


## Step 4: Check local independence (conditional association) -------------

# Local independence means that after controlling for the latent trait, items 
# should be uncorrelated with each other.
# `check.ca()` checks for conditional association violations, which indicate 
# local dependence between items. Items with high local dependence may overlap 
# in content or be redundant.
ca <- mokken::check.ca(kept_items_2, TRUE)

# The following outputs help identify problematic items:
# - `ca$InScale`: Indicates which items remain in the scale after each iteration 
# of removing the worst item.
# - `ca$Index`: Provides the strength of conditional associations (CA) for each 
# item.
# - `ca$Flagged`: Items flagged for high conditional associations, which 
# indicate violations of local independence.
ca$InScale
ca$Index
ca$Flagged


# Step 5: Select items that satisfy local independence --------------------

# We select the final set of items that pass the local independence check.
# `ca$InScale[[8]]` contains the items remaining in the scale after checking for 
# local independence in multiple iterations.
last_iteration <- length(ca$InScale)
names_kept_loc_ind_items <- names(kept_items_2[ca$InScale[[last_iteration]]])


## Step 6: Create the final dataset ---------------------------------------

# Items satisfying all criteria (monotonicity, IIO, local independence)

# The `dplyr::select()` function is used to keep only the items that satisfied 
# all tests.
kept_items_3 <- kept_items_2 %>% 
  dplyr::select(all_of(names_kept_loc_ind_items))

# Check the dimensions of the final dataset
# This shows how many items remain after applying all the checks and removing 
# problematic items.
dim(kept_items_3)

# Outliers
# Calculates the total score for each row (individual) in the kept_items_3 dataset.
xPlus   <- rowSums(kept_items_3)
# Calculates the G-plus statistic for each item in the kept_items_3 dataset 
# using the mokken package. This statistic measures the extent to which an item 
# fits the unidimensionality assumption of the Mokken model.
gPlus   <- mokken::check.errors(kept_items_3)$Gplus
hist(gPlus)
# Calculates the O-plus statistic for each item in the kept_items_3 dataset. 
# This statistic measures the extent to which an item is locally independent of 
# other items in the scale.
oPlus   <- mokken::check.errors(kept_items_3, TRUE, TRUE)$Oplus
cor(cbind(oPlus, gPlus, xPlus))

Q3 <- summary(gPlus)[[5]]
IQR <- Q3 - summary(gPlus)[[2]]
# Identifies outliers as items with G-plus statistics greater than the upper 
# fence (Q3 + 1.5*IQR).
outlier <- gPlus > Q3 + 1.5 * IQR

# Remove outliers
kept_items_3_clean <- kept_items_3[!outlier, ]
kept_items_3 <- kept_items_3_clean
dim(kept_items_3)


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
# High outfit/infit values and low p-values: These suggest that an item is 
# misfitting the model. 
subscale_fit$itemfit

# An outfit value close to 1 indicates a good fit between the item and the model. 
# Deviations from 1 suggest misfit.
outfit <- subscale_fit$itemfit[, c(1, 3)]
outfit$delta <- abs(outfit$Outfit - 1)
outfit %>% 
  arrange(delta)

# Root Mean Square Deviation (RMSD) 
# RMSD < 0.05: Generally considered a good fit.
# 0.05 < RMSD < 0.10: May indicate some misfit, but the item may still be 
# retained depending on the context.
# RMSD > 0.10: Suggests a significant misfit, and the item should be 
# investigated further.
subscale_rmsd <- IRT.itemfit(subscale_2pl) # RMSD item fit
subscale_rmsd$RMSD %>% 
  arrange(Group1)

# (2) Check for local dependence (aQ3), which impairs unidimensionality.
# Local dependence refers to the situation where an item's response is not 
# solely determined by the underlying latent trait, but also depends on the 
# responses to other items. This can impair the unidimensionality of the scale.
# A commonly used cutoff for flagging item pairs with notable local dependence 
# is |0.25|.
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

# To shorten the original item pool we selected the items that offered more 
# information by considering the shape of each item information function, which 
# display the amount of item information along the latent trait. 
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

# Select the items that conveyed higher information along the trait continuum.
names_kept_items <- c("FAI_129", "FAI_133", "FAI_25", "FAI_103") 
# When considering the items content, it can be noted that the
# retained items addressed the key features of...
kept_items <- kept_items_3 |> 
  dplyr::select(
    all_of(names_kept_items)
  )
dim(kept_items)

# TAM IRT Models ----
# 0-4 items, as required by TAM

# PCM (Partial Credit Model)
good_items_r <- TAM::tam.mml(
  kept_items,
  irtmodel = "PCM",
  control = list(Msteps = 10, QMC = FALSE, snodes = 0, 
                 convD = .0001, conv = .00001, convM = .00001)
)
summary(good_items_r)
# The EAP reliability indicates that the scale has good reliability.

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
summary(good_items_fit)
summary(good_items_rmsd)

# Information Criteria to evaluate model fit
anova(good_items_r, good_items_2pl)
good_items_r$ic
good_items_2pl$ic

# local dependence: aQ3 correlations ----
good_items_res <- TAM::tam.modelfit(good_items_2pl)
good_items_res$Q3_summary
subscale_res$Q3.matr

# Local dependence was quantified using the adjusted Q3 
# (aQ3; Marais, 2013) statistic, a bias-corrected form of the 
# traditional Q3 statistic (Yen, 1984). A cut-off of |.25| was 
# used to flag the most notably dependent item pairs 
# (Christensen et al., 2017).
round(good_items_res$aQ3.matr, 3)
# There are no correlations 0.25.


# =========================================================================== #
#                       Step 4: Factor analysis                               #
# =========================================================================== #

# Test sampling adequacy
psych::KMO(kept_items)
psych::cortest.bartlett(cor(kept_items), n=nrow(kept_items))

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
# one_factor_model <-  'F =~ FAI_127 + FAI_129 + FAI_46 + FAI_33'
one_factor_model <-  'F =~ FAI_129 + FAI_133 + FAI_25 + FAI_103'

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
qgraph::qgraph(cor(kept_items), layout = "spring", labels = colnames(kept_items))

# Partial correlations matrix
qgraph::qgraph(cor(kept_items), layout = "spring", labels = colnames(kept_items), graph = "pcor") 

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


# eof ---




# Script name: 012_area_2.R
# Project: project
# Script purpose: AREA 2: Richieste di caregiving
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


# First subscale: items names.
item_subscale <- c(
  "FAI_164", "FAI_180", "FAI_154", "FAI_162", "FAI_105", "FAI_40", "FAI_196",   "FAI_8", 
  "FAI_134", "FAI_131", "FAI_43",  "FAI_187", "FAI_136", "FAI_54",  "FAI_181", "FAI_50", 
  "FAI_48",  "FAI_25",  "FAI_133", "FAI_11",  "FAI_33",  "FAI_129", "FAI_169", "FAI_46", 
  "FAI_174", "FAI_73",  "FAI_127", "FAI_39",  "FAI_112", "FAI_103", "FAI_51",  "FAI_53", 
  "FAI_58",  "FAI_32"
) 

item_subscale_clean <- setdiff(item_subscale, items_skew_kurt_bad)

# Select only the items of this subscale.
subscale_data <- df %>% 
  dplyr::select(all_of(item_subscale_clean))
dim(subscale_data)


# =========================================================================== #
#                     STEP 1: DESCRIPTIVE STATISTICS                          #
# =========================================================================== #

# Descriptives ordinal items.
descrmyitems <- as.data.frame(round(psych::describe(subscale_data ), 2))
descrmyitems

summaries.file <- "summaries.txt";
cat( "Variable\tNegative\t1\t2\t3\t3\tZeroto2\tThreeto4\tmissing\n", file=summaries.file, append=FALSE );
# Write a function to include values for an item.
write.summary.var <- function( x, xname )
{
  a1 <- sum( x == 0, na.rm=TRUE );
  a2 <- sum( x == 1, na.rm=TRUE );
  a3 <- sum( x == 2, na.rm=TRUE );
  a4 <- sum( x == 3, na.rm=TRUE );
  a5 <- sum( x == 4, na.rm=TRUE );
  a6 <- round((sum( x <=2, na.rm=TRUE )*100/nrow(mydata)), 2);
  a7 <- round((sum( x >3, na.rm=TRUE )*100/nrow(mydata)), 2);
  a8 <-sum(is.na(x));
  cat( paste( xname, "\t", a1, "\t", a2, "\t", a3, "\t", a4, "\t", a5, 
              "\t", a6, "\t", a7, "\t", a8, "\n", sep="" ), 
       file=summaries.file, append=TRUE );
}
# For each item, run this function iteratively.
for( n in item_subscale) {
  write.summary.var(subscale_data[,n], n)
}
# and read the table in R
myitemssum = read.table( file="summaries.txt", header = TRUE, sep = "\t", quote="\"" )
# Add column names
colnames(myitemssum) <- c("Item label","0", "1","2","3","4",  "% 0 to 1", "% 3 to 4", "No. missing")
myitemssum

myitemssumOrder <- myitemssum[order(myitemssum[, 6]),] # based on the 6th column (% low scores)
barplot(myitemssumOrder[,"% 3 to 4"], 
        main = "Low score frequencies for items",
        xlab="Items", 
        ylab="Number of respondents", 
        cex.lab=0.8,
        cex.axis=0.8,
        names.arg=myitemssumOrder[, "Item label"], 
        las=2, 
        cex.names=0.6)

myitemssumOrder <- myitemssum[order(myitemssum[, 7]),] # based on the 7th column (% high scores)
barplot(myitemssumOrder[,"% 3 to 4"], 
        main = "High score frequencies for items",
        xlab="Items", 
        ylab="Number of respondents", 
        cex.lab=0.8,
        cex.axis=0.8,
        names.arg=myitemssumOrder[, "Item label"], 
        las=2, 
        cex.names=0.6)

# Plot of the frequency distribution of the responses for each item.
for( n in item_subscale) {
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
kept_h_items <- subscale_data[, aisp.lb == 1]
names(kept_h_items)
coefH(kept_h_items)


# Check monotonicity, IIO, and local independence 

# Examine the monotonicity.
mc <- check.monotonicity(kept_h_items)
summary(mc)
plot(mc)

# Check of Invariant Item Ordering (IIO).
out <- summary(check.iio(kept_h_items))
out2 <- out$backward.selection 
colnames(out2) <- c("step1", "step2", "step3", "step4")
# Select items without IIO violations.
names_kept_iio_items <- out2 |> 
  as.data.frame() |> 
  dplyr::filter(step1 == 0) |> 
  rownames()

# Remove items violating IIO for the scale.
kept_iio_items <- kept_h_items |> 
  dplyr::select(names_kept_iio_items)
summary(check.iio(kept_iio_items))

# Check local independence (conditional association)
ca <- mokken::check.ca(kept_iio_items, TRUE)
# Subcomponents correspond to the iteration. The first subcomponent refers to 
# the situation with all items in the test, the second subcomponent refers to 
# the sitution with the worst item deleted, the third subcomponent refers to 
# the sitution with the two worst items deleted, etc.
ca$InScale
ca$Index
ca$Flagged

# Slect items satisfying local independence.
names_kept_loc_ind_items <- names(kept_iio_items[ca$InScale[[5]]])

# Select items according to local dependence.
kept_loc_ind_items <- good_items %>% 
  dplyr::select(all_of(names_kept_loc_ind_items))
dim(kept_loc_ind_items)


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
  kept_loc_ind_items,
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
  data = kept_items,
  1,
  itemtype = "graded",
  SE = TRUE,
  verbose = FALSE
)

n_item <- length(names(kept_loc_ind_items))
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
kept2_items <- c("FAI_127", "FAI_129", "FAI_46", "FAI_33") 
# When considering the items content, it can be noted that the
# retained items addressed the key features of...
last_items <- subscale_data |> 
  dplyr::select(
    all_of(kept2_items)
  )
dim(last_items)

# We also checked the fit indexes of a CFA model.
one_factor_model <-  '
  F =~ FAI_127 + FAI_129 + FAI_46 + FAI_33
'

fit <- lavaan:::cfa(
  one_factor_model,
  data = last_items,
  ordered = names(last_items),
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
  last_items,
  irtmodel = "PCM",
  control = list(Msteps = 10, QMC = FALSE, snodes = 0, 
                 convD = .0001, conv = .00001, convM = .00001)
)
# summary(good_data)

# 2PL IRT Generalized Partial Credit Model
good_items_2pl <- TAM::tam.mml.2pl(
  last_items,
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

effectsize::cohens_d(rowSums(good_data), d_clean$death_risk_f)

temp <- data.frame(area1 = rowSums(good_data), d_clean$has_emergency_care_f) %>% 
  drop_na()
effectsize::cohens_d(temp$area1, temp$has_emergency_care_f)


# effectsize::cohens_d(rowSums(good_data), demo_info$first_aid)
# plot(demo_info$`NUMERO ACCESSI PS`, rowSums(good_data))
# m <- lm(rowSums(good_data) ~ demo_info$`NUMERO ACCESSI PS`)
# abline(m)
# summary(m)


# =========================================================================== #
#                       Step 4: Factor analysis                               #
# =========================================================================== #

# Test sampling adequacy
psych::KMO(last_items)
psych::cortest.bartlett(last_items)

# Factor analysis via parallel analysis
fa.parallel(last_items, cor = "poly")

# Very simple structure analysis
vss(last_items, 3)

# Factor analysis
fa(last_items, nfactors = 1, fm = "minres", n.iter=100)
fa(last_items, nfactors = 1, fm = "pa", n.iter=100)

# Plot the fa solution
plot(fa(last_items, nfactors = 1, fm = "minres"))

# Plot diagram fa solution
fa.diagram(fa(last_items, nfactors = 1, fm = "minres"))

# Factor Miner package PCA
FactoMineR::PCA(last_items)$eig

# Principal component analysis
principal(last_items, 1, rotate = "varimax")

# Hierarchical cluster analysis using ICLUST (groups items)
iclust(last_items, title = "ICLUST using Pearson correlations")

# SEM
# We also checked the fit indexes of a CFA model.
one_factor_model <-  '
  F =~ FAI_127 + FAI_129 + FAI_46 + FAI_33
'

fit <- lavaan:::cfa(
  one_factor_model,
  data = last_items,
  ordered = names(last_items),
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
qgraph(cor(last_items), layout = "spring", labels = colnames(last_items))

# Partial correlations matrix
qgraph(cor(last_items), layout = "spring", labels = colnames(last_items), graph = "pcor") 

eigen(cov(last_items))$values[1] / eigen(cov(last_items))$values[2]


psych::describe(last_items)
psych::alpha(last_items)

# parallel analysis
spar <- fa.parallel(
  last_items, fm="ml", fa="fa", sim=FALSE,
  error.bars=TRUE, se.bars=FALSE, n.iter=100
)


# =========================================================================== #
#           Step 5: Classical Test Theory (CTT)                               #
# =========================================================================== #

# Calculate Cronbach alpha, beta, omega, and split half reliability
summary(psych::alpha(last_items))
splitHalf(last_items)
omega.res <- ci.reliability(last_items, type = "omega", conf.level = 0.95,
                            interval.type = "perc", B = 100)
omega.res

# Find Cronbach alpha if an item is dropped 
sjPlot::tab_itemscale(good_data)


# =========================================================================== #
#           Step 6: Total (sub)scale scores                                   #
# =========================================================================== #

# Average scale scores
good_data$afi.mean <- rowMeans(good_data)

# Scale mean, SD, skewness, kurtosis
psych::describe(good_data)
Hmisc::describe(good_data)
skimr::skim(good_data)


# Examine frequencies & other descriptives
sjPlot::tab_itemscale(good_data)





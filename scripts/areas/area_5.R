# AREA 5: Coping

item_subscale <- c(
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


# Select only the items of this subscale.
subscale_data <- df %>% 
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
good_items <- c("i195", "i161", "i178", 
                "i128", "i115", "i66", "i7")
# When considering the items content, it can be noted that the 
# retained items addressed the key features of...
good_data <- subscale_data %>% 
  dplyr::select(
    all_of(good_items)
  )
dim(good_data)

# We also checked the fit indexes of a CFA model.
one_factor_model <-  '
  F1 =~ i195 + i161 + i178 + i128 + i115 + i66 + i7
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

# effectsize::cohens_d(rowSums(good_data), demo_info$severe_illness)
# effectsize::cohens_d(rowSums(good_data), demo_info$first_aid)
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




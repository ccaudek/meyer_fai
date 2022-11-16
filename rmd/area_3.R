#' ------------------------------------------------------------------
#' Area 3: percezione della cura formale
#' 
#' "Mon Jan  6 13:35:09 2020"
#' ------------------------------------------------------------------


# Set up ----

library("here")
library("brms")
source(here("scripts", "00_prelims.R"))


# Import data ----

source(here("scripts", "01_read_data_and_MI.R"))
mydata <- complete_data
myitems <- colnames(mydata)


# Extreme response patterns ----

n_subj <- nrow(mydata)
n_items <- ncol(mydata)

# run frequencies for all items
for(n in myitems) {
  cat("\n", n, ":")
  x <- table(mydata[, n], exclude=NULL) / n_subj
  print(x)
}

bad_item <- rep(NA, n_items)
for(n in 1:n_items) {
  x <- table(mydata[, n], exclude=NULL) / n_subj
  bad_item[n] <- any(x > .95)
}

# There are  
sum(bad_item)
# items in which the 95% of responses was given to a 
# single category of the ordinal item. The items 
(1:n_items)[bad_item]
# must be excluded from further analyses.
mydata2 <- mydata[, -((1:n_items)[bad_item])]


# Item selection for Area 3 ----

# Percezione della cura formale

# The item "i77" was already removed.
a3_names <- c(
  "i156", "i67", "i176", "i28", "i149", "i35", "i192", "i87", "i143", 
  "i90", "i160", "i69", "i170", "i79", "i24", "i76", "i114", "i74", 
  "i92", "i163"
)

a3 <- mydata2 %>% 
  dplyr::select(a3_names)

R <- cor(a3)
round(R, 2)


# Mahalanobis D^2 outliers ----

d2mydata <- outlier(a3, cex=.6, bad=5)

# Remove one outlier (obs = 183 was already removed):
a3 <- a3[-c(183, 195, 336, 279, 256), ]

# Change name of the data.frame for the following analyses.
im <- a3


# Mokken Scaling Analysis (MSA) ----

par(mfrow=c(1, 1))

# * Guttman errors ----

# How many, and which respondents have idiosyncratic response patterns?

# Outliers (participants)
xPlus <- rowSums(im)
gPlus <- check.errors(im)$Gplus
hist(gPlus)
oPlus <- check.errors(im, FALSE, TRUE)$Oplus
cor(cbind(oPlus, gPlus, xPlus))

Q3 <- summary(gPlus)[[5]]
IQR <- Q3 - summary(gPlus)[[2]]
outlier <- gPlus > Q3 + 1.5 * IQR 
sum(outlier)
# if needed to further analyse ouliers
cbind(im, gPlus)[outlier, ]
# then possible sensitivity analysis:
# coefH(im[!outlier, ])

# exclude outlying participants
im <- im[!outlier, ]

# There were 
sum(outlier)
# cases with a number of Guttman errors higher than (Q3 plus 1.5 
# times IQR). 


# * Coefficients of homogeneity ----

# Do items form a single scale?

# Compute scalability coefficients
Hvalues <- coefH(im)
knitr::kable(Hvalues$Hi)


# Automatic item selection algorithm (aisp) at 
# increasing homogeneity levels.
# Is the scale uni-dimensional or multi-dimensional?

# examine aisp for increasing c levels (run the function 
# you defined above and give it a name)
motable.mydata <- moscales.for.lowerbounds(im)
# save it as a data frame
aispmydata <- as.data.frame(motable.mydata)
# if you need to view it
# View(aispmydata)
# if you need it outside the output file
# write.table(motable.mydata, file="./aispmydata.csv", quote = FALSE, sep = "\t",row.names = FALSE)

# select the most appropriate solution, 
# for example, the code below is for the solution at lowerbound .60
myselection <- aisp(im,  lowerbound = 0.75)
myselection
# check which items are in which subscales (here the first subscale)
names_selected_items <- names(im[, myselection==1])
names_selected_items

# check properties for subscales:
# select the first subscale (if MSA confirms the initial 
# 3-subscale structure)
mysubscale1 <- im %>% 
  dplyr::select(names_selected_items)

# check H 
HvaluesSubscale1 <- coefH(mysubscale1)
knitr::kable(HvaluesSubscale1$Hi)

# Subscale 1 has a homogeneity value H(se) = 
HvaluesSubscale1$H  

# Determine whether it is optimal to select the items with the 
# higher scalability coefficients.


# * Conditional association ----

# Are items associated only via the latent dimension?

# Check conditional association (local independence)
CA.def.mysubscale1 <- check.ca(mysubscale1, TRUE)
CA.def.mysubscale1$InScale
CA.def.mysubscale1$Index
CA.def.mysubscale1$Flagged


# Local independence for the subscale 1 items is presented below 
# as TRUE/FALSE values: 

CA.def.mysubscale1$InScale[[1]]


# Monotonicity per subscale

# Is the probability of endorsing a ‘correct’ response option 
# increasing with increasing levels of the latent dimension?

# check monotonicity at different minsize:
# with default minsize:
monotonicity.def.mysubscale1 <- 
  check.monotonicity(mysubscale1, minvi = .03)

# Monotonicity tests are shown for default minsize 
# (alternative values of 60 and 50 are displayed below as R output). 
# Item step response functions (minsize=50) are displayed visually

knitr::kable(summary(monotonicity.def.mysubscale1))

# try different minsizes 60 to 10 
monotonicity.60.mysubscale1 <- check.monotonicity(
  mysubscale1, minvi = .03, minsize = 60
)
summary(monotonicity.60.mysubscale1)
plot(monotonicity.60.mysubscale1)


monotonicity.50.mysubscale1 <- 
  check.monotonicity(mysubscale1, minvi = .03, minsize = 20)
# summary(monotonicity.50.mysubscale1)
plot(monotonicity.50.mysubscale1)


# * Non-intersecting item step response functions ----

# Investigate the assumption of non-intersecting item step response functions (ISRFs) 
# using method MIIO (appropriate for ordinal items)
miio.mysubscale1 <- check.iio(mysubscale1)
# or using rest score (for binary items)
# restscore.mysubscale1 <- check.restscore(mysubscale1)
# several other options are available in mokken: pmatrix, mscpm, and IT

knitr::kable(summary(miio.mysubscale1)$item.summary)
knitr::kable(miio.mysubscale1$violations)

miio.mysubscale1$items.removed


bad <- names(miio.mysubscale1$items.removed)
final_items <- mysubscale1[, !(names(mysubscale1) %in% bad)]

miio.mysubscale1$HT

miio.mysubscale1$item.mean


# * Invariant item ordering (IIO) ----

knitr::kable(summary(miio.mysubscale1)$item.summary)

# Investigate the assumption of non-intersecting item step response 
# functions (ISRFs) at different minsize values
miio.60.mysubscale1 <- check.iio(mysubscale1, minsize = 60)
summary(miio.60.mysubscale1)
miio.50.mysubscale1 <- check.iio(mysubscale1, minsize = 50)
summary(miio.50.mysubscale1)

plot(miio.50.mysubscale1)


# * Parametric IRT ----

# Rating Scale Model

mod1 <- TAM::tam.mml(resp=mysubscale1, irtmodel="RSM")
# estimate person parameters
wle1a <- TAM::tam.wle(mod1)
b1 <- - mod1$AXsi[, -1 ]
b2 <- matrix( mod1$xsi$xsi, ncol=3, byrow=TRUE )
# convert b2 to b1
b1b <- 0*b1
b1b[,1] <- b2[,1]
b1b[,2] <- rowSums( b2[,1:2] )
b1b[,3] <- rowSums( b2[,1:3] )
# assess fit
fit1a <- sirt::pcm.fit(b=b1, theta=wle1a$theta, mysubscale1)
fit1a$item

# Item fit (infit and outfit). Criteria for item fit are considered 
# as within the mean squares range of 0.6-1.4 and standardized fit 
# statistics of +/−2.0.  If outfit and infit are within these values, 
# they can be considered adequate for measuring the latent construct 
# on an interval-level. 

# for ordinal items - Rating Scale model (if all items have the same 
# format, e.g. all are on 5-point scales from strongly agree to 
# strongly disagree)
fit1.Subscale1 <- RSM(
  mysubscale1 #, constrained = FALSE, Hessian=TRUE
)

# summary(fit1.Subscale1)

# **Model fit**, and item-pair and item-triplet residuals for testing 
# **local dependencies** are summarized as output text below.


ppr <- person.parameter(fit1.Subscale1)
# goodness of fit indices
# gofIRT(ppr)
# information criteria
IC(ppr)

# item-pair residuals for testing local dependencies:
# fit model with lrm package (eRm does not include these tests)
# for ordinal items: 1-parameter GRM (the RSM model is not (yet) 
# implemented in ltm)
fit1.ltm.Subscale1 <- grm(mysubscale1, constrained =TRUE)
# model summary (item coefficients are item difficulties with standard 
# errors and standardized z values)
summary(fit1.ltm.Subscale1)
# check model fit (GoF should be ns)
# GoF.rasch(fit1.ltm.Subscale1, B = 199)
# residuals item pairs ( chisq residuals < 3.5 is good - rule of thumb)
margins(fit1.ltm.Subscale1)
# residuals item triplets
margins(fit1.ltm.Subscale1, type = "three-way", nprint = 2) # prints triplets of items with the highest residual values for each response pattern

# Item characteristic curves are displayed visually.
# The distribution of person latent scores and location of item 
# difficulties on the latent (person-item map) are displayed visually.
# Item difficulty and infit statistics are shown in ...

# Plot item difficulty & infit statistics (items should be within the 
# green borders)
plotPWmap(fit1.Subscale1)

mysubscale2 <- mysubscale1 %>% 
  dplyr::select(-i192, -i156, -i67, -i69, -i92, -i79)
fit2.Subscale1 <- RSM(
  mysubscale2 #, constrained = FALSE, Hessian=TRUE
)
ppr <- person.parameter(fit2.Subscale1)
plotPWmap(fit2.Subscale1)


## Separation reliability

# item fit (between 0.6 and 1.4 acc to Wright BD, Linacre JM. 
# Reasonable mean-square fit values. Rasch Meas Trans. 1994;8(2):370.)
itemfit.mysubscale <- itemfit(ppr)
# names(print(itemfit.mysubscale$i.fit))
ItemsFitTbl <- as.data.frame(print(itemfit.mysubscale))

knitr::kable(ItemsFitTbl)

# Personfit (z values should be </= 1.96)
personfit.mysubscale <- personfit(ppr)
PersonFitTBL <- as.data.frame(print(personfit.mysubscale))
# misfitting persons
misoutfits <- nrow(
  PersonFitTBL[
    (PersonFitTBL$`Outfit t` > 1.96 | PersonFitTBL$`Outfit t` < -1.96)  
    & (PersonFitTBL$`Outfit MSQ` < 0.6| PersonFitTBL$`Outfit MSQ` > 1.4 ) 
    , ]
)

# misoutfits <- nrow(
#   PersonFitTBL[(PersonFitTBL$p.outfitZ > 1.96 | PersonFitTBL$p.outfitZ < -1.96)  
#                & (PersonFitTBL$p.outfitMSQ < 0.6| PersonFitTBL$p.outfitMSQ > 1.4 ), ]
# )

misinfits <- nrow(
  PersonFitTBL[
    (PersonFitTBL$`Infit t` > 1.96 | PersonFitTBL$`Infit t` < -1.96) 
    & (PersonFitTBL$`Infit MSQ` < 0.6| PersonFitTBL$`Infit MSQ` > 1.4 )
    , ]
)

# misinfits <- nrow(
#   PersonFitTBL[(PersonFitTBL$p.infitZ > 1.96 | PersonFitTBL$p.infitZ < -1.96) 
#                & (PersonFitTBL$p.infitMSQ < 0.6| PersonFitTBL$p.infitMSQ > 1.4 ), ]
# )

# subgroup invariance test (median split) based on Andersen's liekelihood 
# ratio test (should be ns)
# default splitcr="median", but can also be a separate variable that 
# specified group membership, e.g. gender, 2 disease conditions, etc
# other test available in the function NPtest
lrres <- LRtest(fit1.Subscale1, splitcr = "median")
lrres


# Item outfit values ranged between 
round(min(itemfit.mysubscale$i.outfitMSQ), 2)
# and 
round(max(itemfit.mysubscale$i.outfitMSQ), 2)

# Item infit values ranged between 
round(min(itemfit.mysubscale$i.infitMSQ), 2)
# and 
round(max(itemfit.mysubscale$i.infitMSQ), 2)

# There were 
misoutfits
# persons with misfit according to outfit values (representing 
round(misoutfits*100/nrow(PersonFitTBL), 2)
# percent), and 
misinfits
# persons according to infit values (representing 
round(misinfits*100/nrow(PersonFitTBL), 2)
# percent of all participants).

# Item difficulty estimates (and confidence elipses) for high and low 
# latent score groups are displayed visually.

# plot in a pdf
# pdf( "./GOF-mydata1.pdf", width=10, height=10, paper="special" );
# plot item difficulty estimates (& confidence elipses) for high and low latent score groups (should be close to the line, elipses small)
plotGOF(lrres,conf=list(), tlab="number", cex=0.8)
# dev.off();


# Item parameter confidence intervals based on LR test are displayed 
# visually 

# plot item parameter confidence intervals based on LR test
plotDIF(lrres)




# FA ----

fa.parallel(mysubscale1)

summary(iclust(mysubscale1, title="ICLUST using Pearson correlations"))

omega(mysubscale1, nfactors=1, sl=FALSE)


# CTT ----

psych::alpha(mysubscale1) 

# ci.reliability(
#   data=mysubscale1, 
#   type="omega", 
#   conf.level = 0.95,
#   interval.type="perc", 
#   B=100
# )


# CFA ----

model <- 
'
  F =~ i156 + i67 + i149 + i35 + i192 + i90 + i160 + i69 + i170 + i79 + i92
'

fit <- lavaan::cfa(
  model, 
  data = final_items, 
  ordered = c("i156", "i67",  "i149", "i35",  "i192", "i90",  "i160", 
              "i69",  "i170", "i79",  "i92")
  )
summary(fit)


fitMeasures(fit)


# brms ----

# https://github.com/paul-buerkner/Bayesian-IRT-paper/blob/master/Bayesian-IRT.Rmd

data("VerbAgg", package = "lme4")




d <- final_items
d$id <- seq(1:nrow(d))

# change format from wide to long
long <- d %>%
  gather("item", "resp", -id)

long$resp <- ordered(long$resp)

prior_va_1pl <-
  prior("normal(0, 3)", class = "sd", group = "id") +
  prior("normal(0, 3)", class = "sd", group = "item")

formula_va_ord_1pl <- bf(
  resp ~ 1 + (1 | item) + (1 | id)
)


fit_va_ord_1pl <- brm(
  formula = formula_va_ord_1pl,
  data = long,
  family = brmsfamily("cumulative", "logit"),
  prior = prior_va_1pl,
  iter = 1000,
  warmup = 500,
  cores = 2,
  chains = 2
)


pp_check(fit_va_ord_1pl)

loo1 <- loo(
  fit_va_ord_1pl, cores = parallel::detectCores()
)

plot(loo1)
print(loo1)


ranef_va_ord_1pl <- ranef(fit_va_ord_1pl)
ranef_va_ord_1pl$id[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  select(-Est.Error) %>%
  arrange(Estimate) %>%
  mutate(id = seq_len(n())) %>%
  ggplot(aes(id, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Person Number (Sorted)")


df_person_1pl <- as_tibble(cbind(
  mean_va_1pl = ranef_va_1pl$id[, "Estimate", "Intercept"],
  mean_va_ord_1pl = ranef_va_ord_1pl$id[, "Estimate", "Intercept"],
  se_va_1pl = ranef_va_1pl$id[, "Est.Error", "Intercept"],
  se_va_ord_1pl = ranef_va_ord_1pl$id[, "Est.Error", "Intercept"]
))
plot_mean_person <- df_person_1pl %>%
  ggplot(aes(mean_va_1pl, mean_va_ord_1pl, color = se_va_1pl)) +
  geom_point() +
  geom_abline() +
  scale_color_viridis_c() +
  lims(x = c(-4, 5), y = c(-4, 5)) +
  labs(
    x = "Mean (binary)",
    y = "Mean (ordinal)",
    color = "SD (binary)"
  )


formula_va_ord_2pl <- bf(
  resp ~ 1 + (1 |i| item) + (1 | id),
  disc ~ 1 + (1 |i| item)	
)

prior_va_ord_2pl <- 
  prior("normal(0, 3)", class = "sd", group = "id") + 
  prior("normal(0, 3)", class = "sd", group = "item") +
  prior("normal(0, 1)", class = "sd", group = "item", dpar = "disc")



fit_va_ord_2pl <- brm(
  formula = formula_va_ord_2pl,
  data = long,
  family = brmsfamily("cumulative", "logit"),
  prior = prior_va_ord_2pl,
  iter = 1000,
  warmup = 500,
  cores = 2,
  chains = 2
)
fit_va_ord_2pl <- add_loo(fit_va_ord_2pl)

summary(fit_va_ord_2pl)


pp_check(fit_va_ord_2pl)

loo2 <- loo(
  fit_va_ord_2pl, cores = parallel::detectCores()
)

plot(loo2)
print(loo2)

coef_fit_va_ord_2pl <- ranef(fit_va_ord_2pl)
eta <- coef_fit_va_ord_2pl$item[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column()
alpha <- coef_fit_va_ord_2pl$item[, , "disc_Intercept"] %>%
  exp() %>%
  as_tibble() %>%
  rownames_to_column()
bind_rows(eta, alpha, .id = "nlpar") %>%
  select(-Est.Error) %>%
  rename(item = "rowname") %>%
  mutate(item = as.numeric(item)) %>%
  mutate(nlpar = factor(nlpar, labels = c("Easiness", "Discrimination"))) %>%
  ggplot(aes(item, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  facet_wrap("nlpar", scales = "free_x") +
  geom_pointrange() +
  coord_flip() +
  labs(x = "Item Number")

ranef_va_ord_2pl <- ranef(fit_va_ord_2pl)
ranef_va_ord_2pl$id[, , "Intercept"] %>%
  as_tibble() %>%
  rownames_to_column() %>%
  select(-Est.Error) %>%
  arrange(Estimate) %>%
  mutate(id = seq_len(n())) %>%
  ggplot(aes(id, Estimate, ymin = Q2.5, ymax = Q97.5)) +
  geom_pointrange(alpha = 0.7) +
  coord_flip() +
  labs(x = "Person Number (Sorted)")

cor_person <- cbind(
  va_1pl = ranef_va_1pl$id[, "Estimate", "Intercept"],
  va_2pl = ranef_va_2pl$id[, "Estimate", "eta_Intercept"],
  va_ord_1pl = ranef_va_ord_1pl$id[, "Estimate", "Intercept"],
  va_ord_2pl = ranef_va_ord_2pl$id[, "Estimate", "Intercept"]
) %>%
  cor() %>%
  round(3)
cor_person


area3 <- 
  c("i156", "i67",  "i149", "i35",  "i192", "i90",  "i160", 
           "i69",  "i170", "i79",  "i92", "disease")

d <- complete_data %>% 
  dplyr::select(
    area3
  )

table(d$disease)



d$life_treatening <- ifelse(
  d$disease == "ONCO" | 
    d$disease == "NEURO-ONCO" |
    d$disease == "RIA", "yes", "no")




d$id <- seq(1:nrow(d))

# change format from wide to long
long <- d %>%
  gather("item", "resp", -c("id", "life_treatening"))

long$resp <- ordered(long$resp)
long$life_treatening <- factor(long$life_treatening)



formula_va_ord_cov2 <- bf(
  resp ~ cs(life_treatening) +
    (0 + life_treatening | item) + (0 | id)
)

prior_va_1pl <- 
  prior("normal(0, 3)", class = "sd", group = "id") + 
  prior("normal(0, 3)", class = "sd", group = "item")


fit_va_ord_cov2 <- brm(
  formula = formula_va_ord_cov2,
  data = long, 
  family = brmsfamily("acat", "logit"),
  prior = prior_va_1pl,
  iter = 1000,
  warmup = 500,
  cores = 2,
  chains = 2
)


formula_va_1pl_cov1 <- bf(
  life_treatening ~ 1 + (1 | item) + (0 | id)
)


fit_va_1pl_cov1 <- brm(
  formula = formula_va_1pl_cov1,
  data = long, 
  family = brmsfamily("bernoulli", "logit"),
  prior = prior_va_1pl,
  iter = 1000,
  warmup = 500,
  cores = 2,
  chains = 2
)



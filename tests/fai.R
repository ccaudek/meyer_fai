#' ---
#' title: "Development and validation of the FAI scale"
#' author: "Corrado Caudek"
#' date: "`r date()`"
#' output:
#'  html_document:
#'    fig_caption: true
#'    toc: true
#'    highlight: tango
#' ---

#' # Motivation
#'

#-------------------------setup-------------------------
#+ setup, include=FALSE, warning=FALSE, message=FALSE

library("tidyverse")
library("readxl")
library("mice")
library("VIM")
library("paran")
library("psych")
library("lavaan")
library("polycor") # for hetcor()
library("multilevel")
library("here")

options(max.print=1000000)


# read data
# fai_s <- read_xlsx(here("data", "raw", "FAI_S_27_11_19.xlsx"), col_names = TRUE)
fai_s <- read_xlsx(here("data", "raw", "FAI_S_27_11_19.xlsx"), col_names = TRUE)


# glimpse(fai_s)

# correct wrong encoding!
# fai_s$`FAI-S 162` <- as.numeric(fai_s$`FAI-S 162`)


#-------------------------select items-------------------------
#+ chunk_select_items, cache=TRUE, warning=FALSE

items <- fai_s[, 51:247]

hist(rowSums(items, na.rm = TRUE))


#-------------------------multiple imputation-------------------------
#+ chunk_mi, cache=TRUE, warning=FALSE

# Preliminary data analysis revealed that missing data were distributed randomly across 
# items and participants. Given that total missing data were less than 5% for each variable, 
# data were replaced using ...

item_names  <- paste("i", 1:ncol(items), sep="")
colnames(items) <- item_names


if (0) {
  md.pattern(items)
  mice_plot <- aggr(
    items,
    col = c("navyblue", "yellow"),
    numbers = TRUE,
    sortVars = TRUE,
    labels = names(items),
    cex.axis = 0.7,
    gap = 3,
    ylab = c("Missing data", "Pattern")
  )
}


imputed_data <- mice(
  items, 
  m = 1, 
  maxit = 1, 
  method = 'pmm', 
  seed = 123
)


complete_data <- complete(imputed_data, 1)
dim(complete_data)


# All items, with the exception of two items ..., were deemed normally distributed on the basis 
# of the absolute skew and kurtosis values being within the acceptable range of 2.00 and 7.00, 
# respectively (Curran, West, & Finch, 1996).


#-------------------------parallel analysis-------------------------
#+ chunk_parallel_analysis, cache=TRUE, warning=FALSE

# paran_out <- paran(complete_data, iterations = 5000)
# 
# 
# paran(complete_data, iterations = 5000, centile = 0, quietly = FALSE,
#       status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE,
#       col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE,
#       file = "", seed = 0)
# Adjusted eigenvalues > 0 indicate dimensions to retain.
# (13 factors    retained)



# Item analysis  ----------------------------------------------------------

# Calculate item-total correlations.
out <- multilevel::item.total(complete_data)

out1 <- out %>% 
  dplyr::filter(Item.Total > 0.3)
dim(out1)

good_items <- out1$Variable

complete_data_2 <- 
  complete_data[, (names(complete_data) %in% good_items)]

  
# For normal distribution, skewness and kurtosis = 0 

# car::qqPlot(complete_data$i2)
# plot(density(complete_data$i3))
# hist(complete_data$i1)

# Select only items with good skewness and kurtosis
item_descript <- as.data.frame(describe(complete_data_2))
item_descript$item_number <- row.names(item_descript)

item_descript_2 <- 
  item_descript[abs(item_descript$skew) < 1.2 & abs(item_descript$kurtosis) < 1.2, ]

good_items <- item_descript_2$item_number

complete_data_3 <- complete_data_2[, (names(complete_data_2) %in% good_items)]
dim(complete_data_3)

# pairs.panels(complete_data_2[, 1:10])
# outlier(complete_data_3[, 1:10])
# error.bars(complete_data_3)

r <- round(lowerCor(complete_data_3), 2)
# corPlot(r) 
# corr.test(complete_data_2[, 1:10])

fa.parallel(complete_data_3)


# Exploratory FA ----------------------------------------------------------

R <- cor(complete_data_3)
print(R, digits = 2)

fa_out <- principal(R, nfactors = 8, n.obs = 324, rotate = "promax")
fa_out
print(fa_out$loadings, cutoff = 0.4)




# One factor model - CURA FORMALE - RC1 --------------------------------------------------------

cura_formale <- 
  c("i156", "i67", "i176", "i77", "i28", "i149", "i35", 
      "i192", "i87", "i143", "i90", "i160", "i69", "i170",
      "i79", "i24", "i76", "i114", "i74", "i92", "i163")

new_data <- complete_data[, (names(complete_data) %in% cura_formale)]


alpha(new_data)

# Calculate item-total correlations.
out <- multilevel::item.total(new_data)

out1 <- out %>% 
  dplyr::filter(Item.Total > 0.7)
dim(out1)

good_items <- out1$Variable

d <- new_data[, (names(new_data) %in% good_items)]

names(d)



# model definition
onef_model <-  '
F1 =~ NA*i143 + i28 + i87 + i176 + i114
F1 =~ i28 + i35 + i67 + i69 + i79 + i87 + i90 + i143 + i156 + i176 + i192
'

# fit the model
fit1 <- lavaan:::cfa(
  onef_model, 
  data = d,  
  ordered = names(d),
  std.lv = TRUE
  )

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# dd <- d[, names(d) %in% c("i28", "i35", "i67", "i69", "i79", "i87", "i90", "i143", "i156", "i176", "i192")]
dd <- d[, names(d) %in% c("i192", "i87", "i143", "i79")]

mod  <- mirt::mirt(dd, 1)
mod <- mirt(data=dd, 1, itemtype="graded", SE=TRUE, verbose=FALSE)

plot(mod, type = 'trace', which.items = 1:4, 
     main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4)) 

plot(mod, type = 'infotrace', which.items = 1:4, 
     main = "", par.settings = simpleTheme(lwd=2))


ITEM_NUMBER <- 5
extr.item <- extract.item(mod, ITEM_NUMBER)
Theta <- matrix(seq(-4,4, by = .1))
info.item <- iteminfo(extr.item, Theta)
plot(Theta, info.item, type = 'l', main = 'Item information')


# ----GRM: Fit indices----
mod_fit <- mirt::M2(mod, type="C2")
mod_fit

# Assumption testing showed a very strong one factor solution and acceptable 
# Local Dependence (LD) (Yen's Q pairs were around .02 - .25).
fit <- mirt::itemfit(mod)
fit

mirt::residuals(mod)
mirt::residuals(mod, type = 'JSI', fold=FALSE) # unfolded
Theta <- mirt::fscores(mod)
residuals(mod, type = 'Q3', Theta=Theta)
residuals(mod, type = 'Q3', method = 'ML')


# model definition
onef_model <-  '
F1 =~ i192 + i87 + i143 + i79
'

# fit the model
fit1 <- lavaan:::cfa(
  onef_model, 
  data = d,  
  ordered = names(dd),
  std.lv = TRUE
)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)




# One factor model - FATTORI INTRAPSICHICI/AUTOSTIMA - RC3 --------------------------------------------------------


autostima <- c("i177", "i191", "i122", "i62", "i104", "i42", "i111")

new_data <- complete_data[, (names(complete_data) %in% autostima)]


mod <- mirt(data=new_data, 1, itemtype="graded", SE=TRUE, verbose=FALSE)

plot(mod, type = 'trace', which.items = 1:7, 
     main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4)) 

plot(mod, type = 'infotrace', which.items = 1:7, 
     main = "", par.settings = simpleTheme(lwd=2))


# model definition
onef_model <-  '
  F1 =~ i191 + i111 + i62 + i177
'

fit <- lavaan:::cfa(
  onef_model, 
  data = new_data,  
  ordered = names(new_data),
  std.lv = TRUE
)

summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# Assumption testing showed a very strong one factor solution and acceptable 
# Local Dependence (LD) (Yen's Q pairs were around .02 - .25).

good_items <- c("i191", "i111", "i62", "i177")
good_data <- new_data[, names(new_data) %in% good_items]

mod1 <- mirt(
  data= good_data, 
  1, itemtype="graded", SE=TRUE, verbose=FALSE)


fit <- mirt::itemfit(mod1)
fit

residuals(mod1, type = 'Q3', Theta=Theta)

# Local independence is the assumption that, conditional on the 
# latent variable(s), item responses are unrelated to one another 
# (i.e., independent). Local independence implies that the only 
# thing causing items to co-vary is the modeled latent variable(s).

mirt::residuals(mod1)
mirt::residuals(mod1, type = 'JSI', fold=FALSE) # unfolded
Theta <- mirt::fscores(mod)
residuals(mod1, type = 'Q3', Theta = Theta)
residuals(mod1, type = 'Q3', method = 'ML')

plot(mod, type = 'info', theta_lim = c(-4,4), lwd=2)                           
plot(mod, type = 'SE', theta_lim = c(-4,4), lwd=2)                           


alpha(new_data[, names(new_data) %in% c("i191", "i111", "i62", "i177")])
describe(new_data[, names(new_data) %in% c("i191", "i111", "i62", "i177")])




residuals(mod)







# One factor model - FUNZIONAMENTO FAMILIARE - RC5 --------------------------------------------------------


funzionamento_fam <- c("i6", "i36", "i178", "i15")

new_data <- complete_data_3[, (names(complete_data_3) %in% funzionamento_fam)]


# model definition
onef_model <-  '
F1 =~ NA*i6 + i36 + i178 + i15
# variances
F1 ~~ 1*F1
'


# fit the model
fit1 <- lavaan:::cfa(onef_model, data = new_data,  ordered = funzionamento_fam)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)



# One factor model - SUPPORTO SOCIALE - RC6 --------------------------------------------------------

items <- c("i98"
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
, "i38")


new_data <- complete_data[, (names(complete_data) %in% items)]

mod <- mirt(data=new_data, 1, itemtype="graded", SE=TRUE, verbose=FALSE)

n_item <- length(names(new_data))
plot(mod, type = 'trace', which.items = 1:n_item, 
     main = "", par.settings = simpleTheme(lty=1:4,lwd=2),
     auto.key=list(points=FALSE,lines=TRUE, columns=4)) 

plot(mod, type = 'infotrace', which.items = 1:n_item, 
     main = "", par.settings = simpleTheme(lwd=2))

good_items <- c("i186", "i130", "i100", "i66")
good_data <- new_data[, (names(new_data) %in% good_items)]


# model definition
onef_model <-  '
  F1 =~ i186 + i130 + i100 + i66
'

fit <- lavaan:::cfa(
  onef_model, 
  data = good_data,  
  ordered = names(good_data),
  std.lv = TRUE
)

summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Assumption testing showed a very strong one factor solution and acceptable 
# Local Dependence (LD) (Yen's Q pairs were around .02 - .25).

mod1 <- mirt(data=good_data, 1, itemtype="graded", SE=TRUE, verbose=FALSE)

fit <- mirt::itemfit(mod1)
fit

residuals(mod1, type = 'Q3', Theta=Theta)
mirt::residuals(mod1)
mirt::residuals(mod1, type = 'JSI', fold=TRUE) # unfolded

Theta <- fscores(mod1)
residuals(mod1, type = 'Q3', Theta=Theta)
residuals(mod1, type = 'Q3', method = 'ML')

eigen(cov(good_data))$values


ref2pl <- TAM::tam.mml.2pl(
  new_data, 
  irtmodel = "GPCM", 
  control = list(Msteps=10, QMC=FALSE, snodes=0, convD=.0001, 
                 conv=.00001, convM=.00001))


# Information Criteria to evaluate model fit
ref2pl$ic

summary(ref2pl)

#
# item & person fit----
ref.fit<-msq.itemfit(ref2pl) # mean square infit, outfit
ref.rmsd<-IRT.itemfit(ref2pl) # RMSD item fit

summary(ref.fit)
summary(ref.rmsd)

#
# local dependence: aQ3 correlations----
ref.res<-TAM::tam.modelfit(ref2pl)

ref.res$Q3_summary

ref.res$stat.itempair[1:12,c(1,2,6)] # 12 highest aQ3 correlations
ins.res$stat.itempair[1:12,c(1,2,6)] # 12 highest aQ3 correlations









# model definition
onef_model <-  '
F1 =~ NA*i18 + i130 + i98 + i100 + i186
# variances
F1 ~~ 1*F1
'


# fit the model
fit1 <- lavaan:::cfa(onef_model, data = new_data,  ordered = supporto_sociale)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)



mod  <- mirt::mirt(new_data, 1)

# ----GRM: Fit indices----
mod_fit <- mirt::M2(mod, type="C2")
mod_fit

# Assumption testing showed a very strong one factor solution and acceptable 
# Local Dependence (LD) (Yen's Q pairs were around .02 - .25).
fit <- itemfit(mod)
fit

mirt::residuals(mod)
mirt::residuals(mod, type = 'JSI', fold=TRUE) # unfolded

Theta <- fscores(mod)
residuals(mod, type = 'Q3', Theta=Theta)
residuals(mod, type = 'Q3', method = 'ML')


# One factor model - CARICO EMOTIVO - RC2 --------------------------------------------------------


carico_emotivo <- c("i116", "i48", "i127", "i83", "i81")

new_data <- complete_data_3[, (names(complete_data_3) %in% carico_emotivo)]


# model definition
onef_model <-  '
F1 =~ NA*i116 + i48 + i127 + i83 + i81
# variances
F1 ~~ 1*F1
'


# fit the model
fit1 <- lavaan:::cfa(onef_model, data = new_data,  ordered = carico_emotivo)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# One factor model - FATTORI INTRAPSICHICI/COGNIZIONE MALATTIA/ACCETTAZIONE E PADRONANZA - RC11 --------------------------------------------------------


accettazione_padronanza <- c("i147", "i34", "i179")   # i108

new_data <- complete_data_3[, (names(complete_data_3) %in% accettazione_padronanza)]


# model definition
onef_model <-  '
F1 =~ NA*i147 + i34 + i179 
# variances
F1 ~~ 1*F1
'


# fit the model
fit1 <- lavaan:::cfa(onef_model, data = new_data,  ordered = accettazione_padronanza)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)




# One factor model - STILE DI VITA NELLA MALATTIA - RC4 --------------------------------------------------------


stile_vita <- c("i164", "i165", "i126", "i40")

new_data <- complete_data_3[, (names(complete_data_3) %in% stile_vita)]


# model definition
onef_model <-  '
F1 =~ NA*i164 + i165 + i126 + i40
# variances
F1 ~~ 1*F1
'


# fit the model
fit1 <- lavaan:::cfa(onef_model, data = new_data,  ordered = stile_vita)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)



# One factor model - CONTENIMENTO EMOTIVO GENITORIALE - RC10 --------------------------------------------------------


contenimento_emotivo <- c("i188", "i167", "i22", "i85", "i63", "i47")
new_data <- complete_data_3[, (names(complete_data_3) %in% contenimento_emotivo)]


# model definition
onef_model <-  '
F1 =~ NA*i188 + i167 + i22 + i85 + i63 + i47
# variances
F1 ~~ 1*F1
'


# fit the model
fit1 <- lavaan:::cfa(onef_model, data = new_data,  ordered = contenimento_emotivo)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)



# One factor model - IPERPROTEZIONE - RC8 --------------------------------------------------------

# 
# iperprotezione <- c("i137", "i31")
# new_data <- complete_data_3[, (names(complete_data_3) %in% iperprotezione)]
# 
# 
# # model definition
# onef_model <-  '
# F1 =~ NA*i9 + i189 + i137 + i101 + i31
# # variances
# F1 ~~ 1*F1
# '
# 
# 
# # fit the model
# fit1 <- lavaan:::cfa(onef_model, data = new_data,  ordered = iperprotezione)
# 
# # look at the results
# summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# One factor model - FATTORI INTRAPSICHICI/COGNIZIONE DI MALATTIA/PERCEZIONE BENEFICI - RC7 --------------------------------------------------------


percezione_benefici <- c("i41", "i4", "i139")
new_data <- complete_data_3[, (names(complete_data_3) %in% percezione_benefici)]


# model definition
onef_model <-  '
F1 =~ NA*i41 + i4 + i139
# variances
F1 ~~ 1*F1
'


# fit the model
fit1 <- lavaan:::cfa(onef_model, data = new_data,  ordered = percezione_benefici)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)




# CFA with 8 factors ------------------------------------------------------



final_items <- c(
  cura_formale, autostima, funzionamento_fam, supporto_sociale, carico_emotivo,
  accettazione_padronanza, stile_vita, contenimento_emotivo, iperprotezione,
  percezione_benefici
)

final_data <- complete_data_2[, (names(complete_data_2) %in% final_items)]


# model definition
model1 <-  '
  F1 =~ NA*i143 + i28 + i87 + i176 + i114
  F2 =~ NA*i122 + i64 + i191 + i111
  F3 =~ NA*i6 + i36 + i178 + i15
  F4 =~ NA*i18 + i130 + i98 + i100 + i186
  F5 =~ NA*i116 + i48 + i127 + i83 + i81
  F6 =~ NA*i147 + i34 + i179 + i108
  F7 =~ NA*i164 + i165 + i126 + i40
  F8 =~ NA*i188 + i167 + i22 + i85 + i63 + i47
  F9 =~ NA*i9 + i189 + i137 + i101 + i31
  F10 =~ NA*i41 + i4 + i139
  
  # variances
  F1  ~~ 1*F1
  F2  ~~ 1*F2
  F3  ~~ 1*F3
  F4  ~~ 1*F4
  F5  ~~ 1*F5
  F6  ~~ 1*F6
  F7  ~~ 1*F7
  F8  ~~ 1*F8
  F9  ~~ 1*F9
  F10 ~~ 1*F10
'

fit1 <- lavaan:::cfa(model1, data = final_data)
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)



# model definition
model2 <-  '
  F1 =~ NA*i143 + i28 + i87 + i176 + i114
  F2 =~ NA*i122 + i64 + i191 + i111 + i147 + i34 + i179 + i108
  F3 =~ NA*i6 + i36 + i178 + i15
  F4 =~ NA*i18 + i130 + i98 + i100 + i186
  F5 =~ NA*i116 + i48 + i127 + i83 + i81
  F7 =~ NA*i164 + i165 + i126 + i40
  F8 =~ NA*i188 + i167 + i22 + i85 + i63 + i47
  F9 =~ NA*i9 + i189 + i137 + i101 + i31
  F10 =~ NA*i41 + i4 + i139
  
  # variances
  F1  ~~ 1*F1
  F2  ~~ 1*F2
  F3  ~~ 1*F3
  F4  ~~ 1*F4
  F5  ~~ 1*F5
  F7  ~~ 1*F7
  F8  ~~ 1*F8
  F9  ~~ 1*F9
  F10 ~~ 1*F10
'

# fit the model
fit2 <- lavaan:::cfa(model2, data = final_data)
summary(fit2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


anova(fit1, fit2)


# model definition
model3 <-  '
  F1 =~ NA*i143 + i28 + i87 + i176 + i114
  F2 =~ NA*i122 + i64 + i191 + i111 + i34 + i179 + i108
  F3 =~ NA*i6 + i36 + i178 + i15
  F4 =~ NA*i18 + i130 + i98 + i100 + i186
  F5 =~ NA*i116 + i48 + i127 + i83 + i81
  F7 =~ NA*i164 + i165 + i40
  F8 =~ NA*i167 + i22 + i85 + i63 + i47
  F9 =~ NA*i9 + i189 + i137 + i101 + i31
  F10 =~ NA*i41 + i4 + i139
  
  # variances
  F1  ~~ 1*F1
  F2  ~~ 1*F2
  F3  ~~ 1*F3
  F4  ~~ 1*F4
  F5  ~~ 1*F5
  F7  ~~ 1*F7
  F8  ~~ 1*F8
  F9  ~~ 1*F9
  F10 ~~ 1*F10
'

# fit the model
fit3 <- lavaan:::cfa(model3, data = final_data)
summary(fit3, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)



# model definition
model4 <-  '
  F1 =~ NA*i143 + i28 + i87 + i176 + i114
  F2 =~ NA*i122 + i64 + i191 + i111 + i34 + i179 + i108
  F3 =~ NA*i6 + i36 + i178 + i15
  F4 =~ NA*i18 + i130 + i98 + i100 + i186
  F5 =~ NA*i116 + i48 + i127 + i83 + i81
  F7 =~ NA*i164 + i165 + i40
  F8 =~ NA*i167 + i22 + i85 + i63 + i47
  F9 =~ NA*i137 + i101 + i31
  F10 =~ NA*i41 + i4 + i139
  
  # variances
  F1  ~~ 1*F1
  F2  ~~ 1*F2
  F3  ~~ 1*F3
  F4  ~~ 1*F4
  F5  ~~ 1*F5
  F7  ~~ 1*F7
  F8  ~~ 1*F8
  F9  ~~ 1*F9
  F10 ~~ 1*F10
'

# fit the model
fit4 <- lavaan:::cfa(model4, data = final_data)
summary(fit4, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

anova(fit1, fit2)




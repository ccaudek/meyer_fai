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
fai_s <- read_xlsx(here("data", "FAI_O_12_12_19.xlsx"), col_names = TRUE)


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


imputed_data <- mice(items, m = 1, maxit = 1, method = 'pmm', seed = 123)


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

out <- item.total(complete_data)


out1 <- out %>% 
  dplyr::filter(Item.Total > 0.3)
dim(out1)

good_items <- out1$Variable

complete_data_2 <- complete_data[, (names(complete_data) %in% good_items)]

  
# For normal distribution, skewness and kurtosis = 0 

# car::qqPlot(complete_data$i2)
# plot(density(complete_data$i3))
# hist(complete_data$i1)

# Select only items with good skewness and kurtosis
item_descript <- as.data.frame(describe(complete_data_2))
item_descript$item_number <- row.names(item_descript)



item_descript_2 <- item_descript[abs(item_descript$skew) < 1.2 & abs(item_descript$kurtosis) < 1.2, ]

good_items <- item_descript_2$item_number

complete_data_3 <- complete_data_2[, (names(complete_data_2) %in% good_items)]


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


# One factor model - FATTORI INTRAPSICHICI/AUTOSTIMA - RC3


autostima <- c("i122", "i64", "i191", "i111")

new_data <- complete_data_3[, (names(complete_data_3) %in% autostima)]


# model definition
onef_model <-  '
F1 =~ NA*i122 + i64 + i191 + i111
# variances
F1 ~~ 1*F1
'


# fit the model
fit1 <- lavaan:::cfa(onef_model, data = new_data,  ordered = autostima)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# One factor model - CURA FORMALE - RC1 --------------------------------------------------------


cura_formale <- c("i143", "i28", "i87", "i114", "i176")

new_data <- complete_data_3[, (names(complete_data_3) %in% cura_formale)]


# model definition
onef_model <-  '
F1 =~ NA*i143 + i28 + i87 + i114 + i176
# variances
F1 ~~ 1*F1
'


# fit the model
fit1 <- lavaan:::cfa(onef_model, data = new_data,  ordered = cura_formale)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)



# One factor model - FUNZIONAMENTO FAMILIARE - RC4 --------------------------------------------------------


funzionamento_fam <- c("i6", "i36", "i178", "i15", "i128")

new_data <- complete_data_3[, (names(complete_data_3) %in% funzionamento_fam)]


# model definition
onef_model <-  '
F1 =~ NA*i6 + i36 + i178 + i15 + i128
# variances
F1 ~~ 1*F1
'


# fit the model
fit1 <- lavaan:::cfa(onef_model, data = new_data,  ordered = funzionamento_fam)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# One factor model - SUPPORTO SOCIALE - RC6 --------------------------------------------------------


supporto_sociale <- c("i130", "i98", "i100", "i186", "i18")

new_data <- complete_data_3[, (names(complete_data_3) %in% supporto_sociale)]


# model definition
onef_model <-  '
F1 =~ NA*i130 + i98 + i100 + i186 + i18
# variances
F1 ~~ 1*F1
'


# fit the model
fit1 <- lavaan:::cfa(onef_model, data = new_data,  ordered = supporto_sociale)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)



# One factor model - CARICO EMOTIVO - RC2 --------------------------------------------------------


carico_emotivo <- c("i116", "i137", "i48", "i83", "i127")

new_data <- complete_data_3[, (names(complete_data_3) %in% carico_emotivo)]


# model definition
onef_model <-  '
F1 =~ NA*i116 + i137 + i48 + i83 + i127
# variances
F1 ~~ 1*F1
'


# fit the model
fit1 <- lavaan:::cfa(onef_model, data = new_data,  ordered = carico_emotivo)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)



# One factor model - FRATELLI - RC8 --------------------------------------------------------


fratelli <- c("i147", "i150", "i188", "i17")  

new_data <- complete_data_3[, (names(complete_data_3) %in% fratelli)]


# model definition
onef_model <-  '
F1 =~ NA*i147 + i150 + i188 + i17 
# variances
F1 ~~ 1*F1
'


# fit the model
fit1 <- lavaan:::cfa(onef_model, data = new_data,  ordered = fratelli)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)



# One factor model - contenimento emotivo - RC7 --------------------------------------------------------


contenimento_emotivo <- c("i132", "i63", "i47", "i154")
new_data <- complete_data_3[, (names(complete_data_3) %in% contenimento_emotivo)]


# model definition
onef_model <-  '
F1 =~ NA*i132 + i63 + i47 + i154
# variances
F1 ~~ 1*F1
'


# fit the model
fit1 <- lavaan:::cfa(onef_model, data = new_data,  ordered = contenimento_emotivo)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)



# CFA with 7 factors ------------------------------------------------------

final_items <- c(
  autostima, cura_formale, funzionamento_fam, supporto_sociale, carico_emotivo,
  contenimento_emotivo
)

final_data <- complete_data[, (names(complete_data) %in% final_items)]


# model definition
model1 <-  '
F1 =~ NA*i122 + i64 + i191 + i111
F2 =~ NA*i143 + i28 + i87 + i114 + i176
F3 =~ NA*i6 + i36 + i178 + i15 + i128
F4 =~ NA*i130 + i98 + i100 + i186 + i18
F5 =~ NA*i116 + i137 + i48 + i83 + i127
F7 =~ NA*i132 + i63 + i47 + i154


# variances
F1  ~~ 1*F1
F2  ~~ 1*F2
F3  ~~ 1*F3
F4  ~~ 1*F4
F5  ~~ 1*F5
F7  ~~ 1*F7
'

fit1 <- lavaan:::cfa(model1, data = final_data)
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


# model definition: model1 without F5 #


model2 <-  '
F1 =~ NA*i122 + i64 + i191 + i111
F2 =~ NA*i143 + i28 + i87 + i114 + i176
F3 =~ NA*i6 + i36 + i178 + i15 + i128
F4 =~ NA*i130 + i98 + i100 + i186 + i18
F6 =~ NA*i147 + i150 + i188 + i17
F7 =~ NA*i132 + i63 + i47 + i154


# variances
F1  ~~ 1*F1
F2  ~~ 1*F2
F3  ~~ 1*F3
F4  ~~ 1*F4
F6  ~~ 1*F6
F7  ~~ 1*F7
'

fit2 <- lavaan:::cfa(model2, data = final_data)
summary(fit2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

















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

options(max.print=1000000)


setwd("/Users/corrado/Dropbox/papers/meyer/src")


# read data
# fai_s <- read_xlsx("../data/FAI_S_11_05_18.xlsx")

fai_s <- read_xlsx("../data/FAI_S_Maggio.xlsx")
# glimpse(fai_s)

# correct wrong encoding!
fai_s$`FAI-S 162` <- as.numeric(fai_s$`FAI-S 162`)


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

paran_out <- paran(complete_data, iterations = 5000)


paran(complete_data, iterations = 5000, centile = 0, quietly = FALSE,
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE,
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE,
      file = "", seed = 0)
# Adjusted eigenvalues > 0 indicate dimensions to retain.
# (13 factors    retained)



# Item analysis  ----------------------------------------------------------

out <- item.total(complete_data)

out1 <- out %>% 
  dplyr::filter(Item.Total > 0.3)
dim(out1)



# For normal distribution, skewness and kurtosis = 0 

# Select only items with good skewness and kurtosis
item_descript <- describe(complete_data)
item_descript 

bad_items <- c("i1", "i7", "i13", "i24", "i32", "i53", "i74", "i77", "i138", 
               "i152", "i153", "i175", "i182", "i193", "i197")

complete_data_2 <- complete_data[, !(names(complete_data) %in% bad_items)]

item_descript <- describe(complete_data_2)
item_descript 


# pairs.panels(complete_data[, 1:10])

outlier(complete_data[, 1:10])

error.bars(complete_data_2)

r <- round(lowerCor(complete_data), 2)
corPlot(r) 
corr.test(complete_data[, 1:10])

fa.parallel(complete_data)
vss(complete_data)




# Exploratory FA ----------------------------------------------------------

good_items <- as.character(out1$Variable)

items_2 <- complete_data[, good_items]

paran_out <- paran(items_2, iterations = 5000)





R <- cor(items_2)

print(R, digits = 2)

fa_out <- principal(R, nfactors = 8, n.obs = 313, rotate = "promax")
fa_out

# fa_out <- fa(R, nfactors = 11, n.obs = 220, fm = "wls")

print(fa_out$loadings, cutoff = 0.4)



# One factor model --------------------------------------------------------


good_items <- c("i192", "i87", "i143", "i170", "i79")

new_data <- complete_data_2[, (names(complete_data_2) %in% good_items)]


# model definition
onef_model <-  '
  F1 =~ NA*i192 + i87 + i143 + i170 + i79
  # variances
  F1 ~~ 1*F1
'


# fit the model
fit1 <- lavaan:::cfa(onef_model, data = new_data,  ordered = good_items)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


####################################################
# fino a qui!!!



twof_model <-  '
  F1 =~ NA*FAI.S.79 + FAI.S.192 + FAI.S.87 + FAI.S.170 
  F2 =~ NA*FAI.S.25 + FAI.S.133 + FAI.S.11 + FAI.S.33 + FAI.S.129 + FAI.S.105 + FAI.S.50
  # variances
  F1 ~~ 1*F1
  F2 ~~ 1*F2
  F1 ~~ NA*F2
'













#-------------------------perceived cure-------------------------
#+ chunk_perceived_cure, cache=TRUE, warning=FALSE

# all the a-priori items that define the 'construct' perceived_cure
perc_cure <- with(complete_data,
  data.frame(`FAI-S 156`, `FAI-S 67`, `FAI-S 176`, `FAI-S 77`, `FAI-S 28`,
    `FAI-S 149`, `FAI-S 35`, `FAI-S 192`, `FAI-S 87`, `FAI-S 143`, `FAI-S 90`,
    `FAI-S 160`, `FAI-S 69`, `FAI-S 170`, `FAI-S 79`, `FAI-S 24`, `FAI-S 76`,
    `FAI-S 114`, `FAI-S 74`, `FAI-S 92`, `FAI-S 163`))

# check whether skew and kurtosis are adequate - delete items with values > |1.5|
describe(perc_cure)

# removed items: 77, 24, 76, 74
items_perc_cure <- with(complete_data,
                  data.frame(`FAI-S 156`, `FAI-S 67`, `FAI-S 176`, `FAI-S 28`,
                             `FAI-S 149`, `FAI-S 35`, `FAI-S 192`, `FAI-S 87`, `FAI-S 143`, `FAI-S 90`,
                             `FAI-S 160`, `FAI-S 69`, `FAI-S 170`, `FAI-S 79`, 
                             `FAI-S 114`, `FAI-S 92`, `FAI-S 163`))
describe(items_perc_cure)

names_perc_cure <- c("FAI.S.156", "FAI.S.67", "FAI.S.176", 
  "FAI.S.28", "FAI.S.149", "FAI.S.35", "FAI.S.192", "FAI.S.87", "FAI.S.143",
  "FAI.S.90", "FAI.S.160", "FAI.S.69", "FAI.S.170", "FAI.S.79", 
  "FAI.S.114", "FAI.S.92", "FAI.S.163")

# model definition
onef_model <-  '
  F1 =~ NA*FAI.S.192 + FAI.S.87 + FAI.S.143 +  
        FAI.S.170 + FAI.S.79 + 
        FAI.S.92 
  # variances
  F1 ~~ 1*F1
  '

# model definition
onef_model <-  '
F1 =~ NA*FAI.S.192 + FAI.S.87 +  
FAI.S.79 + 
FAI.S.92 
# variances
F1 ~~ 1*F1
'

# fit the model
fit1 <- lavaan:::cfa(onef_model, data = items_perc_cure,  ordered = names_perc_cure)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


##  ............................................................................
##  information quantity                                                    ####
#   select the items with the greatest item information (McDonald)
item_information <- parameterEstimates(fit1)$est[1:17]^2 / parameterEstimates(fit1)$est[87:103]
item_information

# names of selected items 
names_perc_cure_redux <- c("FAI.S.79", "FAI.S.192", "FAI.S.87", "FAI.S.170")

# data.frame with selected items
items_perc_cure_redux <- with(complete_data,
                              data.frame(`FAI-S 79`, `FAI-S 192`, `FAI-S 87`, `FAI-S 170`))


onef_model_redux <-  '
  F1 =~ NA*FAI.S.79 + FAI.S.192 + FAI.S.87 + FAI.S.170
  # variances
  F1 ~~ 1*F1
'

# fit the model
fit2 <- lavaan:::cfa(onef_model_redux, data = items_perc_cure_redux, ordered = names_perc_cure_redux)

# look at the results
summary(fit2, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)



#   ____________________________________________________________________________
#   caregiving request                                                      ####
#+  chunk_caregiving_request, cache=TRUE, warning=FALSE

# all the a-priori items that define the 'construct' caregiving_request
caregiving_request <- with(complete_data,
                  data.frame(`FAI-S 164`, `FAI-S 180`, `FAI-S 154`, `FAI-S 162`, `FAI-S 105`,
                             `FAI-S 40`, `FAI-S 196`, `FAI-S 8`, `FAI-S 134`, `FAI-S 131`, `FAI-S 43`,
                             `FAI-S 187`, `FAI-S 136`, `FAI-S 54`, `FAI-S 181`, `FAI-S 50`, `FAI-S 48`,
                             `FAI-S 25`, `FAI-S 133`, `FAI-S 11`, `FAI-S 33`, `FAI-S 129`, `FAI-S 169`,
                             `FAI-S 46`, `FAI-S 174`, `FAI-S 73`, `FAI-S 127`, `FAI-S 39`, `FAI-S 112`, 
                             `FAI-S 103`, `FAI-S 51`, `FAI-S 53`, `FAI-S 58`, `FAI-S 32`))

describe(caregiving_request)
# 162 174 73 51 58 32

caregiving_request <- with(complete_data,
                           data.frame(`FAI-S 164`, `FAI-S 180`, `FAI-S 154`,  `FAI-S 105`,
                                      `FAI-S 40`, `FAI-S 196`, `FAI-S 8`, `FAI-S 134`, `FAI-S 131`, `FAI-S 43`,
                                      `FAI-S 187`, `FAI-S 136`, `FAI-S 54`, `FAI-S 181`, `FAI-S 50`, `FAI-S 48`,
                                      `FAI-S 25`, `FAI-S 133`, `FAI-S 11`, `FAI-S 33`, `FAI-S 129`, `FAI-S 169`,
                                      `FAI-S 46`, `FAI-S 127`, `FAI-S 39`, `FAI-S 112`, 
                                      `FAI-S 103`))

describe(caregiving_request)




caregiving_request_item_names <- c("FAI.S.25", "FAI.S.133", "FAI.S.11", "FAI.S.33", "FAI.S.129","FAI.S.169", 
                                   "FAI.S.46")


# caregiving_request_item_names <- c("FAI.S.164", "FAI.S.180", "FAI.S.154", "FAI.S.105", "FAI.S.40","FAI.S.196", 
#                           "FAI.S.8", "FAI.S.134", "FAI.S.131", "FAI.S.43",  "FAI.S.187", "FAI.S.136", 
#                           "FAI.S.54", "FAI.S.181", "FAI.S.50",  "FAI.S.48",  "FAI.S.25",  "FAI.S.133",
#                           "FAI.S.11", "FAI.S.33",  "FAI.S.129", "FAI.S.169", "FAI.S.46", "FAI.S.127", "FAI.S.39",  
#                           "FAI.S.112", "FAI.S.103")

onef_model <-  '
F1 =~ NA*FAI.S.25 + FAI.S.133 +
      FAI.S.11 + FAI.S.33 + FAI.S.129 + 
      FAI.S.169 + FAI.S.46
      # variances
      F1 ~~ 1*F1
'

# fit the model
fit3 <- lavaan:::cfa(onef_model, data = caregiving_request,
                     ordered = caregiving_request_item_names)

# look at the results
summary(fit3, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)





##  ............................................................................
##  information quantity                                                    ####
#   select the items with the greatest item information (McDonald)
item_information <- parameterEstimates(fit3)$est[1:25]^2 / parameterEstimates(fit3)$est[127:151]
print(item_information, 2)


caregiving_request_redux <- with(
  complete_data,
          data.frame(`FAI-S 25`, `FAI-S 133`, `FAI-S 11`, `FAI-S 33`, `FAI-S 129`,
                                      `FAI-S 105`, `FAI-S 50`)
  )

caregiving_request_item_names_redux <- c("FAI.S.25", "FAI.S.133", "FAI.S.11", "FAI.S.33", "FAI.S.129",
                                         "FAI.S.105", "FAI.S.50")


onef_model <-  '
  F1 =~ NA*FAI.S.25 + FAI.S.133 + FAI.S.11 + FAI.S.33 + FAI.S.129 + FAI.S.105 + FAI.S.50 
  # variances
  F1 ~~ 1*F1
'

# fit the model
fit4 <- lavaan:::cfa(onef_model, data = caregiving_request_redux,
                     ordered = caregiving_request_item_names_redux)

# look at the results
summary(fit4, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


#   ____________________________________________________________________________
#   hyper-protection                                                        ####

# all the a-priori items that define the 'construct' caregiving_request
hyper_protection <- with(complete_data,
                           data.frame(`FAI-S 9`, `FAI-S 153`, `FAI-S 2`, `FAI-S 137`, `FAI-S 96`,
                                      `FAI-S 165`, `FAI-S 194`, `FAI-S 13`, `FAI-S 14`, `FAI-S 101`, 
                                      `FAI-S 144`, `FAI-S 31`, `FAI-S 151`, `FAI-S 159`, `FAI-S 182`, 
                                      `FAI-S 27`))

describe(hyper_protection)

hyper_protection <- with(complete_data,
                         data.frame(`FAI-S 9`, `FAI-S 2`, `FAI-S 137`, `FAI-S 96`,
                                    `FAI-S 165`, `FAI-S 194`, `FAI-S 14`, `FAI-S 101`, 
                                    `FAI-S 144`, `FAI-S 31`, `FAI-S 151`, `FAI-S 159`,  
                                    `FAI-S 27`))

describe(hyper_protection)

names_hyper_protection <- c("FAI.S.9", "FAI.S.2", "FAI.S.137", "FAI.S.96", "FAI.S.165", 
                            "FAI.S.194", "FAI.S.14", "FAI.S.101", "FAI.S.144",
                            "FAI.S.31", "FAI.S.151", "FAI.S.159", "FAI.S.27")

onef_model <-  '
  F1 =~ NA*FAI.S.137 + FAI.S.96 + FAI.S.101 + FAI.S.159
  # variances
  F1 ~~ 1*F1
  '

# fit the model
fit5 <- lavaan:::cfa(onef_model, data = hyper_protection, ordered = names_hyper_protection)

# look at the results
summary(fit5, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

##  ............................................................................
##  information quantity                                                    ####
#   select the items with the greatest item information (McDonald)
# item_information <- parameterEstimates(fit5)$est[1:25]^2 / parameterEstimates(fit5)$est[127:151]
# print(item_information, 2)

hyper_protection_redux <- with(complete_data,
                         data.frame(`FAI-S 137`, `FAI-S 96`, `FAI-S 159`, `FAI-S 151`))

names_hyper_protection_redux <- c("FAI.S.137", "FAI.S.96", "FAI.S.159", "FAI.S.151")

onef_model <-  '
  F1 =~ NA*FAI.S.137 + FAI.S.96 + FAI.S.159 + FAI.S.151
  # variances
  F1 ~~ 1*F1
'

fit6 <- lavaan:::cfa(onef_model, data = hyper_protection_redux, ordered = names_hyper_protection_redux)
summary(fit6, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


# ____________________________________________________________________________
#   siblings                                                      

# all the a-priori items that define the 'construct' caregiving_request
siblings <- with(complete_data,
                         data.frame(`FAI-S 188`, `FAI-S 17`, `FAI-S 184`, `FAI-S 147`, `FAI-S 109`,
                                    `FAI-S 37`, `FAI-S 171`, `FAI-S 78`, `FAI-S 126`, `FAI-S 84`, 
                                    `FAI-S 55`, `FAI-S 110`, `FAI-S 123`, `FAI-S 150`, `FAI-S 158`))

describe(siblings)

siblings <- with(complete_data,
                 data.frame(`FAI-S 188`, `FAI-S 17`, `FAI-S 147`, 
                            `FAI-S 37`, `FAI-S 171`, `FAI-S 126`, `FAI-S 84`, 
                            `FAI-S 55`, `FAI-S 110`, `FAI-S 123`, `FAI-S 150`, `FAI-S 158`))

describe(siblings)

names_siblings <- c("FAI.S.188", "FAI.S.17", "FAI.S.147", "FAI.S.37", "FAI.S.171", 
                            "FAI.S.126", "FAI.S.84", "FAI.S.55", "FAI.S.110",
                            "FAI.S.123", "FAI.S.150", "FAI.S.158")

onef_model <-  '
  F1 =~ NA*FAI.S.17 + FAI.S.147 + 
    FAI.S.110 + FAI.S.158
  # variances
  F1 ~~ 1*F1
'

# fit the model
fit5 <- lavaan:::cfa(onef_model, data = siblings, ordered = names_siblings)

# look at the results
summary(fit5, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)





# ____________________________________________________________________________
# social support                                                      

# all the a-priori items that define the 'construct' caregiving_request
social_support <- with(complete_data,
                 data.frame(`FAI-S 98`, `FAI-S 125`, `FAI-S 186`, `FAI-S 130`, `FAI-S 44`,
                            `FAI-S 172`, `FAI-S 195`, `FAI-S 66`, `FAI-S 190`, `FAI-S 18`, 
                            `FAI-S 94`, `FAI-S 80`, `FAI-S 117`, `FAI-S 100`, `FAI-S 38`))

describe(social_support)

social_support <- with(complete_data,
                       data.frame(`FAI-S 98`, `FAI-S 125`, `FAI-S 186`, `FAI-S 130`, 
                                  `FAI-S 172`, `FAI-S 195`, `FAI-S 66`, `FAI-S 190`, `FAI-S 18`, 
                                  `FAI-S 94`, `FAI-S 80`, `FAI-S 117`, `FAI-S 100`))

describe(social_support)

names_social_support <- c("FAI.S.98", "FAI.S.125", "FAI.S.186", "FAI.S.130", "FAI.S.172", 
                    "FAI.S.195", "FAI.S.66", "FAI.S.190", "FAI.S.18",
                    "FAI.S.94", "FAI.S.80", "FAI.S.117", "FAI.S.100")

onef_model <-  '
F1 =~ NA*FAI.S.125 + FAI.S.130 + FAI.S.195 + FAI.S.66 
# variances
F1 ~~ 1*F1
'

# fit the model
fit5 <- lavaan:::cfa(onef_model, data = social_support, ordered = names_social_support)

# look at the results
summary(fit5, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)








#   ____________________________________________________________________________
#   put all items together                                                  ####

good_items <- with(complete_data,
                   data.frame(`FAI-S 79`, `FAI-S 192`, `FAI-S 87`, `FAI-S 170`,
                     `FAI-S 25`, `FAI-S 133`, `FAI-S 11`, `FAI-S 33`, `FAI-S 129`,
                      `FAI-S 105`, `FAI-S 50`))


good_items_names <- c("FAI.S.79", "FAI.S.192", "FAI.S.87", "FAI.S.170", 
  "FAI.S.25", "FAI.S.133", "FAI.S.11", "FAI.S.33", "FAI.S.129",
  "FAI.S.105", "FAI.S.50")


onef_model <-  '
  F1 =~ NA*FAI.S.79 + FAI.S.192 + FAI.S.87 + FAI.S.170 + FAI.S.25 + FAI.S.133 + FAI.S.11 +
       FAI.S.33 + FAI.S.129 + FAI.S.105 + FAI.S.50
  # variances
  F1 ~~ 1*F1
'

fit5 <- lavaan:::cfa(onef_model, data = good_items,
                     ordered = good_items_names)

summary(fit5, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


twof_model <-  '
  F1 =~ NA*FAI.S.79 + FAI.S.192 + FAI.S.87 + FAI.S.170 
  F2 =~ NA*FAI.S.25 + FAI.S.133 + FAI.S.11 + FAI.S.33 + FAI.S.129 + FAI.S.105 + FAI.S.50
  # variances
  F1 ~~ 1*F1
  F2 ~~ 1*F2
  F1 ~~ NA*F2
'

fit6 <- lavaan:::cfa(twof_model, data = good_items, ordered = good_items_names)

summary(fit6, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)




coefH(perc_cure)


#' ## Investigation of latent monotonicity

mono <- check.monotonicity(perc_cure)
summary(mono)
plot(mono)

restscore_list <- check.restscore(perc_cure)
summary(restscore_list)
plot(restscore_list)

iio_list <- check.iio(perc_cure)
summary(iio_list)


scale_com <- search.normal(perc_cure)


#--------------------------------------------------------------------------------------------------
# select the items with the greatest item information (McDonald)
item_information <- parameterEstimates(fit1)$est[1:17]^2 / parameterEstimates(fit1)$est[104:120]
item_information

#--------------------------------------------------------------------------------------------------
# data set with only the 5 items with the greatest item information
perc_cura_part <- with(complete_data,
  data.frame(`FAI-S 28`, `FAI-S 192`, `FAI-S 87`, `FAI-S 69`, `FAI-S 79`))


#--------------------------------------------------------------------------------------------------
# model with only the selected items
one_factor_model_part <-  '
             F1 =~ NA*FAI.S.192 + FAI.S.87 + FAI.S.79 + FAI.S.69 + FAI.S.28
             # variances
             F1 ~~ 1*F1
             '

# fit the model
fit2 <- lavaan:::cfa(one_factor_model_part, data = perc_cura_part,
  ordered = c("FAI.S.192", "FAI.S.87", "FAI.S.79", "FAI.S.69", "FAI.S.28"))

# look at the results
summary(fit2, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)




#-------------------------mokken item selection-------------------------
#+ chunk_perceived_cure_mokken, cache=TRUE, warning=FALSE

library("mokken")

coefH(perc_cure)



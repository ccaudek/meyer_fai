# removed items: 77, 24, 76, 74
items_threef <- with(complete_data,
                        data.frame(`FAI-S 137`, `FAI-S 96`, `FAI-S 159`, `FAI-S 192`,
                                   `FAI-S 87`, `FAI-S 79`, `FAI-S 92`,
                                   `FAI-S 130`, `FAI-S 195`, `FAI-S 66`))
describe(items_perc_cure)

names_threef <- c("FAI.S.137", "FAI.S.96", "FAI.S.159", 
                "FAI.S.192", "FAI.S.87", "FAI.S.79", "FAI.S.92",
                "FAI.S.130", "FAI.S.195", "FAI.S.66")

# model definition
threef_model <-  '
  F1 =~ NA*FAI.S.137 + FAI.S.96 + FAI.S.159 
  F2 =~ NA*FAI.S.192 + FAI.S.87 + FAI.S.79 + FAI.S.92
  F3 =~ NA*FAI.S.130 + FAI.S.195 + FAI.S.66
  # variances
  F1 ~~ 1*F1
  F2 ~~ 1*F2
  F3 ~~ 1*F3
'

# fit the model
fit1 <- lavaan:::cfa(threef_model, data = items_threef,  ordered = names_threef)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)





temp <- items_threef[1:120, ]
# fit the model
fit1 <- lavaan:::cfa(threef_model, data = temp,  ordered = names_threef)

# look at the results
summary(fit1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)






out <- principal(cor(complete_data), nfactors = 11, n.obs = 220)
write.csv(print(out$loadings, 2), "loadings.csv")

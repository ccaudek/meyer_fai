
# Area 1
### New selections of the items

new_proposed_items <- c(
  "FAI_49", "FAI_60", "FAI_86",  
  "FAI_121",  "FAI_5", "FAI_85", 
  "FAI_81", "FAI_124", "FAI_106"
)

# Select only the items of this subscale.
good_items <- df %>% 
  dplyr::select(all_of(new_proposed_items))

mod <- mirt(
  data = good_items,
  1,
  itemtype = "graded",
  SE = TRUE,
  verbose = FALSE
)

n_item <- length(names(good_items))
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


# We also checked the fit indexes of a CFA model.
one_factor_model <-  '
  F1 =~ FAI_124 + FAI_106 + FAI_60 + FAI_49 + FAI_121
'

fit <- lavaan:::cfa(
  one_factor_model,
  data = good_items,
  ordered = names(good_items),
  std.lv = TRUE
)

# summary(fit, standardized = TRUE)
fitMeasures(
  fit, c("chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
         "rmsea", "rmsea.scaled", "srmr")
)


# We also checked the fit indexes of a CFA model.
one_factor_model <-  '
  F1 =~  FAI_86 + FAI_121 + FAI_5 + FAI_85 + FAI_81 
'

fit2 <- lavaan:::cfa(
  one_factor_model,
  data = df,
  ordered = names(df),
  std.lv = TRUE
)

fitMeasures(
  fit2, c("chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
         "rmsea", "rmsea.scaled", "srmr")
)

summary(fit2)


summary(psych::alpha(good_items))


# Area 2


new_proposed_items <- c(
  "FAI_162", "FAI_196", "FAI_48", "FAI_25",
  "FAI_133",  "FAI_129", "FAI_127", 
  "FAI_39", "FAI_103"
)

# Select only the items of this subscale.
good_items <- df %>% 
  dplyr::select(all_of(new_proposed_items))

mod <- mirt(
  data = good_items,
  1,
  itemtype = "graded",
  SE = TRUE,
  verbose = FALSE
)

n_item <- length(names(good_items))
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



# We also checked the fit indexes of a CFA model.
one_factor_model <-  '
  F1 =~  FAI_129 + FAI_133 + FAI_25 + FAI_103 
'

fit2 <- lavaan:::cfa(
  one_factor_model,
  data = df,
  ordered = names(df),
  std.lv = TRUE
)

fitMeasures(
  fit2, c("chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
          "rmsea", "rmsea.scaled", "srmr")
)

summary(fit2)


## Area 3

new_proposed_items <- c(
  "FAI_87", "FAI_79", "FAI_143", "FAI_192", "FAI_156", "FAI_90", "FAI_114"
)

# Select only the items of this subscale.
good_items <- df %>% 
  dplyr::select(all_of(new_proposed_items))


mod <- mirt(
  data = good_items,
  1,
  itemtype = "graded",
  SE = TRUE,
  verbose = FALSE
)

n_item <- length(names(good_items))
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



# We also checked the fit indexes of a CFA model.
one_factor_model <-  '
  F1 =~ FAI_87 + FAI_79 + FAI_143 + FAI_192 + FAI_156 + FAI_90 + FAI_114
'

fit2 <- lavaan:::cfa(
  one_factor_model,
  data = df,
  ordered = names(df),
  std.lv = TRUE
)

fitMeasures(
  fit2, c("chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
          "rmsea", "rmsea.scaled", "srmr")
)

summary(fit2)

## Area 4

new_proposed_items <- c(
  "FAI_111", "FAI_104", "FAI_42", "FAI_119", "FAI_93", "FAI_177", "FAI_75", 
  "FAI_108", "FAI_173", "FAI_45", "FAI_139", "FAI_83"   
)

# Select only the items of this subscale.
good_items <- df %>% 
  dplyr::select(all_of(new_proposed_items))


mod <- mirt(
  data = good_items,
  1,
  itemtype = "graded",
  SE = TRUE,
  verbose = FALSE
)

n_item <- length(names(good_items))
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


# We also checked the fit indexes of a CFA model.
one_factor_model <-  '
  F =~ FAI_111 + FAI_104 + FAI_42 + FAI_119 
'

fit2 <- lavaan:::cfa(
  one_factor_model,
  data = df,
  ordered = names(df),
  std.lv = TRUE
)

fitMeasures(
  fit2, c("chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
          "rmsea", "rmsea.scaled", "srmr")
)

summary(fit2)


## Area 5

new_proposed_items <- c(
  "FAI_128", "FAI_115", "FAI_7", "FAI_155", "FAI_195", "FAI_80", "FAI_36",
  "FAI_72", "FAI_102", "FAI_16", "FAI_29", "FAI_157", "FAI_26"
)


# Select only the items of this subscale.
good_items <- df %>% 
  dplyr::select(all_of(new_proposed_items))


mod <- mirt(
  data = good_items,
  1,
  itemtype = "graded",
  SE = TRUE,
  verbose = FALSE
)

n_item <- length(names(good_items))
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



# We also checked the fit indexes of a CFA model.
one_factor_model <-  '
  F =~ FAI_128 + FAI_115 + FAI_7 + FAI_155 + FAI_195 + FAI_80 +
       FAI_36 + FAI_16
'

fit2 <- lavaan:::cfa(
  one_factor_model,
  data = df,
  ordered = names(df),
  std.lv = TRUE
)

fitMeasures(
  fit2, c("chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
          "rmsea", "rmsea.scaled", "srmr")
)

summary(fit2)



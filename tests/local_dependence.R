## ------------------------------------------------------------------
## Filename.R
## 
## Project: 
## Purpose: 
## Author: Corrado Caudek
## Date: 
## ------------------------------------------------------------------


item_set <- c("i9", "i153",  "i2", "i137", "i96", "i165", "i194", "i13", "i14",
              "i101", "i144", "i31", "i151", "i159", "i182", "i27")

new_data <- complete_data[, (names(complete_data) %in% item_set)]

mod  <- mirt::mirt(new_data, 1)

mod_fit <- mirt::M2(mod, type="C2")
mod_fit

# Assumption testing showed a very strong one factor solution and acceptable 
# Local Dependence (LD) (Yen's Q pairs were around .02 - .25).
fit <- itemfit(mod)
fit

mirt::residuals(mod)
mirt::residuals(mod, type = 'JSI', fold=FALSE) # unfolded
Theta <- fscores(mod)
residuals(mod, type = 'Q3', Theta=Theta)
residuals(mod, type = 'Q3', method = 'ML')


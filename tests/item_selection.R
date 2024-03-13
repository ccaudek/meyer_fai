## ------------------------------------------------------------------
## Filename.R
## 
## Project: 
## Purpose: 
## Author: Corrado Caudek
## Date: 
## ------------------------------------------------------------------

library("tidyverse")
library("irtplay")
library("mirt")




## example 1.
## simulates response data with a mixed-format test.
## for the first two polytomous items, the generalized partial credit model is used
## for the last polytomous item, the graded response model is used
# 100 examinees are sampled
theta <- rnorm(100)

# set item parameters for three dichotomous items with the 3PL model
a.dc <- c(1, 1.2, 1.3); b.dc <- c(-1, 0, 1); g.dc <- rep(0.2, 3)

# set item parameters for three polytomous item parameters
# note that 4, 4, and 5 categories are used for polytomous items
a.py <- c(1.3, 1.2, 1.7)
d.py <- list(c(-1.2, -0.3, 0.4), c(-0.2, 0.5, 1.6), c(-1.7, 0.2, 1.1, 2.0))

# create a numeric vector of score categories for both dichotomous and polytomous item data
# this score category vector is used to specify the location of the polytomous items
cats <- c(2, 2, 4, 4, 5, 2)

# create a character vector of the IRT model for the polytomous items
pmodel <- c('GPCM', 'GPCM', 'GPCM')

# simulate the response data
d <- simdat(theta=theta, a.dc=a.dc, b.dc=b.dc, g.dc=NULL,
       a.py=a.py, d.py=d.py, cats=cats, pmodel=pmodel, D=1)

d <- data.frame(d)
d <- d[, c(1, 2, 3)]

names(d) <- c("i1", "i2", "i3", "i4", "i5", "i6")

mod  <- mirt::mirt(d, 1, itemtype = 'graded', SE = TRUE)


# ----GRM: Fit indices----
mod_fit <- mirt::M2(mod, type="C2")
mod_fit


# ----Local independence: JSI----

# Strongly related to the assumption of unidimensionality is that of 
# local independence. Local independence implies that, conditional on 
# the latent variable(s), item responses are unrelated. The version of local i
# JSI

mod <- mirt::mirt(d, 1, itemtype = 'graded')

residuals(mod, type = 'Q3', method = 'ML')

# Assumption testing showed a very strong one factor solution and acceptable 
# Local Dependence (LD) (Yen's Q pairs were around .02 - .25).
fit <- itemfit(mod)
fit

mirt::residuals(mod)
mirt::residuals(mod, type = 'JSI', fold=FALSE) # unfolded


residuals(mod, type = 'JSI')


local_indep_jsi <- lapply(mod.pcm, function(model){
  mirt::residuals(model, type="JSI", df.p=T, fold=TRUE)
})

local_indep_jsi_df <- lapply(local_indep_jsi, function(model){matrix_flatten_upper(model)})
local_indep_jsi_df <- tibble::as_tibble(dplyr::bind_rows(local_indep_jsi_df, .id = "country"))
local_indep_jsi_df$row <- stringr::str_sub(local_indep_jsi_df$row,-1,-1)
local_indep_jsi_df$column <- stringr::str_sub(local_indep_jsi_df$column,-1,-1)
# JSI-Cutoff: Mean(JSI) + 2*SD(JSI)
library(tidyverse)
jsi_mean_sd <- local_indep_jsi_df %>%
  summarise(mean=mean(statistic), sd=sd(statistic), jsi_crit=mean+(2*sd))
local_indep_jsi_df <- dplyr::left_join(local_indep_jsi_df, jsi_mean_sd, by="country")
# Determine Critical value
local_indep_jsi_df$crit <- NA
local_indep_jsi_df$crit[(local_indep_jsi_df$statistic <= local_indep_jsi_df$jsi_crit)] <- "0"
local_indep_jsi_df$crit[(local_indep_jsi_df$statistic > local_indep_jsi_df$jsi_crit)] <- "1"
# Heatmap
library(ggplot2)
ggplot(local_indep_jsi_df, aes(x=row, y=column, fill= crit)) + 
  geom_tile() +
  geom_text(aes(x=row, y=column, label=round(statistic, 2)), color="black", size=3) +
  geom_text(aes(x=3, y=1, label=paste0("JSI-Cutoff: ", round(jsi_crit, 2))), color="black", size=3) +
  facet_wrap(~country) +
  scale_fill_manual(name="JSI", labels=c("below", "above"), values=c("grey90", "red", show.legend = FALSE)) +
  theme_bw(base_size=15, base_family = "serif") +
  theme(axis.title=element_blank(), legend.position="none")




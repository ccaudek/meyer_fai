## ------------------------------------------------------------------
## fai_fnct.R
## 
## Project: 
## Purpose: 
## Author: Corrado Caudek
## Date: 
## ------------------------------------------------------------------


recode_demo_info <- function(fai_s) {
  
  # Select demographic information
  demo_info <- fai_s[, 1:50]
  # Change columns names 
  new_demo_info_names <- c(
    "id1", "id2", "date_code", "relationship", "child_age", "child_sex", 
    "child_birthdate", "child_nationality", "child_nationality_code", 
    "child_birth_place", "child_birth_place_code", "brothers", "sisters", 
    "has_chronic_disease", "chronic_disease", "child_other_problems", 
    "child_other_chronic_disease", "child_psychological_problems", 
    "hospitalization", "hospitalization_number", "has_emergency_care", 
    "emergency_care_number", "mother_age", "mother_nationality",
    "mother_nationality_code", "mother_marital_status", "mother_education",
    "mother_job", "mother_job_code", "mother_194", "father_age", 
    "father_nationality", "father_nationality_code", "father_marital_status", 
    "father_education", "father_job", "father_job_code", "father_194", 
    "address", "address_other_region", "social_services", "death_loved_one", 
    "divorce", "bad_health", "job_change", "low_income", "change_address", 
    "change_city", "other", "other_code"
  )
  names(demo_info) <- new_demo_info_names
  
  # Recoding demographic information
  
  demo_info$has_chronic_disease <- case_when(
    demo_info$has_chronic_disease == 1 ~ 1,
    demo_info$has_chronic_disease == 2 ~ 0,
    TRUE ~ demo_info$has_chronic_disease
  )
  
  demo_info$child_other_problems <- case_when(
    demo_info$child_other_problems == 1 ~ 1,
    demo_info$child_other_problems == 2 ~ 0,
    TRUE ~ demo_info$child_other_problems
  )
  
  demo_info$child_psychological_problems <- case_when(
    demo_info$child_psychological_problems == 1 ~ 1,
    demo_info$child_psychological_problems == 2 ~ 0,
    TRUE ~ demo_info$child_psychological_problems
  )
  
  # In the last 6 months, your son...
  demo_info$hospitalization <- case_when(
    demo_info$hospitalization == 1 ~ 1,
    demo_info$hospitalization == 2 ~ 0,
    TRUE ~ demo_info$hospitalization
  )
  
  demo_info$has_emergency_care <- case_when(
    demo_info$has_emergency_care == 1 ~ 1,
    demo_info$has_emergency_care == 2 ~ 0,
    TRUE ~ demo_info$has_emergency_care
  )
  
  demo_info <- demo_info %>% 
    dplyr::mutate(
      hospitalization_number = replace_na(hospitalization_number, 0),
      emergency_care_number = replace_na(emergency_care_number, 0)
    )
  
  x <- as.character(demo_info$mother_age)
  xx <- ifelse(x == "mancano le informazioni sulla madre", NA, x)
  xxx <- as.numeric(as.character(xx))
  demo_info$mother_age <- xxx
  
  demo_info$is_mother_italian <- demo_info$mother_nationality
  demo_info$mother_nationality <- NULL
  demo_info$is_mother_italian <- case_when(
    demo_info$is_mother_italian == 1 ~ 1,
    demo_info$is_mother_italian == 2 ~ 0,
    TRUE ~ demo_info$is_mother_italian
  )
  
  # father
  x <- as.character(demo_info$father_nationality)
  xx <- ifelse(x == "deceduto", "1", x)
  xxx <- as.numeric(as.character(xx))
  demo_info$father_nationality <- xxx
  
  demo_info$is_father_italian <- demo_info$father_nationality
  demo_info$father_nationality <- NULL
  demo_info$is_father_italian <- case_when(
    demo_info$is_father_italian == 1 ~ 1,
    demo_info$is_father_italian == 2 ~ 0,
    TRUE ~ demo_info$is_father_italian
  )
  
  # Correct coding error
  demo_info$mother_marital_status <- 
    ifelse(demo_info$mother_marital_status == 6, 2, 
           demo_info$mother_marital_status)
  
  demo_info$mother_marital_status <- 
    factor(demo_info$mother_marital_status)
  
  # Replace the 6 NAs with the most likely value
  demo_info <- demo_info %>% 
    dplyr::mutate(
      mother_marital_status = replace_na(mother_marital_status, "2")
    )
  
  demo_info$mother_marital_status <- as.character(demo_info$mother_marital_status)
  demo_info$mother_marital_status <- dplyr::recode(
    demo_info$mother_marital_status,
    "1" = "single",
    "2" = "relation",
    "3" = "divorced",
    "4" = "widow"
  )
  demo_info$mother_marital_status <- factor(demo_info$mother_marital_status)
  
  # father
  x <- as.character(demo_info$father_marital_status)
  xx <- ifelse(x == "deceduto", "2", x)
  xxx <- as.numeric(as.character(xx))
  demo_info$father_marital_status <- xxx
  
  demo_info$father_marital_status <- as.character(demo_info$father_marital_status)
  demo_info$father_marital_status <- dplyr::recode(
    demo_info$father_marital_status,
    "1" = "single",
    "2" = "relation",
    "3" = "divorced",
    "4" = "widower"
  )
  demo_info$father_marital_status <- factor(demo_info$father_marital_status)
  
  # is mother working
  demo_info$is_mother_working <- demo_info$mother_job
  # Replace the 9 NAs with 'working'
  demo_info <- demo_info %>% 
    dplyr::mutate(
      is_mother_working = replace_na(is_mother_working, 7)
    )
  demo_info$is_mother_working <- 
    ifelse(demo_info$is_mother_working == 1, 0, 1)
  
  # is father working
  demo_info$is_father_working <- demo_info$father_job
  # Replace the NAs with 'working'
  demo_info <- demo_info %>% 
    dplyr::mutate(
      is_father_working = replace_na(is_father_working, 7)
    )
  demo_info$is_father_working <- 
    ifelse(demo_info$is_father_working == 1, 0, 1)
  
  # Life events in the last 12 months
  
  # Replace NAs with 0 for the life events occurrences
  demo_info <- demo_info %>% 
    dplyr::mutate(
      death_loved_one = replace_na(death_loved_one, 0),
      divorce = replace_na(divorce, 0),
      bad_health = replace_na(bad_health, 0),
      job_change = replace_na(job_change, 0),
      low_income = replace_na(low_income, 0),
      change_address = replace_na(change_address, 0)
    )
  
  demo_info$death_loved_one <- 
    ifelse(demo_info$death_loved_one == 1, 1, 0)
  demo_info$divorce <- 
    ifelse(demo_info$divorce == 1, 1, 0)
  demo_info$bad_health <- 
    ifelse(demo_info$bad_health == 1, 1, 0)
  demo_info$job_change <- 
    ifelse(demo_info$job_change == 1, 1, 0)
  demo_info$job_change <- 
    ifelse(demo_info$job_change == 1, 1, 0)
  demo_info$low_income <- 
    ifelse(demo_info$low_income == 1, 1, 0)
  demo_info$change_address <- 
    ifelse(demo_info$change_address == 1, 1, 0)
  
  demo_info <- demo_info %>% 
    dplyr::mutate(
      life_events = death_loved_one + divorce + bad_health + 
        job_change + low_income + change_address
    )
  
  good_demo_info_names <- c(
    "id1",
    "child_age", "child_sex", "child_nationality",        
    "brothers", "sisters", "has_chronic_disease", "chronic_disease",
    "child_other_problems", "child_other_chronic_disease", 
    "child_psychological_problems",
    "hospitalization_number", "emergency_care_number",      
    "mother_age", "mother_marital_status", "mother_education",         
    "father_age", "father_marital_status", "father_education",
    "is_mother_italian", "is_father_italian",       
    "is_mother_working", "is_father_working",    
    "life_events"          
  )
  
  demo_info2 <- demo_info %>% 
    dplyr::select(
      all_of(good_demo_info_names)
    )
  
  # -0.5 : male; 0.5 : female
  demo_info2$child_sex <- ifelse(demo_info2$child_sex == 1, -0.5, 0.5)
  
  demo_info2 <- demo_info2 %>% 
    dplyr::mutate(
      child_nationality = replace_na(child_nationality, 1)
    )
  demo_info2$is_child_italian <- demo_info2$child_nationality
  demo_info2$child_nationality <- NULL
  demo_info2$is_child_italian <- ifelse(
    demo_info2$is_child_italian == 1, 1, 0
  )
  
  # siblings: replace NAs with 1
  demo_info2$siblings <- demo_info2$brothers + demo_info2$sisters
  demo_info2 <- demo_info2 %>% 
    dplyr::mutate(
      siblings = replace_na(siblings, 1)
    )
  demo_info2$brothers <- NULL
  demo_info2$sisters <- NULL
  
  demo_info2 <- demo_info2 %>% 
    dplyr::mutate(
      has_chronic_disease = replace_na(has_chronic_disease, 1)
    )
  
  demo_info2 <- demo_info2 %>% 
    dplyr::mutate(
      child_other_problems = replace_na(child_other_problems, 0)
    )
  
  demo_info2 <- demo_info2 %>% 
    dplyr::mutate(
      child_other_problems = replace_na(child_other_problems, 0)
    )
  
  demo_info2 <- demo_info2 %>% 
    dplyr::mutate(
      child_psychological_problems = replace_na(child_psychological_problems, 0)
    )
  
  # multiple imputation
  demo_num <- select_if(demo_info2, is.numeric) 
  imp <- mice::mice(demo_num, method = "norm.predict", m = 1) 
  temp <- round(complete(imp))
  demo_not_num <- select_if(demo_info2, negate(is.numeric))
  demo_info3 <- bind_cols(demo_not_num, temp)
  
  # Use demo id1 to classify cases into death-risk or not
  death_risk_categories <- c(
    "FC", "MM", "NEUROLOGIA", "NEURO-ONCO", "ONCO", "RIA"
  )
  
  demo_info3$death_risk <- ifelse(
    demo_info3$id1 %in% death_risk_categories, 1, 0
  )
  
  demo_info3
}


plot_ridges <- function(data, ncolumns) {
  
  d_ridge <- pivot_longer(
    data
    , cols=1:ncolumns
    , names_to="Item"
    , values_to="Response"
  )
  
  d_ridge$Item <- as.factor(d_ridge$Item)
  
  # changes the order of the factor levels
  # sri_ridge <- sri_ridge %>%
  #   mutate(ItemNew = fct_relevel(SRIS.Item,"ins08","ins07","ins06","ins05","ins04","ins03","ins02","ins01",
  #                                "sr12","sr11","sr10","sr09","sr08","sr07","sr06","sr05","sr04","sr03",
  #                                "sr02","sr01"))
  
  if (ncolumns < 21) {
    ridges <- ggplot(
      d_ridge, 
      aes(x = Response, y = Item)
      ) +
      geom_density_ridges(
        stat="binline"
        , bins=5
        , rel_min_height=.01
        , scale=.9
        , alpha=.85
        , show.legend = FALSE
        , aes(color=Item, fill=Item)
      ) +
      scale_color_paletteer_d("cartography::harmo.pal", direction=1, dynamic=TRUE) +
      scale_fill_paletteer_d("cartography::harmo.pal", direction=-1, dynamic=TRUE) +
      labs(
        x="Scale Response", y="Item"
      ) +
      coord_cartesian(clip="off") +
      scale_x_continuous(expand = c(.01, -1), breaks=c(1,2,3,4,5)) +
      scale_y_discrete(expand = expansion(mult = c(0.01, .01))) +
      theme(axis.text.y = element_text(vjust = 0))
  } else {
    ridges <- ggplot(
      d_ridge, aes(x = Response, y = Item)) +
      geom_density_ridges(stat="binline", bins=5, rel_min_height=.01, scale=.9, 
                          alpha=.85, show.legend = FALSE, 
                          aes(color=Item, fill=Item)) +
      # scale_color_paletteer_d("cartography::harmo.pal", direction=1, dynamic=TRUE) +
      # scale_fill_paletteer_d("cartography::harmo.pal", direction=-1, dynamic=TRUE) +
      labs(x="Scale Response", y="Item") +
      coord_cartesian(clip="off") +
      scale_x_continuous(expand = c(.01, -1), breaks=c(1,2,3,4,5)) +
      scale_y_discrete(expand = expansion(mult = c(0.01, .01))) +
      #theme_ridges(font_family = "Fira Sans Medium", font_size = 12) +
      theme(axis.text.y = element_text(vjust = 0))
  }
  
  print(ridges)
  
}


# Item difficulty figure --------------------------------------------

plot_item_difficulty = function(good_items_2pl, n_items) {
  
  sr.dat = good_items_2pl$item_irt
  sr.dat$item = seq.int(1, n_items, 1)
  
  p = ggdotchart(
    sr.dat, x="item", y="beta", size=3,
    color = "#F88080FF",
    dot.size = 3.5, rotate = FALSE,
    xlab = "Items", ylab = "Difficulty",
    title="Sub-scale") 
    
    print(p)
}


# Fit 2PL IRT Generalized Partial Credit Model ----------------------

get_2pl_indices <- function(d) {
  # Fit 2PL IRT Generalized Partial Credit Model
  subscale_2pl <- TAM::tam.mml.2pl(
    d
    , irtmodel = "GPCM"
    , control = list(Msteps = 10, QMC = FALSE, snodes = 0,
                     convD = .0001, conv = .00001, convM = .00001)
  )

  subscale_fit <- msq.itemfit(subscale_2pl) # mean square infit, outfit
  outfit <- subscale_fit$itemfit[, c(1, 3)]
  infit <- subscale_fit$itemfit[, c(1, 6)]
  
  subscale_rmsd <- IRT.itemfit(subscale_2pl) # RMSD item fit
  rmsd <- subscale_rmsd$RMSD %>% 
    arrange(Group1)
  
  list(
    outfit = outfit,
    infit = infit,
    rmsd = rmsd
  )
}



local_dependence <- function(d) {
  
  subscale_2pl <- TAM::tam.mml.2pl(
    d
    , irtmodel = "GPCM"
    , control = list(Msteps = 10, QMC = FALSE, snodes = 0,
                     convD = .0001, conv = .00001, convM = .00001)
  )
  
  subscale_res <- TAM::tam.modelfit(subscale_2pl)
  subscale_res$Q3_summary
  # A cut-off of |.25| is used to flag the most notably dependent item pairs.
  
  round(subscale_res$aQ3.matr, 3)
  
}


# IRT a and b -------------------------------------------------------

irt_ab <- function(d) {
  subscale_2pl <- TAM::tam.mml.2pl(
    d
    , irtmodel = "GPCM"
    , control = list(Msteps = 10, QMC = FALSE, snodes = 0,
                     convD = .0001, conv = .00001, convM = .00001)
  )
  
  list(
    alpha = subscale_2pl$item_irt[, c(1, 2)],
    beta = subscale_2pl$item_irt[, c(1, 3)]
  )
}


plot_iif <- function(d) {
  
  mod <- mirt(
    data = d
    , 1
    , itemtype = "graded"
    , SE = TRUE
    , verbose = FALSE
  )
  
  n_item <- length(names(d))
  
  p <- plot(
    mod
    , type = 'infotrace'
    , which.items = 1:n_item
    , main = ""
    , par.settings = simpleTheme(lwd = 2)
  )
  print(p)
  
}

# CFA 

gof_indices_cfa <- function(d, one_factor_model) {
  
  fit <- lavaan:::cfa(
    one_factor_model
    , data = d
    , ordered = names(d)
    , std.lv = TRUE
  )
  
  # summary(fit, standardized = TRUE)
  gof <- fitMeasures(
    fit, c("chisq", "df", "cfi", "cfi.scaled", "tli", "tli.scaled",
           "rmsea", "rmsea.scaled", "srmr")
  )
  
  gof
}


# plot_tif() --------------------------------------------------------

plot_tif <- function(d) {
  mod <- mirt(
    data = d
    , 1
    , itemtype = "graded"
    , SE = TRUE
    , verbose = FALSE
  )
  
  plot(mod, type = 'info', theta_lim = c(-4,4), lwd=2) 
}


# plot_se() --------------------------------------------------------

plot_se <- function(d) {
  mod <- mirt(
    data = d
    , 1
    , itemtype = "graded"
    , SE = TRUE
    , verbose = FALSE
  )
  
  plot(mod, type = 'SE', theta_lim = c(-4,4), lwd=2) 
}


                          

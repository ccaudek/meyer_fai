
#' @details Perform a LPA with the careless responding indices. Adds a column 
#' to the input data.frame with FLAG_1 ("keep", "delete") to flag the 
#' participants belonging to the two classes characterized by extreme values on  
#' the careless responding indices.
#' 
#' @import nothing
#' @return data.frame
#' 
flag_careless_responding_LPA <- function(dat) {
  
  library("here")
  suppressPackageStartupMessages(library("tidyverse"))
  library("forcats")
  library("readxl")
  library("mice")
  library("careless")
  library("tidyLPA")
  
  options(max.print = 999999)
  
  set.seed(12345)
  
  # # Read data.
  # all_items <- readRDS(
  #   file = here(
  #     "data", "processed", "all_items",
  #     "final_complete_rescue_workers_controls_data.Rds"
  #   )
  # )
  
  # Compute careless statistics indices.
  preds_mahad <- mahad(dat, plot = FALSE, flag = FALSE, confidence = 0.99)
  temp <- longstring(dat, avg = TRUE)
  longstr <- temp[, 1]
  avgstr <- temp[2]
  cm <- colMeans(dat)
  ptc <- apply(dat, 1, function(x) cor(x, cm))
  preds_psychsyn <- psychsyn(dat)
  # preds_psychant <- psychant(dat)
  # preds_psychant <- ifelse(is.na(preds_psychant), 0, preds_psychant)
  irv_total <- irv(dat)
  
  d <- cbind(preds_mahad, longstr, avgstr, ptc, preds_psychsyn, irv_total)
  
  # LPA analysis on careless responding measures.
  d %>%
    scale() %>%
    estimate_profiles(1:6) %>%
    compare_solutions(statistics = c("AIC", "BIC"))

  mod <- d %>%
    scale() %>%
    estimate_profiles(6)
  
  by_subj_vals <- get_data(mod)
  table(by_subj_vals$Class)
  #   1   2   3   4   5   6 
  # 165   6  89 199  19  16 
  
  m6 <- d %>%
    scale() %>%
    estimate_profiles(6)
  
  by_subj_vals <- get_data(m6)
  table(by_subj_vals$Class)
  # 1   2   3   4   5   6 
  # 22  95   2 233  20 122 
  
  get_estimates(m6) %>%
    as.data.frame()
  #     Category      Parameter     Estimate         se            p Class Model Classes
  # 1      Means    preds_mahad  0.254568379 0.17971603 1.566282e-01     1     1       6
  # 2      Means        longstr -0.041097644 0.29805159 8.903292e-01     1     1       6
  # 3      Means         avgstr -0.142282145 0.22672269 5.302921e-01     1     1       6
  # 4      Means            ptc -0.119512804 0.22029221 5.874607e-01     1     1       6
  # 5      Means preds_psychsyn -0.307955630 0.18941570 1.039882e-01     1     1       6
  # 6      Means      irv_total  0.042797060 0.13059648 7.431351e-01     1     1       6
  # 7  Variances    preds_mahad  0.255139060 0.04044446 2.819695e-10     1     1       6
  # 8  Variances        longstr  0.574506559 0.15715764 2.565742e-04     1     1       6
  # 9  Variances         avgstr  0.415772199 0.10214800 4.695424e-05     1     1       6
  # 10 Variances            ptc  0.535350647 0.04863739 3.536730e-28     1     1       6
  # 11 Variances preds_psychsyn  0.845662560 0.07679025 3.321637e-28     1     1       6
  # 12 Variances      irv_total  0.355467780 0.04396125 6.168919e-16     1     1       6
  # 13     Means    preds_mahad -0.899980206 0.55445083 1.045483e-01     2     1       6
  # 14     Means        longstr  0.266256744 0.56842117 6.394882e-01     2     1       6
  # 15     Means         avgstr -0.070771595 0.54888492 8.974075e-01     2     1       6
  # 16     Means            ptc -0.386627068 0.24896234 1.204341e-01     2     1       6
  # 17     Means preds_psychsyn  2.026654561 0.45888764 1.003348e-05     2     1       6
  # 18     Means      irv_total  2.233815387 0.83120804 7.200369e-03     2     1       6
  # 19 Variances    preds_mahad  0.255139060 0.04044446 2.819695e-10     2     1       6
  # 20 Variances        longstr  0.574506559 0.15715764 2.565742e-04     2     1       6
  # 21 Variances         avgstr  0.415772199 0.10214800 4.695424e-05     2     1       6
  # 22 Variances            ptc  0.535350647 0.04863739 3.536730e-28     2     1       6
  # 23 Variances preds_psychsyn  0.845662560 0.07679025 3.321637e-28     2     1       6
  # 24 Variances      irv_total  0.355467780 0.04396125 6.168919e-16     2     1       6
  # 25     Means    preds_mahad  0.973164939 0.17735926 4.088862e-08     3     1       6
  # 26     Means        longstr -0.372661957 0.06964576 8.755633e-08     3     1       6
  # 27     Means         avgstr -0.496354699 0.05792328 1.042289e-17     3     1       6
  # 28     Means            ptc -0.503326726 0.19073765 8.318949e-03     3     1       6
  # 29     Means preds_psychsyn  0.570172242 0.22742630 1.217387e-02     3     1       6
  # 30     Means      irv_total  1.172914249 0.18930406 5.794279e-10     3     1       6
  # 31 Variances    preds_mahad  0.255139060 0.04044446 2.819695e-10     3     1       6
  # 32 Variances        longstr  0.574506559 0.15715764 2.565742e-04     3     1       6
  # 33 Variances         avgstr  0.415772199 0.10214800 4.695424e-05     3     1       6
  # 34 Variances            ptc  0.535350647 0.04863739 3.536730e-28     3     1       6
  # 35 Variances preds_psychsyn  0.845662560 0.07679025 3.321637e-28     3     1       6
  # 36 Variances      irv_total  0.355467780 0.04396125 6.168919e-16     3     1       6
  # 37     Means    preds_mahad -0.844024663 0.11449280 1.682664e-13     4     1       6
  # 38     Means        longstr -0.085690318 0.07075180 2.258418e-01     4     1       6
  # 39     Means         avgstr  0.036636622 0.09623852 7.034365e-01     4     1       6
  # 40     Means            ptc  0.636803670 0.06297941 4.922685e-24     4     1       6
  # 41     Means preds_psychsyn -0.005181049 0.08258206 9.499750e-01     4     1       6
  # 42     Means      irv_total -0.728609610 0.10766278 1.310119e-11     4     1       6
  # 43 Variances    preds_mahad  0.255139060 0.04044446 2.819695e-10     4     1       6
  # 44 Variances        longstr  0.574506559 0.15715764 2.565742e-04     4     1       6
  # 45 Variances         avgstr  0.415772199 0.10214800 4.695424e-05     4     1       6
  # 46 Variances            ptc  0.535350647 0.04863739 3.536730e-28     4     1       6
  # 47 Variances preds_psychsyn  0.845662560 0.07679025 3.321637e-28     4     1       6
  # 48 Variances      irv_total  0.355467780 0.04396125 6.168919e-16     4     1       6
  # 49     Means    preds_mahad  2.454974525 0.27603473 5.909857e-19     5     1       6
  # 50     Means        longstr -0.017314472 0.45236849 9.694683e-01     5     1       6
  # 51     Means         avgstr -0.196313949 0.28874898 4.965820e-01     5     1       6
  # 52     Means            ptc -2.378228546 0.33795457 1.962468e-12     5     1       6
  # 53     Means preds_psychsyn -0.595284471 0.35589819 9.440100e-02     5     1       6
  # 54     Means      irv_total  1.556574126 0.31364556 6.946823e-07     5     1       6
  # 55 Variances    preds_mahad  0.255139060 0.04044446 2.819695e-10     5     1       6
  # 56 Variances        longstr  0.574506559 0.15715764 2.565742e-04     5     1       6
  # 57 Variances         avgstr  0.415772199 0.10214800 4.695424e-05     5     1       6
  # 58 Variances            ptc  0.535350647 0.04863739 3.536730e-28     5     1       6
  # 59 Variances preds_psychsyn  0.845662560 0.07679025 3.321637e-28     5     1       6
  # 60 Variances      irv_total  0.355467780 0.04396125 6.168919e-16     5     1       6
  # 61     Means    preds_mahad -0.407637529 0.36712827 2.668522e-01     6     1       6
  # 62     Means        longstr  3.453688713 1.75969877 4.968580e-02     6     1       6
  # 63     Means         avgstr  4.000426441 1.89825791 3.508120e-02     6     1       6
  # 64     Means            ptc -0.678247662 0.23437431 3.805315e-03     6     1       6
  # 65     Means preds_psychsyn  0.005214516 0.69074652 9.939767e-01     6     1       6
  # 66     Means      irv_total -0.779272419 0.50990189 1.264432e-01     6     1       6
  # 67 Variances    preds_mahad  0.255139060 0.04044446 2.819695e-10     6     1       6
  # 68 Variances        longstr  0.574506559 0.15715764 2.565742e-04     6     1       6
  # 69 Variances         avgstr  0.415772199 0.10214800 4.695424e-05     6     1       6
  # 70 Variances            ptc  0.535350647 0.04863739 3.536730e-28     6     1       6
  # 71 Variances preds_psychsyn  0.845662560 0.07679025 3.321637e-28     6     1       6
  # 72 Variances      irv_total  0.355467780 0.04396125 6.168919e-16     6     1       6
  
  (6+19+16) / nrow(dat)
  # [1] 0.08299595
  
  by_subj_vals$index <- 1:nrow(dat)
  by_subj_vals$FLAG <- ifelse(
    by_subj_vals$Class == 3 | by_subj_vals$Class == 5, 
    "delete", "keep"
  )
  
  dat$FLAG <- by_subj_vals$FLAG
  
  dat
  
}

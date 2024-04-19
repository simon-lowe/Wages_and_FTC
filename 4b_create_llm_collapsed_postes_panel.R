rm(list = ls())
gc()

# Load packages -----------------------------------------------------------

list.of.packages <- c(
  "tidyverse", "data.table", "haven", "fst", "expss", "janitor", "here",
  "DescTools", "tictoc", "collapse",
  "fixest", "binsreg", "did", "ggplot2", "patchwork"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(list.of.packages, library, character.only = TRUE))
rm(list.of.packages, new.packages)

setDTthreads(0)
setFixest_nthreads(8)



# Load data ---------------------------------------------------------------

dat <- read_fst(here("Data", "ss_dads_hiring_panel_1014.fst"), as.data.table = TRUE)

dat <- dat[hwr != Inf]

dat[, siren := substr(siret, 1, 9)]

# Prepare outcome variable ------------------------------------------------

dat[, l_hwr := log(hwr)]
dat[, l_hwr_agg := log(hwr_agg)]

# Trimming
trim_level <- 0.001
dat[, l_hwr_w2 := ifelse(between(l_hwr, quantile(l_hwr, trim_level, na.rm = TRUE), quantile(l_hwr, 1-trim_level, na.rm = TRUE)), 
                         l_hwr, NA)]
dat[, l_hwr_agg_w2 := ifelse(between(l_hwr_agg, quantile(l_hwr_agg, trim_level, na.rm = TRUE), quantile(l_hwr_agg, 1-trim_level, na.rm = TRUE)), 
                             l_hwr_agg, NA)]

# Residualized wages ------------------------------------------------------

# # Year, age and sex
# reg <- feols(data = dat, l_hwr_agg_w2 ~ 1 | year + age_bin + sexe)
# dat[!is.na(l_hwr_agg_w2), l_hwr_res1 := reg$residuals]
# 
# # Add experience in occupation
# reg <- feols(data = dat, l_hwr_agg_w2 ~ 1 | year + age_bin + sexe + exp_cs_bin)
# dat[!is.na(l_hwr_agg_w2), l_hwr_res2 := reg$residuals]
# 
# # Add occupation
# reg <- feols(data = dat, l_hwr_agg_w2 ~ 1 | year + age_bin + sexe + exp_cs_bin + pcs)
# dat[!is.na(l_hwr_agg_w2), l_hwr_res3 := reg$residuals]

# Add industry (but without occupation)
reg <- feols(data = dat, l_hwr_agg_w2 ~ 1 | year + age_bin + sexe + exp_cs_bin + a38)
dat[!is.na(l_hwr_agg_w2), l_hwr_res4 := reg$residuals]

# # Add individual fixed-effect
# reg <- feols(data = dat, l_hwr_agg_w2 ~ 1 | ident_all + year + age_bin + exp_cs_bin + a38)
# dat[!is.na(l_hwr_agg_w2), l_hwr_res5 := reg$residuals]

# Collapse data -----------------------------------------------------------


dat2 <- dat[, .(n_cdd = sum(cdd), n = .N,
               m_lhw = mean(l_hwr_agg_w2, na.rm = TRUE), m_lhw_cdi = mean(l_hwr_agg_w2[cdd == FALSE], na.rm = TRUE), m_lhw_cdd = mean(l_hwr_agg_w2[cdd == TRUE], na.rm = TRUE),
               m_lhw_res1 = mean(l_hwr_res1, na.rm = TRUE), m_lhw_res1_cdi = mean(l_hwr_res1[cdd == FALSE], na.rm = TRUE),
               m_lhw_res1_cdd = mean(l_hwr_res1[cdd == TRUE], na.rm = TRUE),
               m_lhw_res2 = mean(l_hwr_res2, na.rm = TRUE), m_lhw_res2_cdi = mean(l_hwr_res2[cdd == FALSE], na.rm = TRUE),
               m_lhw_res2_cdd = mean(l_hwr_res2[cdd == TRUE], na.rm = TRUE),
               m_lhw_res3 = mean(l_hwr_res3, na.rm = TRUE), m_lhw_res3_cdi = mean(l_hwr_res3[cdd == FALSE], na.rm = TRUE),
               m_lhw_res3_cdd = mean(l_hwr_res3[cdd == TRUE], na.rm = TRUE),
               m_lhw_res4 = mean(l_hwr_res4, na.rm = TRUE), m_lhw_res4_cdi = mean(l_hwr_res4[cdd == FALSE], na.rm = TRUE),
               m_lhw_res4_cdd = mean(l_hwr_res4[cdd == TRUE], na.rm = TRUE),
               m_lhw_res5 = mean(l_hwr_res5, na.rm = TRUE), m_lhw_res5_cdi = mean(l_hwr_res5[cdd == FALSE], na.rm = TRUE),
               m_lhw_res5_cdd = mean(l_hwr_res5[cdd == TRUE], na.rm = TRUE)), by = .(zempt10, pcs, year)]

write.fst(dat2, here("Data", "ss_panel_1014_collapse_pcsLLM_hiring_wage.fst"))

rm(dat2)

dat2 <- dat[, .(n_cdd = sum(cdd), n = .N,
                m_lhw = mean(l_hwr_agg_w2, na.rm = TRUE), m_lhw_cdi = mean(l_hwr_agg_w2[cdd == FALSE], na.rm = TRUE), m_lhw_cdd = mean(l_hwr_agg_w2[cdd == TRUE], na.rm = TRUE),
                m_lhw_res1 = mean(l_hwr_res1, na.rm = TRUE), m_lhw_res1_cdi = mean(l_hwr_res1[cdd == FALSE], na.rm = TRUE),
                m_lhw_res1_cdd = mean(l_hwr_res1[cdd == TRUE], na.rm = TRUE),
                m_lhw_res2 = mean(l_hwr_res2, na.rm = TRUE), m_lhw_res2_cdi = mean(l_hwr_res2[cdd == FALSE], na.rm = TRUE),
                m_lhw_res2_cdd = mean(l_hwr_res2[cdd == TRUE], na.rm = TRUE),
                m_lhw_res3 = mean(l_hwr_res3, na.rm = TRUE), m_lhw_res3_cdi = mean(l_hwr_res3[cdd == FALSE], na.rm = TRUE),
                m_lhw_res3_cdd = mean(l_hwr_res3[cdd == TRUE], na.rm = TRUE),
                m_lhw_res4 = mean(l_hwr_res4, na.rm = TRUE), m_lhw_res4_cdi = mean(l_hwr_res4[cdd == FALSE], na.rm = TRUE),
                m_lhw_res4_cdd = mean(l_hwr_res4[cdd == TRUE], na.rm = TRUE),
                m_lhw_res5 = mean(l_hwr_res5, na.rm = TRUE), m_lhw_res5_cdi = mean(l_hwr_res5[cdd == FALSE], na.rm = TRUE),
                m_lhw_res5_cdd = mean(l_hwr_res5[cdd == TRUE], na.rm = TRUE)), by = .(zempt10, cs_clean, year)]

write.fst(dat2, here("Data", "ss_panel_1014_collapse_csLLM_hiring_wage.fst"))


dat2 <- dat[, .(n_cdd = sum(cdd), n = .N,
                m_lhw = mean(l_hwr_agg_w2, na.rm = TRUE), m_lhw_cdi = mean(l_hwr_agg_w2[cdd == FALSE], na.rm = TRUE), 
                m_lhw_cdd = mean(l_hwr_agg_w2[cdd == TRUE], na.rm = TRUE),
                m_lhw_res4 = mean(l_hwr_res4, na.rm = TRUE), m_lhw_res4_cdi = mean(l_hwr_res4[cdd == FALSE], na.rm = TRUE),
                m_lhw_res4_cdd = mean(l_hwr_res4[cdd == TRUE], na.rm = TRUE)), by = .(zempt10, cs_clean)]

write.fst(dat2, here("Data", "ss_panel_1014_collapse_csLLM_hiring_wage_ya.fst"))
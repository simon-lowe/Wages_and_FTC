rm(list = ls())
gc()

# Load packages -----------------------------------------------------------

list.of.packages <- c(
  "tidyverse", "data.table", "haven", "fst", "expss", "janitor", "here",
  "DescTools", "tictoc", "collapse",
  "fixest", "binsreg", "did", "ggplot2", "patchwork",
  "gtools"
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
dat <- dat[a38 != "" & a38 != "00"]

dat[, siren := substr(siret, 1, 9)]


# Add financial data ------------------------------------------------------

# Financial data
firm_fin <- read_fst(here("Data", "firm_fin_panel.fst"), as.data.table = TRUE)
firm_fin <- firm_fin[between(year, 2010, 2014)]
firm_fin[, siren := str_pad(siren, 9, pad = "0")]

firm_fin[, vacf_bw := ifelse(effsalm > 0, vacf/effsalm, NA)]
firm_fin[, vaht_bw := ifelse(effsalm > 0, vaht/effsalm, NA)]

dat <- merge(dat, firm_fin, by = c("year", "siren"), all = FALSE)

dat <- dat[!(is.na(vaht_bw) | is.nan(vaht_bw))]

# Trimming the VA variables
trim_level <- 0.01
dat[, vaht_bw_t := ifelse(between(vaht_bw, quantile(vaht_bw, trim_level, na.rm = TRUE), quantile(vaht_bw, 1-trim_level, na.rm = TRUE)), 
                          vaht_bw, NA)]

# Taking the log
dat[, l_vaht_bw := log(vaht_bw_t)]

# Restricting
dat <- dat[!is.na(l_vaht_bw) & l_vaht_bw != -Inf]


# DPAE information --------------------------------------------------------

# Load data
dpae_dat <- read_fst(here("Data", "dpae_col_hiring_siren_drop2.fst"), as.data.table = TRUE)

dpae_dat[, f_conv_cdi := ifelse(n_cdi_h > 0, n_conv_cdi/n_cdi_h, NA)]
dpae_dat[, f_conv_cdd := ifelse(n_cdd_h > 0, n_conv_cdd/n_cdd_h, NA)]
dpae_dat[, f_cdd := ifelse(n_h > 0, n_cdd_h/n_h, -NA)]

dat <- merge(dat, dpae_dat, by = c("year", "siren"), all = FALSE)

rm(dpae_dat)
gc()

# Drop NAs in FTC length
dat <- dat[!is.na(m_cdd_dur)]

# Prepare outcome variable ------------------------------------------------

dat[, l_hwr := log(hwr)]
dat[, l_hwr_agg := log(hwr_agg)]

# Trimming
trim_level <- 0.001
dat[, l_hwr_w2 := ifelse(between(l_hwr, quantile(l_hwr, trim_level, na.rm = TRUE), quantile(l_hwr, 1-trim_level, na.rm = TRUE)), 
                         l_hwr, NA)]
dat[, l_hwr_agg_w2 := ifelse(between(l_hwr_agg, quantile(l_hwr_agg, trim_level, na.rm = TRUE), quantile(l_hwr_agg, 1-trim_level, na.rm = TRUE)), 
                             l_hwr_agg, NA)]


# Create variables --------------------------------------------------------

dat[, size_bin := quantcut(eff_3112_et, q = 10, na.rm = TRUE)]
dat <- dat[!is.na(size_bin)]

# Residualized wages ------------------------------------------------------

reg <- feols(data = dat, l_hwr_agg_w2 ~ 1 | year + age_bin + sexe + exp_cs_bin + pcs + a38 + size_bin)
dat[!is.na(l_hwr_agg_w2), l_hwr_res4 := reg$residuals]


# Residualized VA ----------------------------------------------------------

reg <- feols(data = dat, l_vaht_bw ~ 1 | year + age_bin + sexe + exp_cs_bin + pcs + a38 + size_bin)
dat[!is.na(l_vaht_bw), l_vaht_bw_res4 := reg$residuals]

# Residualized FTC duration ----------------------------------------------------------

reg <- feols(data = dat, m_cdd_dur ~ 1 | year + age_bin + sexe + exp_cs_bin + pcs + a38 + size_bin)
dat[!is.na(m_cdd_dur), m_cdd_dur_res4 := reg$residuals]

# Residualized fraction of FTC ----------------------------------------------------------

reg <- feols(data = dat, f_cdd ~ 1 | year + age_bin + sexe + exp_cs_bin + pcs + a38 + size_bin)
dat[!is.na(f_cdd), f_cdd_res4 := reg$residuals]


# Age bin collapse data ---------------------------------------------------

age_bin_col <- dcast(data = dat[, .(siren, year, age_bin)][, age_bin := paste0("age_bin_", age_bin)], siren + year ~ age_bin, fun.aggregate = length)

# # sexe collapse data ---------------------------------------------------
# 
# sexe_col <- dcast(data = dat[, .(siren, year, sexe)][, sexe := paste0("sexe_", sexe)], siren + year ~ sexe, fun.aggregate = length)
# 
# # exp_cs_bin collapse data ---------------------------------------------------
# 
# exp_cs_bin_col <- dcast(data = dat[, .(siren, year, exp_cs_bin)][, exp_cs_bin := paste0("exp_cs_bin_", exp_cs_bin)], siren + year ~ exp_cs_bin, fun.aggregate = length)
# 
# # cs_clean collapse data ---------------------------------------------------
# 
# cs_col <- dcast(data = dat[, .(siren, year, cs_clean)][, cs := paste0("cs_", cs_clean)], siren + year ~ cs, fun.aggregate = length)

# Collapse data -----------------------------------------------------------


dat2 <- dat[, .(n_cdd = sum(cdd), n = .N,
                a38 = a38[1], zempt10 = zempt10[1], dept = dept[1], eff_3112_et = eff_3112_et[1],
                m_lhw = mean(l_hwr_agg_w2, na.rm = TRUE), m_lhw_cdi = mean(l_hwr_agg_w2[cdd == FALSE], na.rm = TRUE), m_lhw_cdd = mean(l_hwr_agg_w2[cdd == TRUE], na.rm = TRUE),
                m_lhw_res4 = mean(l_hwr_res4, na.rm = TRUE), m_lhw_res4_cdi = mean(l_hwr_res4[cdd == FALSE], na.rm = TRUE),
                m_lhw_res4_cdd = mean(l_hwr_res4[cdd == TRUE], na.rm = TRUE),
                m_l_vaht_bw = mean(l_vaht_bw, na.rm = TRUE),
                m_l_vaht_bw_res4 = mean(l_vaht_bw_res4, na.rm = TRUE),
                m_cdd_dur = mean(m_cdd_dur, na.rm = TRUE),
                m_cdd_dur_res4 = mean(m_cdd_dur_res4, na.rm = TRUE),
                f_cdd = mean(f_cdd, na.rm = TRUE),
                f_cdd_res4 = mean(f_cdd_res4, na.rm = TRUE)
), by = .(siren, cs_clean, year)]

# dat <- merge(dat2, age_bin_col, by = c("siren", "year"))
# dat <- merge(dat, sexe_col, by = c("siren", "year"))
# dat <- merge(dat, exp_cs_bin_col, by = c("siren", "year"))
# dat <- merge(dat, cs_col, by = c("siren", "year"))

write.fst(dat2, here("Data", "ss_panel_1014_collapseSiren_hiring_wage_siren_cs.fst"))
rm(dat2)



dat2 <- dat[, .(n_cdd = sum(cdd), n = .N,
               a38 = a38[1], zempt10 = zempt10[1], dept = dept[1], eff_3112_et = eff_3112_et[1],
               m_lhw = mean(l_hwr_agg_w2, na.rm = TRUE), m_lhw_cdi = mean(l_hwr_agg_w2[cdd == FALSE], na.rm = TRUE), m_lhw_cdd = mean(l_hwr_agg_w2[cdd == TRUE], na.rm = TRUE),
               m_lhw_res4 = mean(l_hwr_res4, na.rm = TRUE), m_lhw_res4_cdi = mean(l_hwr_res4[cdd == FALSE], na.rm = TRUE),
               m_lhw_res4_cdd = mean(l_hwr_res4[cdd == TRUE], na.rm = TRUE),
               m_l_vaht_bw = mean(l_vaht_bw, na.rm = TRUE),
               m_l_vaht_bw_res4 = mean(l_vaht_bw_res4, na.rm = TRUE),
               m_cdd_dur = mean(m_cdd_dur, na.rm = TRUE),
               m_cdd_dur_res4 = mean(m_cdd_dur_res4, na.rm = TRUE),
               f_cdd = mean(f_cdd, na.rm = TRUE),
               f_cdd_res4 = mean(f_cdd_res4, na.rm = TRUE)
               ), by = .(siren, year)]

# dat <- merge(dat2, age_bin_col, by = c("siren", "year"))
# dat <- merge(dat, sexe_col, by = c("siren", "year"))
# dat <- merge(dat, exp_cs_bin_col, by = c("siren", "year"))
# dat <- merge(dat, cs_col, by = c("siren", "year"))

write.fst(dat2, here("Data", "ss_panel_1014_collapseSiren_hiring_wage_v3.fst"))
rm(dat2)


dat2 <- dat[between(age, 30, 60), .(n_cdd = sum(cdd), n = .N,
                a38 = a38[1], zempt10 = zempt10[1], dept = dept[1], eff_3112_et = eff_3112_et[1],
                m_lhw = mean(l_hwr_agg_w2, na.rm = TRUE), m_lhw_cdi = mean(l_hwr_agg_w2[cdd == FALSE], na.rm = TRUE), m_lhw_cdd = mean(l_hwr_agg_w2[cdd == TRUE], na.rm = TRUE),
                m_lhw_res4 = mean(l_hwr_res4, na.rm = TRUE), m_lhw_res4_cdi = mean(l_hwr_res4[cdd == FALSE], na.rm = TRUE),
                m_lhw_res4_cdd = mean(l_hwr_res4[cdd == TRUE], na.rm = TRUE),
                m_l_vaht_bw = mean(l_vaht_bw, na.rm = TRUE),
                m_l_vaht_bw_res4 = mean(l_vaht_bw_res4, na.rm = TRUE),
                m_cdd_dur = mean(m_cdd_dur, na.rm = TRUE),
                m_cdd_dur_res4 = mean(m_cdd_dur_res4, na.rm = TRUE),
                f_cdd = mean(f_cdd, na.rm = TRUE),
                f_cdd_res4 = mean(f_cdd_res4, na.rm = TRUE)
                ), by = .(siren, year)]

# dat <- merge(dat2, age_bin_col, by = c("siren", "year"))
# dat <- merge(dat, sexe_col, by = c("siren", "year"))
# dat <- merge(dat, exp_cs_bin_col, by = c("siren", "year"))
# dat <- merge(dat, cs_col, by = c("siren", "year"))

write.fst(dat2, here("Data", "ss_panel_1014_collapseSiren_hiring_wage_v3_age_rest.fst"))
rm(dat2)


rm(list = ls())
gc()

# Load packages -----------------------------------------------------------

list.of.packages <- c(
  "tidyverse", "data.table", "haven", "fst", "expss", "janitor", "here",
  "DescTools", "tictoc", "collapse",
  "fixest", "binsreg", "did", "ggplot2", "patchwork",
  "igraph"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(list.of.packages, library, character.only = TRUE))
rm(list.of.packages, new.packages)

setDTthreads(0)
setFixest_nthreads(8)



# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# 2010 - 2014 -------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Load data ---------------------------------------------------------------

dat <- read_fst(here("Data", "ss_dads_hiring_panel_1014.fst"), as.data.table = TRUE)

dat <- dat[hwr != Inf]

# Prepare outcome variable ------------------------------------------------

dat[, l_hwr := log(hwr)]
dat[, l_hwr_agg := log(hwr_agg)]

# Trimming
trim_level <- 0.001
dat[, l_hwr_w2 := ifelse(between(l_hwr, quantile(l_hwr, trim_level, na.rm = TRUE), quantile(l_hwr, 1-trim_level, na.rm = TRUE)), 
                         l_hwr, NA)]
dat[, l_hwr_agg_w2 := ifelse(between(l_hwr_agg, quantile(l_hwr_agg, trim_level, na.rm = TRUE), quantile(l_hwr_agg, 1-trim_level, na.rm = TRUE)), 
                             l_hwr_agg, NA)]

# A21 variable
dat[, a21 := substr(a38, 1, 1)]
dat[, cddu_a21 := a21 %in% c("A", "C", "F", "H", "I", "J", "K", "M", "N", "P", "Q", "R")]

# Regressions -------------------------------------------------------------
gc()

reg <- feols(data = dat, l_hwr_agg_w2 ~ cdd | year + csw0(sexe, age_bin, a38, pcs, exp_cs_bin, siret, ident_all), lean = TRUE, cluster = ~siret)

etable(reg, title = "FTC-OEC wage gap regression",
       dict = c(cddTRUE = "FTC", l_hwr_agg_w2 = "Log hourly wage", year = "Year",
                sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.",a38 = "Industry", pcs = "Occupation", 
                siret = "Establishment", ident_all = "Individual"),
       # cluster = ~siret,
       tex = TRUE, file = here("Results", "Average wage gap", "table_cdd_reg_hiringwage.tex"), style.tex = style.tex("aer"))

rm(reg)
gc()

reg <- feols(data = dat, l_hwr_agg_w2 ~ cdd | year + csw0(sexe, age_bin, a38, cs_clean, exp_cs_bin, siret, ident_all), lean = TRUE, cluster = ~siret)

etable(reg, title = "FTC-OEC wage gap regression",
       dict = c(cddTRUE = "FTC", l_hwr_agg_w2 = "Log hourly wage", year = "Year",
                sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.",a38 = "Industry", cs_clean = "Occupation (2 digit)", 
                siret = "Establishment", ident_all = "Individual"),
       # cluster = ~siret,
       tex = TRUE, file = here("Results", "Average wage gap", "table_cdd_reg_hiringwage_occ2.tex"), style.tex = style.tex("aer"))

rm(reg)
gc()

# reg <- feols(data = dat, l_hwr_w2 ~ cdd | year + csw0(sexe, age_bin, a38, pcs, exp_cs_bin, siret, ident_all), lean = TRUE, cluster = ~siret)
reg <- feols(data = dat, l_hwr_w2 ~ cdd | year + csw0(sexe, age_bin, a38, pcs, siret), lean = TRUE, cluster = ~siret)

etable(reg, title = "FTC-OEC wage gap regression",
       dict = c(cddTRUE = "FTC", l_hwr_w2 = "Log hourly wage", year = "Year",
                sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.", a38 = "Industry", pcs = "Occupation", 
                siret = "Establishment", ident_all = "Individual"),
       tex = TRUE, file = here("Results", "Average wage gap", "table_cdd_reg_hiringwage_v2comp.tex"), style.tex = style.tex("aer"))

rm(reg)
gc()


# Age restriction ---------------------------------------------------------

reg <- feols(data = dat[between(age, 30, 60)], l_hwr_agg_w2 ~ cdd | year + csw0(sexe, age_bin, a38, pcs, exp_cs_bin, siret), lean = TRUE, cluster = ~siret)

etable(reg, title = "FTC-OEC wage gap regression, age restriction 30 - 60",
       dict = c(cddTRUE = "FTC", l_hwr_agg_w2 = "Log hourly wage", year = "Year",
                sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.",a38 = "Industry", pcs = "Occupation", 
                siret = "Establishment", ident_all = "Individual"),
       # cluster = ~siret,
       tex = TRUE, file = here("Results", "Average wage gap", "table_cdd_reg_hiringwage_age3060.tex"), style.tex = style.tex("aer"))

rm(reg)
gc()

reg <- feols(data = dat[between(age, 30, 70)], l_hwr_agg_w2 ~ cdd | year + csw0(sexe, age_bin, a38, pcs, exp_cs_bin, siret), lean = TRUE, cluster = ~siret)

etable(reg, title = "FTC-OEC wage gap regression, age restriction 30+",
       dict = c(cddTRUE = "FTC", l_hwr_agg_w2 = "Log hourly wage", year = "Year",
                sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.", a38 = "Industry", pcs = "Occupation", 
                siret = "Establishment", ident_all = "Individual"),
       # cluster = ~siret,
       tex = TRUE, file = here("Results", "Average wage gap", "table_cdd_reg_hiringwage_age30plus.tex"), style.tex = style.tex("aer"))

rm(reg)
gc()


# Firm heterogeneity regression -------------------------------------------

# Financial data
firm_fin <- read_fst(here("Data", "firm_fin_panel.fst"), as.data.table = TRUE)
firm_fin <- firm_fin[between(year, 2010, 2014)]
firm_fin[, siren := str_pad(siren, 9, pad = "0")]

firm_fin[, vacf_bw := ifelse(effsalm > 0, vacf/effsalm, NA)]
firm_fin[, vaht_bw := ifelse(effsalm > 0, vaht/effsalm, NA)]

dat[, siren := substr(siret, 1, 9)]

dat <- merge(dat, firm_fin, by = c("year", "siren"), all = FALSE)

dat <- dat[!(is.na(vaht_bw) | is.nan(vaht_bw))]

rm(firm_fin)
gc()

# DPAE information

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

# Trimming the VA variables
trim_level <- 0.01
dat[, vaht_bw_t := ifelse(between(vaht_bw, quantile(vaht_bw, trim_level, na.rm = TRUE), quantile(vaht_bw, 1-trim_level, na.rm = TRUE)), 
                          vaht_bw, NA)]

# Taking the log
dat[, l_vaht_bw := log(vaht_bw_t)]

# Restricting
dat <- dat[!is.na(l_vaht_bw) & l_vaht_bw != -Inf]

# Create a high vs low VA variable
dat[, high_va := l_vaht_bw > median(l_vaht_bw)]

# Create a high vs low FTC length
dat[, high_cdd_dur := m_cdd_dur > median(m_cdd_dur)]

# General regression
reg <- feols(data = dat, l_hwr_agg_w2 ~ cdd | year + csw0(sexe, age_bin, a38, pcs, exp_cs_bin, siret), lean = TRUE, cluster = ~siret)
etable(reg, title = "FTC-OEC wage gap regression",
       dict = c(cddTRUE = "FTC", l_hwr_w2 = "Log hourly wage", year = "Year",
                sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.", a38 = "Industry", pcs = "Occupation", 
                siret = "Establishment", high_vaTRUE = "Above median VA per worker"),
       tex = TRUE, file = here("Results", "Average wage gap", "table_cdd_reg_hiringwage_hetsample.tex"), style.tex = style.tex("aer"))
rm(reg)
gc()

# VA regression
reg <- feols(data = dat, l_hwr_agg_w2 ~ cdd*high_va | year + csw0(sexe, age_bin, a38, pcs, exp_cs_bin, siret), lean = TRUE, cluster = ~siret)
etable(reg, title = "FTC-OEC wage gap regression",
       dict = c(cddTRUE = "FTC", l_hwr_w2 = "Log hourly wage", year = "Year",
                sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.", a38 = "Industry", pcs = "Occupation", 
                siret = "Establishment", high_vaTRUE = "Above median VA per worker"),
       tex = TRUE, file = here("Results", "Average wage gap", "table_cdd_reg_hiringwage_VAhet.tex"), style.tex = style.tex("aer"))
rm(reg)
gc()

# FTC length regression
reg <- feols(data = dat, l_hwr_agg_w2 ~ cdd*high_cdd_dur | year + csw0(sexe, age_bin, a38, pcs, exp_cs_bin, siret), lean = TRUE, cluster = ~siret)
etable(reg, title = "FTC-OEC wage gap regression",
       dict = c(cddTRUE = "FTC", l_hwr_w2 = "Log hourly wage", year = "Year",
                sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.", a38 = "Industry", pcs = "Occupation", 
                siret = "Establishment", high_cdd_durTRUE = "Above median FTC length"),
       tex = TRUE, file = here("Results", "Average wage gap", "table_cdd_reg_hiringwage_FTCLhet.tex"), style.tex = style.tex("aer"))
rm(reg)
gc()

# VA regression - age restriction
reg <- feols(data = dat[between(age, 30, 60)], l_hwr_agg_w2 ~ cdd*high_va | year + csw0(sexe, age_bin, a38, pcs, exp_cs_bin, siret), lean = TRUE, cluster = ~siret)
etable(reg, title = "FTC-OEC wage gap regression",
       dict = c(cddTRUE = "FTC", l_hwr_w2 = "Log hourly wage", year = "Year",
                sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.", a38 = "Industry", pcs = "Occupation", 
                siret = "Establishment", high_vaTRUE = "Above median VA per worker"),
       tex = TRUE, file = here("Results", "Average wage gap", "table_cdd_reg_hiringwage_VAhet_age_rest.tex"), style.tex = style.tex("aer"))
rm(reg)
gc()

# FTC length regression - age restriction
reg <- feols(data = dat[between(age, 30, 60)], l_hwr_agg_w2 ~ cdd*high_cdd_dur | year + csw0(sexe, age_bin, a38, pcs, exp_cs_bin, siret), lean = TRUE, cluster = ~siret)
etable(reg, title = "FTC-OEC wage gap regression",
       dict = c(cddTRUE = "FTC", l_hwr_w2 = "Log hourly wage", year = "Year",
                sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.", a38 = "Industry", pcs = "Occupation", 
                siret = "Establishment", high_cdd_durTRUE = "Above median FTC length"),
       tex = TRUE, file = here("Results", "Average wage gap", "table_cdd_reg_hiringwage_FTCLhet_age_rest.tex"), style.tex = style.tex("aer"))
rm(reg)
gc()

# Firm heterogeneity regression by CDD-U sectors -------------------------------------------

# General regression
reg <- feols(data = dat, l_hwr_agg_w2 ~ cdd | year + sexe + age_bin + a38 + pcs + exp_cs_bin + siret, lean = TRUE, cluster = ~siret, fsplit = ~cddu_a21)
etable(reg, title = "FTC-OEC wage gap regression",
       dict = c(cddTRUE = "FTC", l_hwr_agg_w2 = "Log hourly wage", year = "Year",
                sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.", a38 = "Industry", pcs = "Occupation", 
                siret = "Establishment", high_vaTRUE = "Above median VA per worker", cddu_a21 = "CDD-U sectors"),
       tex = TRUE, file = here("New Results", "Average wage gap", "table_cdd_reg_hiringwage_hetsample_cddu.tex"), style.tex = style.tex("aer"))
rm(reg)
gc()

# VA regression
reg <- feols(data = dat, l_hwr_agg_w2 ~ cdd*high_va | year + sexe + age_bin + a38 + pcs + exp_cs_bin + siret, lean = TRUE, cluster = ~siret, fsplit = ~cddu_a21)
etable(reg, title = "FTC-OEC wage gap regression",
       dict = c(cddTRUE = "FTC", l_hwr_agg_w2 = "Log hourly wage", year = "Year",
                sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.", a38 = "Industry", pcs = "Occupation", 
                siret = "Establishment", high_vaTRUE = "Above median VA per worker", cddu_a21 = "CDD-U sectors"),
       tex = TRUE, file = here("New Results", "Average wage gap", "table_cdd_reg_hiringwage_VAhet_cddu.tex"), style.tex = style.tex("aer"))
rm(reg)
gc()

# Create a high vs low FTC length
dat[, high_cdd_dur := m_cdd_dur > median(m_cdd_dur)]

# FTC length regression
reg <- feols(data = dat, l_hwr_agg_w2 ~ cdd*high_cdd_dur | year + sexe + age_bin + a38 + pcs + exp_cs_bin + siret, lean = TRUE, cluster = ~siret, fsplit = ~cddu_a21)
etable(reg, title = "FTC-OEC wage gap regression",
       dict = c(cddTRUE = "FTC", l_hwr_agg_w2 = "Log hourly wage", year = "Year",
                sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.", a38 = "Industry", pcs = "Occupation", 
                siret = "Establishment", high_cdd_durTRUE = "Above median FTC length", cddu_a21 = "CDD-U sectors"),
       tex = TRUE, file = here("New Results", "Average wage gap", "table_cdd_reg_hiringwage_FTCLhet_cddu.tex"), style.tex = style.tex("aer"))
rm(reg)
gc()

# # VA regression - age restriction
# reg <- feols(data = dat[between(age, 30, 60)], l_hwr_agg_w2 ~ cdd*high_va | year + csw0(sexe, age_bin, a38, pcs, exp_cs_bin, siret), lean = TRUE, cluster = ~siret)
# etable(reg, title = "FTC-OEC wage gap regression",
#        dict = c(cddTRUE = "FTC", l_hwr_w2 = "Log hourly wage", year = "Year",
#                 sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.", a38 = "Industry", pcs = "Occupation", 
#                 siret = "Establishment", high_vaTRUE = "Above median VA per worker"),
#        tex = TRUE, file = here("Results", "Average wage gap", "table_cdd_reg_hiringwage_VAhet_age_rest.tex"), style.tex = style.tex("aer"))
# rm(reg)
# gc()
# 
# # FTC length regression - age restriction
# reg <- feols(data = dat[between(age, 30, 60)], l_hwr_agg_w2 ~ cdd*high_cdd_dur | year + csw0(sexe, age_bin, a38, pcs, exp_cs_bin, siret), lean = TRUE, cluster = ~siret)
# etable(reg, title = "FTC-OEC wage gap regression",
#        dict = c(cddTRUE = "FTC", l_hwr_w2 = "Log hourly wage", year = "Year",
#                 sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.", a38 = "Industry", pcs = "Occupation", 
#                 siret = "Establishment", high_cdd_durTRUE = "Above median FTC length"),
#        tex = TRUE, file = here("Results", "Average wage gap", "table_cdd_reg_hiringwage_FTCLhet_age_rest.tex"), style.tex = style.tex("aer"))
# rm(reg)
# gc()

# Individual Heterogeneity -----------------------------------------------------------

# # Occupation PCS residualisation
# reg2 <-  feols(data = dat, l_hwr_agg_w2 ~ 1 | year + sexe + age_bin + a38 + exp_cs_bin, lean = TRUE, cluster = ~siret)
# dat2 <- dat[!is.na(l_hwr_agg_w2), res_test := reg2$residuals]
# 
# reg3 <- feols(data = dat2, res_test ~ cdd, split = ~pcs, lean = TRUE, cluster = ~siret)
# 
# foo2 <- do.call(rbind, lapply(reg3, coeftable, keep = "cddTRUE"))
# foo2 <- data.table(cbind(foo2, names(reg3)))
# setnames(foo2, "V5", "pcs")
# write.fst(foo2, here("Results", "Average wage gap", "base_reg_hetoccpcs_resid.fst"))
# 
# w_occ <- dat[, .(m_lwage = mean(l_hwr_agg_w2, na.rm = TRUE)), by = pcs]
# 
# res <- merge(foo2, w_occ, by = "pcs")
# 
# res[, y := as.numeric(Estimate)]
# 
# p <- res[pcs != ""] %>%
#   ggplot(aes(x = m_lwage, y = y)) +
#   geom_point() +
#   theme_bw() +
#   geom_smooth(method = "lm", se = FALSE) +
#   ylab("FTC-OEC gap in log hourly wage") + xlab("Average log hourly wage in PCS")
# p
# ggsave(p, filename = here("Results", "Average wage gap", "base_reg_hetoccpcs_resid_plot_orderhw.png"))

reg <- feols(data = dat, l_hwr_agg_w2 ~ cdd | year + sexe + age_bin + a38 + pcs + exp_cs_bin + siret, lean = TRUE, cluster = ~siret, split = ~cs_clean)

# Plot
tp <- coeftable(reg) %>% setDT
tp <- tp[sample > 10][, .(sample, Estimate, `Std. Error`)]
names(tp) <- c("cs", "pe", "se")

tp[, cs1 := substr(cs, 1, 1)]

tp2 <- dat[, .(fcdd = mean(cdd)), by = cs_clean]
tp2[, cs1 := substr(cs_clean, 1, 1)]

p <- tp %>%
  ggplot(aes(x = cs, y = pe, fill = cs1)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = pe - qnorm(0.975)*se, ymax = pe + qnorm(0.975)*se), width = 0.2, position = position_dodge(width = 0.9)) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = "none") +
  xlab("2-digit occupation") + ylab("Estimated FTC-OEC log hourly wage gap")
p
ggsave(p, filename = here("New Results", "Average wage gap", "hiring_wage_gap_cs2.png"), width = 7)

p2 <- tp2[cs_clean > 10] %>%
  ggplot(aes(x = as.factor(cs_clean), y = fcdd, fill = cs1)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    legend.position = "none") +
  xlab("2-digit occupation") + ylab("Fraction of hiring in FTC")
p2
ggsave(p2, filename = here("New Results", "Average wage gap", "fcdd_cs2.png"), width = 7)

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# 2010 - 2014; MW restricted version -------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Load data ---------------------------------------------------------------

dat <- read_fst(here("Data", "ss_dads_hiring_panel_1014_restMW.fst"), as.data.table = TRUE)

dat <- dat[hwr != Inf]

# Prepare outcome variable ------------------------------------------------

dat[, l_hwr := log(hwr)]
dat[, l_hwr_agg := log(hwr_agg)]

# Trimming
trim_level <- 0.001
dat[, l_hwr_w2 := ifelse(between(l_hwr, quantile(l_hwr, trim_level, na.rm = TRUE), quantile(l_hwr, 1-trim_level, na.rm = TRUE)), 
                         l_hwr, NA)]
dat[, l_hwr_agg_w2 := ifelse(between(l_hwr_agg, quantile(l_hwr_agg, trim_level, na.rm = TRUE), quantile(l_hwr_agg, 1-trim_level, na.rm = TRUE)), 
                             l_hwr_agg, NA)]

# Regressions -------------------------------------------------------------
gc()

reg <- feols(data = dat, l_hwr_agg_w2 ~ cdd | year + csw0(sexe, age_bin, a38, pcs, exp_cs_bin, siret, ident_all), lean = TRUE, cluster = ~siret)

etable(reg, title = "FTC-OEC wage gap regression",
       dict = c(cddTRUE = "FTC", l_hwr_w = "Log hourly wage", year = "Year",
                sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.",a38 = "Industry", cs_clean = "Occupation (2 digit)", 
                siret = "Establishment", ident_all = "Individual"),
       # cluster = ~siret,
       tex = TRUE, file = here("Results", "Average wage gap", "table_cdd_reg_hiringwage_restMW.tex"), style.tex = style.tex("aer"))

rm(reg)
gc()

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# 2005 - 2009 -------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Load data ---------------------------------------------------------------

dat <- read_fst(here("Data", "ss_dads_hiring_panel_0509.fst"), as.data.table = TRUE)

dat <- dat[hwr != Inf]

# Prepare outcome variable ------------------------------------------------

dat[, l_hwr := log(hwr)]
dat[, l_hwr_agg := log(hwr_agg)]

# Trimming
trim_level <- 0.001
dat[, l_hwr_w2 := ifelse(between(l_hwr, quantile(l_hwr, trim_level, na.rm = TRUE), quantile(l_hwr, 1-trim_level, na.rm = TRUE)), 
                         l_hwr, NA)]
dat[, l_hwr_agg_w2 := ifelse(between(l_hwr_agg, quantile(l_hwr_agg, trim_level, na.rm = TRUE), quantile(l_hwr_agg, 1-trim_level, na.rm = TRUE)), 
                             l_hwr_agg, NA)]

# Regressions -------------------------------------------------------------
gc()

reg <- feols(data = dat, l_hwr_agg_w2 ~ cdd | year + csw0(sexe, age_bin, a38, cs_clean, exp_cs_bin, siret, ident_all), lean = TRUE, cluster = ~siret)

etable(reg, title = "FTC-OEC wage gap regression",
       dict = c(cddTRUE = "FTC", l_hwr_w = "Log hourly wage", year = "Year",
                sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.",a38 = "Industry", cs_clean = "Occupation (2 digit)", 
                siret = "Establishment", ident_all = "Individual"),
       # cluster = ~siret,
       tex = TRUE, file = here("Results", "Average wage gap", "table_cdd_reg_hiringwage_occ2_0509.tex"), style.tex = style.tex("aer"))

rm(reg)
gc()

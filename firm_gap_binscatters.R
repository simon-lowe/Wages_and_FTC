rm(list = ls())
gc()

# Saving suffix -----------------------------------------------------------

sav_suf <- ""

# Load packages -----------------------------------------------------------

list.of.packages <- c(
  "tidyverse", "data.table", "haven", "fst", "expss", "janitor", "here",
  "DescTools", "tictoc", "collapse", "readxl",
  "fixest", "binsreg", "did", "ggplot2", "patchwork", "modelsummary"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(list.of.packages, library, character.only = TRUE))
rm(list.of.packages, new.packages)

setDTthreads(0)
setFixest_nthreads(8)



# Load data ---------------------------------------------------------------

# Descriptive stats on data merging dataset initialization
desc_dat <- data.table()

# DADS data
dat <- read.fst(here("Data", "ss_panel_1014_collapseSiren_hiring_wage.fst"), as.data.table = TRUE)

desc_dat <- rbind(desc_dat, data.table(data = "Full data", n_obs = nrow(dat), 
                                       n_firms = length(unique(dat$siren)),
                                       mean_tot_emp = mean(dat$n),
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

# Restrict to firms which have a FTC-OEC gap (ie in a given year have hired both)
dat <- dat[!is.na(m_lhw_res4_cdd) & !is.na(m_lhw_res4_cdi)]

desc_dat <- rbind(desc_dat, data.table(data = "FTC-OEC gap", n_obs = nrow(dat), 
                                       n_firms = length(unique(dat$siren)),
                                       mean_tot_emp = mean(dat$n),
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

# Financial data
firm_fin <- read_fst(here("Data", "firm_fin_panel.fst"), as.data.table = TRUE)
firm_fin <- firm_fin[between(year, 2010, 2014)]
firm_fin[, siren := str_pad(siren, 9, pad = "0")]

firm_fin[, vacf_bw := ifelse(effsalm > 0, vacf/effsalm, NA)]
firm_fin[, vaht_bw := ifelse(effsalm > 0, vaht/effsalm, NA)]

dat <- merge(dat, firm_fin, by = c("year", "siren"), all = FALSE)

dat <- dat[!(is.na(vaht_bw) | is.nan(vaht_bw))]

desc_dat <- rbind(desc_dat, data.table(data = "fin data", n_obs = nrow(dat), 
                                       n_firms = length(unique(dat$siren)),
                                       mean_tot_emp = mean(dat$n),
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

rm(firm_fin)
gc()

# DPAE information
dpae_dat <- read_fst(here("Data", "dpae_col_hiring_siren_drop2.fst"), as.data.table = TRUE)

dpae_dat[, f_conv_cdi := ifelse(n_cdi_h > 0, n_conv_cdi/n_cdi_h, -99)]
dpae_dat[, f_conv_cdd := ifelse(n_cdd_h > 0, n_conv_cdd/n_cdd_h, -99)]
dpae_dat[, f_cdd := ifelse(n_h > 0, n_cdd_h/n_h, -99)]

dat <- merge(dat, dpae_dat, by = c("year", "siren"), all = FALSE)

rm(dpae_dat)
gc()

desc_dat <- rbind(desc_dat, data.table(data = "dpae info", n_obs = nrow(dat), 
                                       n_firms = length(unique(dat$siren)),
                                       mean_tot_emp = mean(dat$n),
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

# Drop NAs in FTC length
dat <- dat[!is.na(m_cdd_dur)]

desc_dat <- rbind(desc_dat, data.table(data = "dpae ftc length", n_obs = nrow(dat), 
                                       n_firms = length(unique(dat$siren)),
                                       mean_tot_emp = mean(dat$n),
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

# Create variables --------------------------------------------------------

# Wage gap variable
dat[, gap := m_lhw_res4_cdd - m_lhw_res4_cdi]


# Trimming the VA variables
trim_level <- 0.01
dat[, vaht_bw_t := ifelse(between(vaht_bw, quantile(vaht_bw, trim_level, na.rm = TRUE), quantile(vaht_bw, 1-trim_level, na.rm = TRUE)), 
                           vaht_bw, NA)]

# Taking the log
dat[, l_vaht_bw := log(vaht_bw_t)]

# Restricting
dat <- dat[!is.na(l_vaht_bw) & l_vaht_bw != -Inf]

desc_dat <- rbind(desc_dat, data.table(data = "VA info", n_obs = nrow(dat), 
                                       n_firms = length(unique(dat$siren)),
                                       mean_tot_emp = mean(dat$n),
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

# VA per worker quantiles
y_quant <- quantile(dat$l_vaht_bw, probs = (0:4)/4)
y_quant[1] <- -Inf
y_quant[5] <- +Inf

dat[, l_vaht_bw_q := cut(l_vaht_bw, breaks = y_quant)]

# FTC length quantiles
y_quant <- quantile(dat$m_cdd_dur, probs = (0:4)/4)
y_quant[1] <- -Inf
y_quant[5] <- +Inf

dat[, m_cdd_dur_q := cut(m_cdd_dur, breaks = y_quant)]


# FTC hiring fraction from DADS info
dat[, f_cdd_dads := n_cdd/n]

# Add 10 level industry
ind_conv <- read_excel("C:/Users/Public/Documents/sources/Industry homogeneization/Conversion_Tables.xlsx", 
                       sheet = "Agg2")
setDT(ind_conv)
ind_conv <- clean_names(ind_conv)
ind_conv <- unique(ind_conv)

dat <- merge(dat, ind_conv, by = "a38", all.x = TRUE, all.y = FALSE)

# Name of 10 level industry
tmp <- read_excel("C:/Users/Public/Documents/sources/Industry homogeneization/MyConv.xls", 
                  sheet = "A10")
setDT(tmp)

dat <- merge(dat, tmp, by.x = "a10", by.y = "code", all.x = TRUE, all.y = FALSE)

# Save desc_dat
datasummary_df(desc_dat[, names(desc_dat)[-1] := lapply(.SD, function(x) round(x, 2)), .SDcols = names(desc_dat)[-1]], 
               fmt = NULL,
               notes = "foo",
               output = here("Results", "Firm gap binscatters", paste0("tab_desc_dat_firm_gap", sav_suf, ".tex")))

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Gaps --------------------------------------------------------------------

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------



# Log value-added per worker ----------------------------------------------

# Overall
est_va <- binsreg(data = dat, y = gap, x = l_vaht_bw, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est_va$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]

p <- ggplot() + labs(x = "Log VA per worker", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw", sav_suf, ".png")))

est_va_w <- binsreg(data = dat, y = gap, x = l_vaht_bw, weights = n, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est_va_w$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]

p <- ggplot() + labs(x = "Log VA per worker", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_w", sav_suf, ".png")))


# By FTC length quartiles
est <- binsreg(data = dat, y = gap, x = l_vaht_bw, by = m_cdd_dur_q, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- data.table()
tp2 <- data.table()
tp3 <- data.table()
for(i in 1:length(est$data.plot)){
  tp <- rbind(tp, est$data.plot[[i]]$data.dots)
  tp2 <- rbind(tp2, est$data.plot[[i]]$data.ci)
  tp3 <- rbind(tp3, est$data.plot[[i]]$data.cb)
}

tmp <- tp2[, .(x_min = min(x), x_max = max(x)), by = group]
tp3 <- merge(tp3, tmp, by = "group", all = TRUE)
tp3 <- tp3[between(x, x_min - 0.2, x_max + 0.2)]

tp[, group := factor(group, levels = levels(dat$m_cdd_dur_q))]
tp2[, group := factor(group, levels = levels(dat$m_cdd_dur_q))]
tp3[, group := factor(group, levels = levels(dat$m_cdd_dur_q))]

p <- tp %>% ggplot() + 
  labs(x = "Log VA per worker", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14)) +
  facet_wrap(~group, nrow = 2, scales = "free_y")
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_byFTCl", sav_suf, ".png")))

p <- tp %>% ggplot() + 
  geom_point(data = tp, aes(x = (x), y = fit, color = group)) +
  geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r, fill = group), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Log VA per worker", y = "Firm gaps in log hourly wage", color = "FTC length", fill = "FTC length") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12))
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_byFTCl2", sav_suf, ".png")))

# By industry
est_byind <- binsreg(data = dat[a10 != "AZ"],
                     y = gap, x = l_vaht_bw, by = name_eng, dots = c(3,3), line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- data.table()
tp2 <- data.table()
tp3 <- data.table()
for(i in 1:length(est_byind$data.plot)){
  tp <- rbind(tp, est_byind$data.plot[[i]]$data.dots)
  tp2 <- rbind(tp2, est_byind$data.plot[[i]]$data.ci)
  tp3 <- rbind(tp3, est_byind$data.plot[[i]]$data.cb)
}

tmp <- tp2[, .(x_min = min(x), x_max = max(x)), by = group]
tp3 <- merge(tp3, tmp, by = "group", all = TRUE)
tp3 <- tp3[between(x, x_min-0.5, x_max + 0.5)]

p <- tp %>% ggplot() + 
  labs(x = "Log VA per worker", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14)) +
  facet_wrap(~group, nrow = 3, scales = "free_y", labeller = labeller(group = label_wrap_gen(30)))
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_byind", sav_suf, ".png")), width = 10, height = 10)

# By a38 industry
tmp_dat <- dat
tmp_dat[, n_ind := .N, by = a38]

est_byind <- binsreg(data = tmp_dat[a10 != "AZ" & n_ind > 1e2],
                     y = gap, x = l_vaht_bw, by = a38, dots = c(3,3), line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- data.table()
tp2 <- data.table()
tp3 <- data.table()
for(i in 1:length(est_byind$data.plot)){
  tp <- rbind(tp, est_byind$data.plot[[i]]$data.dots)
  tp2 <- rbind(tp2, est_byind$data.plot[[i]]$data.ci)
  tp3 <- rbind(tp3, est_byind$data.plot[[i]]$data.cb)
}

tmp <- tp2[, .(x_min = min(x), x_max = max(x)), by = group]
tp3 <- merge(tp3, tmp, by = "group", all = TRUE)
tp3 <- tp3[between(x, x_min-0.5, x_max + 0.5)]

p <- tp %>% ggplot() + 
  labs(x = "Log VA per worker", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14)) +
  facet_wrap(~group, nrow = 3, scales = "free_y", labeller = labeller(group = label_wrap_gen(30)))
p

rm(tmp_dat)


# Average CDD length ----------------------------------------------

# Overall
est_l <- binsreg(data = dat, y = gap, x = m_cdd_dur, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est_l$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)
tp_cb <- tp_cb[between(x, min(tp_p$x) - 2, max(tp_p$x) + 10)]

p <- ggplot() + labs(x = "Average FTC length in days", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic()
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_lcdd", sav_suf, ".png")))


est_l <- binsreg(data = dat, y = gap, x = m_cdd_dur, weights = n, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est_l$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)
tp_cb <- tp_cb[between(x, min(tp_p$x) - 2, max(tp_p$x) + 10)]

p <- ggplot() + labs(x = "Average FTC length in days", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic()
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_lcdd_w", sav_suf, ".png")))

# By log VA per worker quartiles
est <- binsreg(data = dat, y = gap, x = m_cdd_dur, by = l_vaht_bw_q, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- data.table()
tp2 <- data.table()
tp3 <- data.table()
for(i in 1:length(est$data.plot)){
  tp <- rbind(tp, est$data.plot[[i]]$data.dots)
  tp2 <- rbind(tp2, est$data.plot[[i]]$data.ci)
  tp3 <- rbind(tp3, est$data.plot[[i]]$data.cb)
}

tmp <- tp2[, .(x_min = min(x), x_max = max(x)), by = group]
tp3 <- merge(tp3, tmp, by = "group", all = TRUE)
tp3 <- tp3[between(x, x_min - 10, x_max + 10)]

tp[, group := factor(group, levels = levels(dat$l_vaht_bw_q))]
tp2[, group := factor(group, levels = levels(dat$l_vaht_bw_q))]
tp3[, group := factor(group, levels = levels(dat$l_vaht_bw_q))]

p <- tp %>% ggplot() + 
  labs(x = "Average FTC length in days", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14)) +
  facet_wrap(~group, nrow = 2, scales = "free_y")
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_lcdd_byvaht", sav_suf, ".png")))

p <- tp %>% ggplot() + 
  geom_point(data = tp, aes(x = (x), y = fit, color = group)) +
  geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r, fill = group), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Average FTC length in days", y = "Firm gaps in log hourly wage", color = "Log VA per worker", fill = "Log VA per worker") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12))
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_lcdd_byvaht", sav_suf, ".png")))

# Single year
est_l <- binsreg(data = dat[year == 2012], y = gap, x = m_cdd_dur, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est_l$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)
tp_cb <- tp_cb[between(x, min(tp_p$x) - 2, max(tp_p$x) + 10)]

p <- ggplot() + labs(x = "Average FTC length in days", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic()
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_lcdd_y2012", sav_suf, ".png")))

# By industry
est_byind <- binsreg(data = dat[a10 != "AZ"],
                     y = gap, x = m_cdd_dur, by = name_eng, dots = c(3,3), line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- data.table()
tp2 <- data.table()
tp3 <- data.table()
for(i in 1:length(est_byind$data.plot)){
  tp <- rbind(tp, est_byind$data.plot[[i]]$data.dots)
  tp2 <- rbind(tp2, est_byind$data.plot[[i]]$data.ci)
  tp3 <- rbind(tp3, est_byind$data.plot[[i]]$data.cb)
}

tmp <- tp2[, .(x_min = min(x), x_max = max(x)), by = group]
tp3 <- merge(tp3, tmp, by = "group", all = TRUE)
tp3 <- tp3[between(x, x_min-10, x_max + 20)]

p <- tp %>% ggplot() + 
  labs(x = "Average FTC length in days", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14)) +
  facet_wrap(~group, nrow = 3, scales = "free_y", labeller = labeller(group = label_wrap_gen(30)))
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_lcdd_byind", sav_suf, ".png")), width = 10, height = 10)


# Hiring fraction ----------------------------------------------

# Using DPAE
est_hir <- binsreg(data = dat, y = gap, x = f_cdd, line = c(3,3), ci = c(3,3), cb = c(3,3))


tp <- est_hir$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.02, max(tp_p$x) + 0.02)]

p <- ggplot() + labs(x = "Fraction of hires in FTC", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14))
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_fcdd", sav_suf, ".png")))

est_hir_w <- binsreg(data = dat, y = gap, x = f_cdd, weights = n, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est_hir_w$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.02, max(tp_p$x) + 0.02)]

p <- ggplot() + labs(x = "Fraction of hires in FTC", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14))
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_fcdd_w", sav_suf, ".png")))

# Using DADS
est_hir <- binsreg(data = dat, y = gap, x = f_cdd_dads, line = c(3,3), ci = c(3,3), cb = c(3,3))


tp <- est_hir$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.02, max(tp_p$x) + 0.02)]

p <- ggplot() + labs(x = "Fraction of hires in FTC", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14))
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_fcdd_dads", sav_suf, ".png")))

est_hir_w <- binsreg(data = dat, y = gap, x = f_cdd_dads, weights = n, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est_hir_w$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.02, max(tp_p$x) + 0.02)]

p <- ggplot() + labs(x = "Fraction of hires in FTC", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14))
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_fcdd_dads_w", sav_suf, ".png")))

# By industry
est_byind <- binsreg(data = dat[a10 != "AZ"],
                     y = gap, x = f_cdd, by = name_eng, dots = c(3,3), line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- data.table()
tp2 <- data.table()
tp3 <- data.table()
for(i in 1:length(est_byind$data.plot)){
  tp <- rbind(tp, est_byind$data.plot[[i]]$data.dots)
  tp2 <- rbind(tp2, est_byind$data.plot[[i]]$data.ci)
  tp3 <- rbind(tp3, est_byind$data.plot[[i]]$data.cb)
}

tmp <- tp2[, .(x_min = min(x), x_max = max(x)), by = group]
tp3 <- merge(tp3, tmp, by = "group", all = TRUE)
tp3 <- tp3[between(x, x_min-0.02, x_max + 0.02)]

p <- tp %>% ggplot() + 
  labs(x = "Fraction of hires in FTC", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14)) +
  facet_wrap(~group, nrow = 3, scales = "free_y", labeller = labeller(group = label_wrap_gen(30)))
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_fcdd_byind", sav_suf, ".png")), width = 10, height = 10)

# CDI Conversion rate ----------------------------------------------

est_hir <- binsreg(data = dat[f_conv_cdi != -99], y = gap, x = f_conv_cdi, line = c(3,3), ci = c(3,3), cb = c(3,3), masspoints = "off")

tp <- est_hir$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.02, max(tp_p$x) + 0.02)]

p <- ggplot() + 
  # geom_point(aes(x = -0.02, y = dat[f_conv_cdi == -99, mean(gap)], color = "red")) +
  # geom_text(aes(x = 0.045, y = dat[f_conv_cdi == -99, mean(gap)] + 0.01, label = "Firms with \n no OEC hires", color = "red")) +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Fraction of OEC hires that come from FTC-conversion", y = "Firm gaps in log hourly wage") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "none")
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_fconvcdi", sav_suf, ".png")))

# CDD Conversion rate ----------------------------------------------

est_hir <- binsreg(data = dat[f_conv_cdd != -99], y = gap, x = f_conv_cdd, line = c(3,3), ci = c(3,3), cb = c(3,3), masspoints = "off")

tp <- est_hir$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.02, max(tp_p$x) + 0.02)]

p <- ggplot() + 
  geom_point(aes(x = -0.02, y = dat[f_conv_cdd == -99, mean(gap)], color = "red")) +
  geom_text(aes(x = 0.045, y = dat[f_conv_cdd == -99, mean(gap)] + 0.01, label = "Firms with \n no OEC hires", color = "red")) +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Fraction of FTC hires that will get converted to OEC", y = "Firm gaps in log hourly wage") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "none")
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_fconvcdd", sav_suf, ".png")))

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Levels --------------------------------------------------------------------

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------



# Log value-added per worker ----------------------------------------------

# Binscatter
est_va_cdd_raw <- binsreg(data = dat, y = m_lhw_cdd, x = l_vaht_bw, line = c(3,3), ci = c(3,3), cb = c(3,3))

est_va_cdi_raw <- binsreg(data = dat, y = m_lhw_cdi, x = l_vaht_bw, line = c(3,3), ci = c(3,3), cb = c(3,3))

est_va_cdd_res <- binsreg(data = dat, y = m_lhw_res4_cdd, x = l_vaht_bw, line = c(3,3), ci = c(3,3), cb = c(3,3))

est_va_cdi_res <- binsreg(data = dat, y = m_lhw_res4_cdi, x = l_vaht_bw, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp0 <- est_va_cdd_raw$data.plot$`Group Full Sample` 
tp1 <- est_va_cdi_raw$data.plot$`Group Full Sample`
tp2 <- est_va_cdd_res$data.plot$`Group Full Sample`
tp3 <- est_va_cdi_res$data.plot$`Group Full Sample`

l_text <- c("FTC", "OEC")

tp_p1 <- setDT(tp0$data.dots)[, c("x", "fit")][, group := l_text[1]]
tp_p1 <- rbind(tp_p1, setDT(tp1$data.dots)[, c("x", "fit")][, group := l_text[2]])

tp_ci1 <- setDT(tp0$data.ci)[, group := l_text[1]]
tp_ci1 <- rbind(tp_ci1, setDT(tp1$data.ci)[, group := l_text[2]])

tp_cb1 <- setDT(tp0$data.cb)[, group := l_text[1]]
tp_cb1 <- rbind(tp_cb1, setDT(tp1$data.cb)[, group := l_text[2]])


tp_p2 <- setDT(tp2$data.dots)[, c("x", "fit")][, group := l_text[1]]
tp_p2 <- rbind(tp_p2, setDT(tp3$data.dots)[, c("x", "fit")][, group := l_text[2]])

tp_ci2 <- setDT(tp2$data.ci)[, group := l_text[1]]
tp_ci2 <- rbind(tp_ci2, setDT(tp3$data.ci)[, group := l_text[2]])

tp_cb2 <- setDT(tp2$data.cb)[, group := l_text[1]]
tp_cb2 <- rbind(tp_cb2, setDT(tp3$data.cb)[, group := l_text[2]])


tp_cb1 <- tp_cb1[between(x, min(tp_p1$x) -0.2, max(tp_p1$x) + 0.2)]
tp_cb2 <- tp_cb2[between(x, min(tp_p2$x) -0.2, max(tp_p2$x) + 0.2)]

p1 <- ggplot() + labs(x = "Log VA per worker", y = "Log hourly wage") +
  geom_point(data = tp_p1, aes(x = (x), y = fit, color = factor(group))) +
  geom_ribbon(data = tp_cb1, aes(x = (x), ymin = cb.l, ymax = cb.r, fill = factor(group)), alpha = 0.2) +
  scale_color_discrete(breaks = l_text) + scale_fill_discrete(breaks = l_text) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("Raw")
p1

p2 <- ggplot() + labs(x = "Log VA per worker", y = "Log hourly wage") +
  geom_point(data = tp_p2, aes(x = (x), y = fit, color = factor(group))) +
  geom_ribbon(data = tp_cb2, aes(x = (x), ymin = cb.l, ymax = cb.r, fill = factor(group)), alpha = 0.2) +
  scale_color_discrete(breaks = l_text) + scale_fill_discrete(breaks = l_text) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("Residualized")
p2

p <- p1 + p2  + plot_layout(guides = "collect") & theme(legend.position = "bottom")
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_levels_lvaht", sav_suf, ".png")))

# Regression
reg1 <- feols(data = dat, m_lhw_cdd ~ l_vaht_bw)
reg2 <- feols(data = dat, m_lhw_cdi ~ l_vaht_bw)
reg3 <- feols(data = dat, m_lhw_res4_cdd ~ l_vaht_bw)
reg4 <- feols(data = dat, m_lhw_res4_cdi ~ l_vaht_bw)
etable(reg1, reg2, reg3, reg4, 
       title = "Unrestricted regression of log hourly wage on log VA per worker",
       dict = c(m_lhw_cdd = "FTC log hourly wage", m_lhw_cdi = "OEC log hourly wage", m_lhw_res4_cdd = "Residualized FTC log hourly wage",
                m_lhw_res4_cdi = "Residualized OEC log hourly wage", l_vaht_bw = "Log Value Added per worker"),
       # cluster = ~siret,
       tex = TRUE, file = here("Results", "Firm gap binscatters", "table_unrest_lhw_va.tex"), style.tex = style.tex("aer"))

reg_cut_off <- 3.5
reg1 <- feols(data = dat[l_vaht_bw > reg_cut_off], m_lhw_cdd ~ l_vaht_bw)
reg2 <- feols(data = dat[l_vaht_bw > reg_cut_off], m_lhw_cdi ~ l_vaht_bw)
reg3 <- feols(data = dat[l_vaht_bw > reg_cut_off], m_lhw_res4_cdd ~ l_vaht_bw)
reg4 <- feols(data = dat[l_vaht_bw > reg_cut_off], m_lhw_res4_cdi ~ l_vaht_bw)
etable(reg1, reg2, reg3, reg4, 
       title = "Restricted regression of log hourly wage on log VA per worker",
       dict = c(m_lhw_cdd = "FTC log hourly wage", m_lhw_cdi = "OEC log hourly wage", m_lhw_res4_cdd = "Residualized FTC log hourly wage",
                m_lhw_res4_cdi = "Residualized OEC log hourly wage", l_vaht_bw = "Log Value Added per worker"),
       # cluster = ~siret,
       tex = TRUE, file = here("Results", "Firm gap binscatters", "table_rest_lhw_va.tex"), style.tex = style.tex("aer"))

# Average FTC length ----------------------------------------------

est_va_cdd_raw <- binsreg(data = dat, y = m_lhw_cdd, x = m_cdd_dur, line = c(3,3), ci = c(3,3), cb = c(3,3))

est_va_cdi_raw <- binsreg(data = dat, y = m_lhw_cdi, x = m_cdd_dur, line = c(3,3), ci = c(3,3), cb = c(3,3))

est_va_cdd_res <- binsreg(data = dat, y = m_lhw_res4_cdd, x = m_cdd_dur, line = c(3,3), ci = c(3,3), cb = c(3,3))

est_va_cdi_res <- binsreg(data = dat, y = m_lhw_res4_cdi, x = m_cdd_dur, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp0 <- est_va_cdd_raw$data.plot$`Group Full Sample` 
tp1 <- est_va_cdi_raw$data.plot$`Group Full Sample`
tp2 <- est_va_cdd_res$data.plot$`Group Full Sample`
tp3 <- est_va_cdi_res$data.plot$`Group Full Sample`

l_text <- c("FTC", "OEC")

tp_p1 <- setDT(tp0$data.dots)[, c("x", "fit")][, group := l_text[1]]
tp_p1 <- rbind(tp_p1, setDT(tp1$data.dots)[, c("x", "fit")][, group := l_text[2]])

tp_ci1 <- setDT(tp0$data.ci)[, group := l_text[1]]
tp_ci1 <- rbind(tp_ci1, setDT(tp1$data.ci)[, group := l_text[2]])

tp_cb1 <- setDT(tp0$data.cb)[, group := l_text[1]]
tp_cb1 <- rbind(tp_cb1, setDT(tp1$data.cb)[, group := l_text[2]])

tp_p2 <- setDT(tp2$data.dots)[, c("x", "fit")][, group := l_text[1]]
tp_p2 <- rbind(tp_p2, setDT(tp3$data.dots)[, c("x", "fit")][, group := l_text[2]])

tp_ci2 <- setDT(tp2$data.ci)[, group := l_text[1]]
tp_ci2 <- rbind(tp_ci2, setDT(tp3$data.ci)[, group := l_text[2]])

tp_cb2 <- setDT(tp2$data.cb)[, group := l_text[1]]
tp_cb2 <- rbind(tp_cb2, setDT(tp3$data.cb)[, group := l_text[2]])

tp_cb1 <- tp_cb1[between(x, min(tp_p1$x) -2, max(tp_p1$x) + 10)]
tp_cb2 <- tp_cb2[between(x, min(tp_p2$x) -2, max(tp_p2$x) + 10)]

p1 <- ggplot() + labs(x = "Average FTC length", y = "Log hourly wage") +
  geom_point(data = tp_p1, aes(x = (x), y = fit, color = factor(group))) +
  geom_ribbon(data = tp_cb1, aes(x = (x), ymin = cb.l, ymax = cb.r, fill = factor(group)), alpha = 0.2) +
  scale_color_discrete(breaks = l_text) + scale_fill_discrete(breaks = l_text) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("Raw")
p1

p2 <- ggplot() + labs(x = "Average FTC length", y = "Log hourly wage") +
  geom_point(data = tp_p2, aes(x = (x), y = fit, color = factor(group))) +
  geom_ribbon(data = tp_cb2, aes(x = (x), ymin = cb.l, ymax = cb.r, fill = factor(group)), alpha = 0.2) +
  scale_color_discrete(breaks = l_text) + scale_fill_discrete(breaks = l_text) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("Residualized")
p2

p <- p1 + p2  + plot_layout(guides = "collect") & theme(legend.position = "bottom")
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_levels_FTCl", sav_suf, ".png")))


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Other graphs ------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------



# Log value-added per worker against average cdd ----------------------------------------------

est <- binsreg(data = dat, y = m_cdd_dur, x = l_vaht_bw, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]

p <- ggplot() + labs(x = "Log VA per worker", y = "Average FTC length") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic()
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("FTCl_vs_VA", sav_suf, ".png")))

# Log value-added per worker against cdd use ----------------------------------------------

est <- binsreg(data = dat, y = f_cdd, x = l_vaht_bw, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]

p <- ggplot() + labs(x = "Log VA per worker", y = "Fraction of hires in FTC") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic()
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("f_cdd_vs_VA", sav_suf, ".png")))

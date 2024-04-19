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
dat <- read.fst(here("Data", "postes_1014_collapseSiren_hiring_wage.fst"), as.data.table = TRUE)

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


# DPAE information
# dpae_dat <- read_fst(here("Data", "dpae_col_hiring_siren_drop2.fst"), as.data.table = TRUE)
# 
# dpae_dat[, f_conv_cdi := ifelse(n_cdi_h > 0, n_conv_cdi/n_cdi_h, -99)]
# dpae_dat[, f_conv_cdd := ifelse(n_cdd_h > 0, n_conv_cdd/n_cdd_h, -99)]
# dpae_dat[, f_cdd := ifelse(n_h > 0, n_cdd_h/n_h, -99)]
# 
# dat <- merge(dat, dpae_dat, by = c("year", "siren"), all = FALSE)
# 
# rm(dpae_dat)
# gc()
# 
# desc_dat <- rbind(desc_dat, data.table(data = "dpae info", n_obs = nrow(dat), 
#                                        n_firms = length(unique(dat$siren)),
#                                        mean_tot_emp = mean(dat$n),
#                                        mean_wage = mean(dat$m_lhw, na.rm = TRUE)))
# 
# # Drop NAs in FTC length
# dat <- dat[!is.na(m_cdd_dur)]
# 
# desc_dat <- rbind(desc_dat, data.table(data = "dpae ftc length", n_obs = nrow(dat), 
#                                        n_firms = length(unique(dat$siren)),
#                                        mean_tot_emp = mean(dat$n),
#                                        mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

# Create variables --------------------------------------------------------

# Wage gap variable
dat[, gap := m_lhw_res4_cdd - m_lhw_res4_cdi]


# # Trimming the VA variables
# trim_level <- 0.01
# dat[, vaht_bw_t := ifelse(between(vaht_bw, quantile(vaht_bw, trim_level, na.rm = TRUE), quantile(vaht_bw, 1-trim_level, na.rm = TRUE)), 
#                           vaht_bw, NA)]
# 
# # Taking the log
# dat[, l_vaht_bw := log(vaht_bw_t)]
# 
# # Restricting
# dat <- dat[!is.na(l_vaht_bw) & l_vaht_bw != -Inf]
# 
# desc_dat <- rbind(desc_dat, data.table(data = "VA info", n_obs = nrow(dat), 
#                                        n_firms = length(unique(dat$siren)),
#                                        mean_tot_emp = mean(dat$n),
#                                        mean_wage = mean(dat$m_lhw, na.rm = TRUE)))
# 
# # VA per worker quantiles
# y_quant <- quantile(dat$l_vaht_bw, probs = (0:4)/4)
# y_quant[1] <- -Inf
# y_quant[5] <- +Inf
# 
# dat[, l_vaht_bw_q := cut(l_vaht_bw, breaks = y_quant)]
# 
# # FTC length quantiles
# y_quant <- quantile(dat$m_cdd_dur, probs = (0:4)/4)
# y_quant[1] <- -Inf
# y_quant[5] <- +Inf
# 
# dat[, m_cdd_dur_q := cut(m_cdd_dur, breaks = y_quant)]


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

# # Save desc_dat
# datasummary_df(desc_dat[, names(desc_dat)[-1] := lapply(.SD, function(x) round(x, 2)), .SDcols = names(desc_dat)[-1]], 
#                fmt = NULL,
#                notes = "foo",
#                output = here("Results", "Firm gap binscatters", paste0("tab_desc_dat_firm_gap", sav_suf, ".tex")))

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Gaps --------------------------------------------------------------------

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------



# Log value-added per worker ----------------------------------------------

# Overall
est_va <- binsreg(data = dat, y = gap, x = m_l_vaht_bw, line = c(3,3), ci = c(3,3), cb = c(3,3))

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
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_res", sav_suf, ".png")))

est_va_w <- binsreg(data = dat, y = gap, x = m_l_vaht_bw, weights = n, line = c(3,3), ci = c(3,3), cb = c(3,3))

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
est <- binsreg(data = dat, y = gap, x = m_l_vaht_bw, by = m_cdd_dur_q, line = c(3,3), ci = c(3,3), cb = c(3,3))

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
                     y = gap, x = m_l_vaht_bw, by = name_eng, dots = c(3,3), line = c(3,3), ci = c(3,3), cb = c(3,3))

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



# Log value-added per worker residualized ----------------------------------------------

# Overall
est_va <- binsreg(data = dat, y = gap, x = m_l_vaht_bw_res4, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est_va$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]

p <- ggplot() + labs(x = "Log VA per worker (residualized)", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
p
ggsave(p, filename = here("Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_res", sav_suf, ".png")))

est_va_w <- binsreg(data = dat, y = gap, x = m_l_vaht_bw_res2, weights = n, line = c(3,3), ci = c(3,3), cb = c(3,3))

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


# Average CDD length residualized ----------------------------------------------

# Overall
est_l <- binsreg(data = dat, y = gap, x = m_cdd_dur_res4, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est_l$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)
tp_cb <- tp_cb[between(x, min(tp_p$x) - 2, max(tp_p$x) + 10)]

p <- ggplot() + labs(x = "Average FTC length in days (residualized)", y = "Firm gaps in log hourly wage") +
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


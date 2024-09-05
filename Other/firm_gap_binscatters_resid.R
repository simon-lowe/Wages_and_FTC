rm(list = ls())
gc()

# Saving suffix -----------------------------------------------------------

sav_suf <- ""
# sav_suf <- "_30plus"

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
# dat <- read.fst(here("Data", "ss_panel_1014_collapseSiren_hiring_wage_v2.fst"), as.data.table = TRUE)
# dat <- read.fst(here("Data", "ss_panel_1014_collapseSiren_hiring_wage_v3.fst"), as.data.table = TRUE)
# dat <- read.fst(here("Data", "ss_panel_1014_collapseSiren_hiring_wage_v3_age_rest.fst"), as.data.table = TRUE)
dat <- read.fst(here("Data", "ss_panel_1014_collapseSiren_hiring_wage_siren_cs_ya.fst"), as.data.table = TRUE)
# dat <- read.fst(here("Data", "ss_panel_1014_collapseSiren_hiring_wage_siren_cs.fst"), as.data.table = TRUE)

# Drop the arts and entertainment industry
dat <- dat[a38 != "RZ"]

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


# # DPAE information
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
dat[, gap0 := m_lhw_cdd - m_lhw_cdi]


# VA per worker quantiles
y_quant <- quantile(dat$m_l_vaht_bw, probs = (0:4)/4)
y_quant[1] <- -Inf
y_quant[5] <- +Inf

dat[, m_l_vaht_bw_q := cut(m_l_vaht_bw, breaks = y_quant)]

y_quant <- quantile(dat$m_l_vaht_bw_res4, probs = (0:4)/4)
y_quant[1] <- -Inf
y_quant[5] <- +Inf

dat[, m_l_vaht_bw_res4_q := cut(m_l_vaht_bw_res4, breaks = y_quant)]

# FTC length quantiles
y_quant <- quantile(dat$m_cdd_dur, probs = (0:4)/4)
y_quant[1] <- -Inf
y_quant[5] <- +Inf

dat[, m_cdd_dur_q := cut(m_cdd_dur, breaks = y_quant)]

y_quant <- quantile(dat$m_cdd_dur_res4, probs = (0:4)/4)
y_quant[1] <- -Inf
y_quant[5] <- +Inf

dat[, m_cdd_dur_res4_q := cut(m_cdd_dur_res4, breaks = y_quant)]


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
#                output = here("New Results", "Firm gap binscatters", paste0("tab_desc_dat_firm_gap", sav_suf, ".tex")))

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Gaps --------------------------------------------------------------------

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------



# Log value-added per worker ----------------------------------------------

# Levels plot
est_va_cdd_raw <- binsreg(data = dat, y = m_lhw_cdd, x = m_l_vaht_bw, nbins = 20)
est_va_cdi_raw <- binsreg(data = dat, y = m_lhw_cdi, x = m_l_vaht_bw, nbins = 20)

est_va_cdd_res <- binsreg(data = dat, y = m_lhw_res4_cdd, x = m_l_vaht_bw, nbins = 20)
est_va_cdi_res <- binsreg(data = dat, y = m_lhw_res4_cdi, x = m_l_vaht_bw, nbins = 20)

est_va_cdd_res2 <- binsreg(data = dat, y = m_lhw_res4_cdd, x = m_l_vaht_bw_res4, nbins = 20)
est_va_cdi_res2 <- binsreg(data = dat, y = m_lhw_res4_cdi, x = m_l_vaht_bw_res4, nbins = 20)

tp0 <- est_va_cdd_raw$data.plot$`Group Full Sample` 
tp1 <- est_va_cdi_raw$data.plot$`Group Full Sample`
tp2 <- est_va_cdd_res$data.plot$`Group Full Sample`
tp3 <- est_va_cdi_res$data.plot$`Group Full Sample`
tp4 <- est_va_cdd_res2$data.plot$`Group Full Sample`
tp5 <- est_va_cdi_res2$data.plot$`Group Full Sample`

l_text <- c("FTC", "OEC")

tp_p1 <- setDT(tp0$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p1 <- rbind(tp_p1, setDT(tp1$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])

tp_p2 <- setDT(tp2$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p2 <- rbind(tp_p2, setDT(tp3$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])

tp_p3 <- setDT(tp4$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p3 <- rbind(tp_p3, setDT(tp5$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])

bin_cutoff <- 0

p1 <- ggplot(data = tp_p1[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Log VA per worker", y = "Log hourly wage") +
  geom_point() +
  # geom_smooth(method = lm, se = FALSE) +
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

p2 <- ggplot(data = tp_p2[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Log VA per worker", y = "Log hourly wage (residualized)") +
  geom_point() +
  # geom_smooth(method = lm, se = FALSE) +
  scale_color_discrete(breaks = l_text) + scale_fill_discrete(breaks = l_text) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("Wage residualized")
p2

p3 <- ggplot(data = tp_p3[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Log VA per worker (residualized)", y = "Log hourly wage (residualized)") +
  geom_point() +
  # geom_smooth(method = lm, se = FALSE) +
  scale_color_discrete(breaks = l_text) + scale_fill_discrete(breaks = l_text) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("VA residualized")
p3

p <- p1 + p2 + p3 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_levels_lvaht_full", sav_suf, ".png")),
       width = 15, height = 10)


bin_cutoff <- 2

p1 <- ggplot(data = tp_p1[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Log VA per worker", y = "Log hourly wage") +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
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

p2 <- ggplot(data = tp_p2[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Log VA per worker", y = "Log hourly wage (residualized)") +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_color_discrete(breaks = l_text) + scale_fill_discrete(breaks = l_text) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("Wage residualized")
p2

p3 <- ggplot(data = tp_p3[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Log VA per worker (residualized)", y = "Log hourly wage (residualized)") +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  scale_color_discrete(breaks = l_text) + scale_fill_discrete(breaks = l_text) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("VA residualized")
p3

p <- p1 + p2 + p3 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_levels_lvaht_cut", sav_suf, ".png")),
       width = 15, height = 10)

# Overall
est_va <- binsreg(data = dat, y = gap, x = m_l_vaht_bw_res4, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

tp <- est_va$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit", "bin")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]

p <- ggplot() + labs(x = "Log VA per worker (residualized)", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  # geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_res", sav_suf, ".png")))

p <- ggplot() + labs(x = "Log VA per worker (residualized)", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p[bin > 2], aes(x = (x), y = fit)) +
  # geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_res_cut", sav_suf, ".png")))

est_va_w <- binsreg(data = dat, y = gap, x = m_l_vaht_bw_res4, weights = n, line = c(3,3), ci = c(3,3), cb = c(3,3))

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
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_w_res", sav_suf, ".png")))

# Regressions
reg_cut_off1 <- 3.4
reg_cut_off2 <- -0.5
reg1 <- feols(data = dat[m_l_vaht_bw > reg_cut_off1], m_lhw_cdd ~ m_l_vaht_bw)
reg2 <- feols(data = dat[m_l_vaht_bw > reg_cut_off1], m_lhw_cdi ~ m_l_vaht_bw)
reg3 <- feols(data = dat[m_l_vaht_bw > reg_cut_off1], m_lhw_res4_cdd ~ m_l_vaht_bw)
reg4 <- feols(data = dat[m_l_vaht_bw > reg_cut_off1], m_lhw_res4_cdi ~ m_l_vaht_bw)
reg5 <- feols(data = dat[m_l_vaht_bw_res4 > reg_cut_off2], m_lhw_res4_cdd ~ m_l_vaht_bw_res4)
reg6 <- feols(data = dat[m_l_vaht_bw_res4 > reg_cut_off2], m_lhw_res4_cdi ~ m_l_vaht_bw_res4)
etable(reg1, reg2, reg3, reg4, reg5, reg6,
       title = "Restricted regression of log hourly wage on log VA per worker",
       dict = c(m_lhw_cdd = "FTC log hourly wage", m_lhw_cdi = "OEC log hourly wage", m_lhw_res4_cdd = "Residualized FTC log hourly wage",
                m_lhw_res4_cdi = "Residualized OEC log hourly wage", m_l_vaht_bw = "Log Value Added per worker", m_l_vaht_bw_res4 = "Residualized Log Value Added per worker"),
       drop = "Constant",
       cluster = ~siren,
       tex = TRUE, file = here("Results", "Firm gap binscatters", paste0("table_rest_lhw_va", sav_suf, ".tex")), style.tex = style.tex("aer"))


# By year
est <- binsreg(data = dat, y = gap, x = m_l_vaht_bw_res4, by = year, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

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

tp[, group := factor(group)]
# tp[, group := factor(group, levels = levels(dat$year))]
# tp2[, group := factor(group, levels = levels(dat$year))]
# tp3[, group := factor(group, levels = levels(dat$year))]

p <- tp[bin > 2] %>% ggplot(aes(x = (x), y = fit, color = group)) + 
  geom_point() +
  # geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r, fill = group), alpha = 0.2) +
  geom_smooth(method = lm, se = FALSE) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Log VA per worker (residualized)", y = "Firm gaps in log hourly wage (residualized)", color = "Year") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12))
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_byyear", sav_suf, ".png")))


# By FTC length quartiles (residualized)
est <- binsreg(data = dat, y = gap, x = m_l_vaht_bw_res4, by = m_cdd_dur_res4_q, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

tp <- data.table()
tp2 <- data.table()
tp3 <- data.table()
for(i in 1:length(est$data.plot)){
  tp <- rbind(tp, est$data.plot[[i]]$data.dots)
  tp2 <- rbind(tp2, est$data.plot[[i]]$data.ci)
  tp3 <- rbind(tp3, est$data.plot[[i]]$data.cb)
}

tmp <- tp2[, .(x_min = min(x), x_max = max(x)), by = group]

tp[, group := factor(group, levels = levels(dat$m_cdd_dur_res4_q))]

p <- tp[bin > 2] %>% ggplot(aes(x = x, y = fit, color = group)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Log VA per worker (residualized)", y = "Firm gaps in log hourly wage (residualized)", color = "FTC length (residualized)", fill = "FTC length (residualized)") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom")
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_byFTCl_res", sav_suf, ".png")),
       width = 8, height = 6)

# By FTC length quartiles
est <- binsreg(data = dat, y = gap, x = m_l_vaht_bw_res4, by = m_cdd_dur_q, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

tp <- data.table()
tp2 <- data.table()
tp3 <- data.table()
for(i in 1:length(est$data.plot)){
  tp <- rbind(tp, est$data.plot[[i]]$data.dots)
  tp2 <- rbind(tp2, est$data.plot[[i]]$data.ci)
  tp3 <- rbind(tp3, est$data.plot[[i]]$data.cb)
}

tmp <- tp2[, .(x_min = min(x), x_max = max(x)), by = group]

tp[, group := factor(group, levels = levels(dat$m_cdd_dur_q))]

p <- tp[bin > 2] %>% ggplot(aes(x = x, y = fit, color = group)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Log VA per worker (residualized)", y = "Firm gaps in log hourly wage (residualized)", color = "FTC length", fill = "FTC length") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom")
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_byFTCl", sav_suf, ".png")),
       width = 8, height = 6)

# By industry (residualized)
est_byind <- binsreg(data = dat[a10 != "AZ"],
                     y = gap, x = m_l_vaht_bw_res4, by = name_eng, dots = c(3,3), line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

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

p <- tp %>% ggplot(aes(x = (x), y = fit)) + 
  labs(x = "Log VA per worker (residualized)", y = "Firm gaps in log hourly wage (residualized)") +
  geom_point() +
  # geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14)) +
  facet_wrap(~group, nrow = 3, scales = "free_y", labeller = labeller(group = label_wrap_gen(30)))
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_res_byind", sav_suf, ".png")), width = 10, height = 10)

# By a38 industry
tmp_dat <- dat
tmp_dat[, n_ind := .N, by = a38]

est_byind <- binsreg(data = tmp_dat[a10 != "AZ" & n_ind > 1e2],
                     y = gap, x = m_l_vaht_bw_res4, by = a38, dots = c(3,3), line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

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
  # geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
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


# Average FTC length ----------------------------------------------

# Levels plot
est_va_cdd_raw <- binsreg(data = dat, y = m_lhw_cdd, x = m_cdd_dur, nbins = 20)
est_va_cdi_raw <- binsreg(data = dat, y = m_lhw_cdi, x = m_cdd_dur, nbins = 20)

est_va_cdd_res <- binsreg(data = dat, y = m_lhw_res4_cdd, x = m_cdd_dur, nbins = 20)
est_va_cdi_res <- binsreg(data = dat, y = m_lhw_res4_cdi, x = m_cdd_dur, nbins = 20)

est_va_cdd_res2 <- binsreg(data = dat, y = m_lhw_res4_cdd, x = m_cdd_dur_res4, nbins = 20)
est_va_cdi_res2 <- binsreg(data = dat, y = m_lhw_res4_cdi, x = m_cdd_dur_res4, nbins = 20)

tp0 <- est_va_cdd_raw$data.plot$`Group Full Sample` 
tp1 <- est_va_cdi_raw$data.plot$`Group Full Sample`
tp2 <- est_va_cdd_res$data.plot$`Group Full Sample`
tp3 <- est_va_cdi_res$data.plot$`Group Full Sample`
tp4 <- est_va_cdd_res2$data.plot$`Group Full Sample`
tp5 <- est_va_cdi_res2$data.plot$`Group Full Sample`

l_text <- c("FTC", "OEC")

tp_p1 <- setDT(tp0$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p1 <- rbind(tp_p1, setDT(tp1$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])

tp_p2 <- setDT(tp2$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p2 <- rbind(tp_p2, setDT(tp3$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])

tp_p3 <- setDT(tp4$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p3 <- rbind(tp_p3, setDT(tp5$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])

bin_cutoff <- 0

p1 <- ggplot(data = tp_p1[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Average FTC length", y = "Log hourly wage") +
  geom_point() +
  # geom_smooth(method = lm, se = FALSE) +
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

p2 <- ggplot(data = tp_p2[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Average FTC length", y = "Log hourly wage (residualized)") +
  geom_point() +
  # geom_smooth(method = lm, se = FALSE) +
  scale_color_discrete(breaks = l_text) + scale_fill_discrete(breaks = l_text) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("Wage residualized")
p2

p3 <- ggplot(data = tp_p3[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Average FTC length (residualized)", y = "Log hourly wage (residualized)") +
  geom_point() +
  # geom_smooth(method = lm, se = FALSE) +
  scale_color_discrete(breaks = l_text) + scale_fill_discrete(breaks = l_text) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("FTC length residualized")
p3

p <- p1 + p2 + p3 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_levels_ftcl_full", sav_suf, ".png")),
       width = 15, height = 10)


# Overall
est_va <- binsreg(data = dat, y = gap, x = m_cdd_dur_res4, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

tp <- est_va$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit", "bin")]

p <- ggplot() + labs(x = "Average FTC length (residualized)", y = "Firm gaps in log hourly wage (residualized)") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  # geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_ftcl_res", sav_suf, ".png")))


est_va <- binsreg(data = dat, y = gap, x = m_cdd_dur, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

tp <- est_va$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit", "bin")]

p <- ggplot() + labs(x = "Average FTC length", y = "Firm gaps in log hourly wage (residualized)") +
  geom_point(data = tp_p[bin > 2], aes(x = (x), y = fit)) +
  # geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_ftcl", sav_suf, ".png")))

# By year
est <- binsreg(data = dat, y = gap, x = m_cdd_dur_res4, by = year, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

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

tp[, group := factor(group)]
# tp[, group := factor(group, levels = levels(dat$year))]
# tp2[, group := factor(group, levels = levels(dat$year))]
# tp3[, group := factor(group, levels = levels(dat$year))]

p <- tp %>% ggplot(aes(x = (x), y = fit, color = group)) + 
  geom_point() +
  # geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r, fill = group), alpha = 0.2) +
  # geom_smooth(method = lm, se = FALSE) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Log VA per worker (residualized)", y = "Firm gaps in log hourly wage (residualized)", color = "Year") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12))
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_byyear", sav_suf, ".png")))


# By VA per worker (residualized)
est <- binsreg(data = dat, y = gap, x = m_cdd_dur_res4, by = m_l_vaht_bw_res4_q, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

tp <- data.table()
for(i in 1:length(est$data.plot)){
  tp <- rbind(tp, est$data.plot[[i]]$data.dots)
}

tp[, group := factor(group, levels = levels(dat$m_l_vaht_bw_res4_q))]

p <- tp %>% ggplot(aes(x = x, y = fit, color = group)) + 
  geom_point() +
  # geom_smooth(method = lm, se = FALSE) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Average FTC length (residualized)", y = "Firm gaps in log hourly wage (residualized)", color = "Log VA per worker (residualized)") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom")
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_FTCl_bylvahtbw_res", sav_suf, ".png")),
       width = 8, height = 6)

# By FTC length quartiles
est <- binsreg(data = dat, y = gap, x = m_cdd_dur_res4, by = m_l_vaht_bw_q, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

tp <- data.table()
for(i in 1:length(est$data.plot)){
  tp <- rbind(tp, est$data.plot[[i]]$data.dots)
}

tp[, group := factor(group, levels = levels(dat$m_l_vaht_bw_q))]

p <- tp %>% ggplot(aes(x = x, y = fit, color = group)) + 
  geom_point() +
  # geom_smooth(method = lm, se = FALSE) +
  geom_hline(aes(yintercept = 0)) +
  labs(x = "Average FTC length (residualized)", y = "Firm gaps in log hourly wage (residualized)", color = "Log VA per worker") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.position = "bottom")
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_FTCl_bylvahtbw", sav_suf, ".png")),
       width = 8, height = 6)

# By industry (residualized)
est_byind <- binsreg(data = dat[a10 != "AZ"],
                     y = gap, x = m_cdd_dur_res4, by = name_eng, dots = c(3,3), line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

tp <- data.table()
for(i in 1:length(est_byind$data.plot)){
  tp <- rbind(tp, est_byind$data.plot[[i]]$data.dots)
}


p <- tp %>% ggplot(aes(x = (x), y = fit)) + 
  labs(x = "Average FTC length (residualized)", y = "Firm gaps in log hourly wage (residualized)") +
  geom_point() +
  # geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14)) +
  facet_wrap(~group, nrow = 3, scales = "free_y", labeller = labeller(group = label_wrap_gen(30)))
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_ftcl_res_byind", sav_suf, ".png")), width = 10, height = 10)

# By industry 
est_byind <- binsreg(data = dat[a10 != "AZ"],
                     y = gap, x = m_cdd_dur, by = name_eng, dots = c(3,3), line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

tp <- data.table()
for(i in 1:length(est_byind$data.plot)){
  tp <- rbind(tp, est_byind$data.plot[[i]]$data.dots)
}


p <- tp %>% ggplot(aes(x = (x), y = fit)) + 
  labs(x = "Average FTC length", y = "Firm gaps in log hourly wage (residualized)") +
  geom_point() +
  # geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14)) +
  facet_wrap(~group, nrow = 3, scales = "free_y", labeller = labeller(group = label_wrap_gen(30)))
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_ftcl_byind", sav_suf, ".png")), width = 10, height = 10)


# Firm Fixed Effects ----------------------------------------------

# Load data
akm_dat <- read.csv(here("Data", "akm_1014_full_withc_siren.csv"))
setDT(akm_dat)

akm_dat <- akm_dat[, .(original_j, psi_hat)]

akm_dat[, siren := str_pad(original_j, 9, pad = "0")]

# Collapse data
akm_dat <- akm_dat[, .(psi = mean(psi_hat), n = .N), by = siren]

# Merge data
dat <- merge(dat, akm_dat, by = "siren")

# Levels plot - 100 bins
est_va_cdd_raw <- binsreg(data = dat, y = m_lhw_cdd, x = psi, nbins = 100)
est_va_cdi_raw <- binsreg(data = dat, y = m_lhw_cdi, x = psi, nbins = 100)

est_va_cdd_res <- binsreg(data = dat, y = m_lhw_res4_cdd, x = psi, nbins = 100)
est_va_cdi_res <- binsreg(data = dat, y = m_lhw_res4_cdi, x = psi, nbins = 100)

tp0 <- est_va_cdd_raw$data.plot$`Group Full Sample` 
tp1 <- est_va_cdi_raw$data.plot$`Group Full Sample`
tp2 <- est_va_cdd_res$data.plot$`Group Full Sample`
tp3 <- est_va_cdi_res$data.plot$`Group Full Sample`

l_text <- c("FTC", "OEC")

tp_p1 <- setDT(tp0$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p1 <- rbind(tp_p1, setDT(tp1$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])

tp_p2 <- setDT(tp2$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p2 <- rbind(tp_p2, setDT(tp3$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])


bin_cutoff <- 0

p1 <- ggplot(data = tp_p1[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Firm fixed-effect", y = "Log hourly wage") +
  geom_point() +
  # geom_smooth(method = lm, se = FALSE) +
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

p2 <- ggplot(data = tp_p2[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Firm fixed-effect", y = "Log hourly wage (residualized)") +
  geom_point() +
  # geom_smooth(method = lm, se = FALSE) +
  scale_color_discrete(breaks = l_text) + scale_fill_discrete(breaks = l_text) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("Wage residualized")
p2

p <- p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_levels_akm_100", sav_suf, ".png")),
       width = 10, height = 8)

# Levels plot - 20 bins
est_va_cdd_raw <- binsreg(data = dat, y = m_lhw_cdd, x = psi, nbins = 20)
est_va_cdi_raw <- binsreg(data = dat, y = m_lhw_cdi, x = psi, nbins = 20)

est_va_cdd_res <- binsreg(data = dat, y = m_lhw_res4_cdd, x = psi, nbins = 20)
est_va_cdi_res <- binsreg(data = dat, y = m_lhw_res4_cdi, x = psi, nbins = 20)

tp0 <- est_va_cdd_raw$data.plot$`Group Full Sample` 
tp1 <- est_va_cdi_raw$data.plot$`Group Full Sample`
tp2 <- est_va_cdd_res$data.plot$`Group Full Sample`
tp3 <- est_va_cdi_res$data.plot$`Group Full Sample`

l_text <- c("FTC", "OEC")

tp_p1 <- setDT(tp0$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p1 <- rbind(tp_p1, setDT(tp1$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])

tp_p2 <- setDT(tp2$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p2 <- rbind(tp_p2, setDT(tp3$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])


bin_cutoff <- 0

p1 <- ggplot(data = tp_p1[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Firm fixed-effect", y = "Log hourly wage") +
  geom_point() +
  # geom_smooth(method = lm, se = FALSE) +
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

p2 <- ggplot(data = tp_p2[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Firm fixed-effect", y = "Log hourly wage (residualized)") +
  geom_point() +
  # geom_smooth(method = lm, se = FALSE) +
  scale_color_discrete(breaks = l_text) + scale_fill_discrete(breaks = l_text) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("Wage residualized")
p2

p <- p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_levels_akm_20", sav_suf, ".png")),
       width = 10, height = 8)

# Contract Firm Fixed Effects vs VA ----------------------------------------------

# Load separate AKMs
dat_cdd <- read.csv(here("Data", "akm_1014_ftc_withc_siren.csv"))
dat_cdi <- read.csv(here("Data", "akm_1014_oec_withc_siren.csv"))
setDT(dat_cdd)
setDT(dat_cdi)

dat_cdd2 <- dat_cdd[, .(original_j, psi_hat)][, .(psi_cdd = mean(psi_hat), n_cdd = .N), by = original_j]
dat_cdi2 <- dat_cdi[, .(original_j, psi_hat)][, .(psi_cdi = mean(psi_hat), n_cdi = .N), by = original_j]

dat_cdd2[, siren := str_pad(original_j, 9, pad = "0")]
dat_cdi2[, siren := str_pad(original_j, 9, pad = "0")]

akm_dat <- merge(dat_cdd2, dat_cdi2, by = "siren")
# setnames(akm_dat, "original_j", "siren")

# Merge data
dat <- merge(dat, akm_dat, by = "siren")

# Levels plot - 100 bins
est_va_cdd_raw <- binsreg(data = dat, y = psi_cdd, x = m_l_vaht_bw, nbins = 100)
est_va_cdi_raw <- binsreg(data = dat, y = psi_cdi, x = m_l_vaht_bw, nbins = 100)

est_va_cdd_res <- binsreg(data = dat, y = psi_cdd, x = m_l_vaht_bw_res4, nbins = 100)
est_va_cdi_res <- binsreg(data = dat, y = psi_cdi, x = m_l_vaht_bw_res4, nbins = 100)

tp0 <- est_va_cdd_raw$data.plot$`Group Full Sample` 
tp1 <- est_va_cdi_raw$data.plot$`Group Full Sample`
tp2 <- est_va_cdd_res$data.plot$`Group Full Sample`
tp3 <- est_va_cdi_res$data.plot$`Group Full Sample`

l_text <- c("FTC", "OEC")

tp_p1 <- setDT(tp0$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p1 <- rbind(tp_p1, setDT(tp1$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])

tp_p2 <- setDT(tp2$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p2 <- rbind(tp_p2, setDT(tp3$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])


bin_cutoff <- 0

p1 <- ggplot(data = tp_p1[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Log Value added per worker", y = "Firm fixed-effect") +
  geom_point() +
  # geom_smooth(method = lm, se = FALSE) +
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

p2 <- ggplot(data = tp_p2[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + labs(x = " Residualized Log Value added per worker", y = "Firm fixed-effect") +
  geom_point() +
  # geom_smooth(method = lm, se = FALSE) +
  scale_color_discrete(breaks = l_text) + scale_fill_discrete(breaks = l_text) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom", legend.title = element_blank()) +
  ggtitle("VA residualized")
p2

p <- p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
p
ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("VA_vscontractAKM_100", sav_suf, ".png")),
       width = 10, height = 8)


# Log VA per worker and HHI -----------------------------------------------

# hhi_cdd <- read.fst(here("Data", "hhi_ind_sir_cdd.fst"), as.data.table = TRUE)
hhi_cdd <- read.fst(here("Data", "hhi_cs_sir_cdd_ya1014.fst"), as.data.table = TRUE)
# hhi_cdd <- read.fst(here("Data", "hhi_cs_sir_cdd.fst"), as.data.table = TRUE)
setnames(hhi_cdd, "hhi", "hhi_cdd")
# hhi_cdi <- read.fst(here("Data", "hhi_ind_sir_cdi.fst"), as.data.table = TRUE)
hhi_cdi <- read.fst(here("Data", "hhi_cs_sir_cdi_ya1014.fst"), as.data.table = TRUE)
# hhi_cdi <- read.fst(here("Data", "hhi_cs_sir_cdi.fst"), as.data.table = TRUE)
setnames(hhi_cdi, "hhi", "hhi_cdi")

# dat2 <- merge(dat, hhi_cdd, by = c("zempt10", "a38", "year"))
dat2 <- merge(dat, hhi_cdd, by = c("zempt10", "cs_clean"))
# dat2 <- merge(dat, hhi_cdd, by = c("zempt10", "cs_clean", "year"))
# dat3 <- merge(dat2, hhi_cdi, by = c("zempt10", "a38", "year"))
dat3 <- merge(dat2, hhi_cdi, by = c("zempt10", "cs_clean"))
# dat3 <- merge(dat2, hhi_cdi, by = c("zempt10", "cs_clean", "year"))

y_quant <- quantile(dat3$hhi_cdd, probs = (0:4)/4)
y_quant[1] <- -Inf
y_quant[5] <- +Inf

dat3[, hhi_cdd_q := cut(hhi_cdd, breaks = y_quant)]

y_quant <- quantile(dat3$hhi_cdi, probs = (0:4)/4)
y_quant[1] <- -Inf
y_quant[5] <- +Inf

dat3[, hhi_cdi_q := cut(hhi_cdi, breaks = y_quant)]


# est1 <- binsreg(data = dat3, y = m_lhw_res4_cdd, x = m_l_vaht_bw_res4, by = hhi_cdi_q, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)
# est2 <- binsreg(data = dat3, y = m_lhw_res4_cdi, x = m_l_vaht_bw_res4, by = hhi_cdi_q, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)
# 
# tp <- data.table()
# tp2 <- data.table()
# for(i in 1:length(est1$data.plot)){
#   tp <- rbind(tp, est1$data.plot[[i]]$data.dots)
#   tp2 <- rbind(tp2, est2$data.plot[[i]]$data.dots)
# }
# 
# l_text <- c("FTC", "OEC")
# 
# tp_p1 <- tp[, c("x", "fit", "bin", "group")][, ct := l_text[1]]
# tp_p1 <- rbind(tp_p1, tp2[, c("x", "fit", "bin", "group")][, ct := l_text[2]])
# 
# 
# p <- tp_p1[bin > 2] %>%
#   ggplot(aes(x = x, y = fit, color = ct)) +
#   geom_point() +
#   geom_smooth(method = lm, se = FALSE) +
#   geom_abline(slope = 0.1, linetype = "dashed") +
#   facet_wrap(~group, nrow = 2, scales = "free_y") +
#   theme_classic() +
#   theme(
#     text = element_text(size = 14),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     legend.text = element_text(size = 14),
#     legend.position = "bottom", legend.title = element_blank())
# p  
# 
# 
# # Regressions
# reg_cut_off1 <- 3.4
# reg_cut_off2 <- -0.5
# # reg1 <- feols(data = dat[m_l_vaht_bw > reg_cut_off1], m_lhw_cdd ~ m_l_vaht_bw)
# # reg2 <- feols(data = dat[m_l_vaht_bw > reg_cut_off1], m_lhw_cdi ~ m_l_vaht_bw)
# # reg3 <- feols(data = dat[m_l_vaht_bw > reg_cut_off1], m_lhw_res4_cdd ~ m_l_vaht_bw)
# # reg4 <- feols(data = dat[m_l_vaht_bw > reg_cut_off1], m_lhw_res4_cdi ~ m_l_vaht_bw)
# 
# 
# 
# reg5 <- feols(data = dat3[m_l_vaht_bw > reg_cut_off1], m_lhw_res4_cdd ~ m_l_vaht_bw, split = ~hhi_cdd_q)
# reg6 <- feols(data = dat3[m_l_vaht_bw > reg_cut_off1], m_lhw_res4_cdi ~ m_l_vaht_bw, split = ~hhi_cdd_q)
# # reg5 <- feols(data = dat3[m_l_vaht_bw_res4 > reg_cut_off2], m_lhw_res4_cdd ~ m_l_vaht_bw_res4, split = ~hhi_cdd_q)
# # reg6 <- feols(data = dat3[m_l_vaht_bw_res4 > reg_cut_off2], m_lhw_res4_cdi ~ m_l_vaht_bw_res4, split = ~hhi_cdd_q)
# # etable(reg1, reg2, reg3, reg4, reg5, reg6,
# etable(reg5, reg6,
#        title = "Restricted regression of log hourly wage on log VA per worker",
#        dict = c(m_lhw_cdd = "FTC log hourly wage", m_lhw_cdi = "OEC log hourly wage", m_lhw_res4_cdd = "Residualized FTC log hourly wage",
#                 m_lhw_res4_cdi = "Residualized OEC log hourly wage", m_l_vaht_bw = "Log Value Added per worker", m_l_vaht_bw_res4 = "Residualized Log Value Added per worker"),
#        drop = "Constant",
#        cluster = ~siren)
# 
# reg5 <- feols(data = dat3[m_l_vaht_bw > reg_cut_off1], m_lhw_res4_cdd ~ m_l_vaht_bw, split = ~hhi_cdi_q)
# reg6 <- feols(data = dat3[m_l_vaht_bw > reg_cut_off1], m_lhw_res4_cdi ~ m_l_vaht_bw, split = ~hhi_cdi_q)
# # reg5 <- feols(data = dat3[m_l_vaht_bw_res4 > reg_cut_off2], m_lhw_res4_cdd ~ m_l_vaht_bw_res4, split = ~hhi_cdi_q)
# # reg6 <- feols(data = dat3[m_l_vaht_bw_res4 > reg_cut_off2], m_lhw_res4_cdi ~ m_l_vaht_bw_res4, split = ~hhi_cdi_q)
# etable(reg5, reg6,
#        title = "Restricted regression of log hourly wage on log VA per worker",
#        dict = c(m_lhw_cdd = "FTC log hourly wage", m_lhw_cdi = "OEC log hourly wage", m_lhw_res4_cdd = "Residualized FTC log hourly wage",
#                 m_lhw_res4_cdi = "Residualized OEC log hourly wage", m_l_vaht_bw = "Log Value Added per worker", m_l_vaht_bw_res4 = "Residualized Log Value Added per worker"),
#        drop = "Constant",
#        cluster = ~siren)





levels(dat3$hhi_cdd_q) <- paste0("HHI FTC - Q", 1:4)
levels(dat3$hhi_cdi_q) <- paste0("HHI OEC - Q", 1:4)


dat3[, comb_fac_cdd1 := as.factor(paste0(hhi_cdd_q, " X ", hhi_cdi_q))]
dat3[, comb_fac_cdi1 := as.factor(paste0(hhi_cdi_q, " X ", hhi_cdd_q))]

dat3[, n_cdi := n - n_cdd]

# summary
foo <- rbind(cbind(data.frame(Data = "HHI FTC"), tidy(summary(hhi_cdd$hhi_cdd))),
             cbind(data.frame("Data" = "HHI OEC"), tidy(summary(hhi_cdi$hhi_cdi))),
             cbind(data.frame(Data = "HHI FTC after merge"), tidy(summary(dat3$hhi_cdd))),
             cbind(data.frame("Data" = "HHI OEC after merge"), tidy(summary(dat3$hhi_cdi))))
datasummary_df(foo,
               fmt = 3,
               output = here("New Results", "LLM gap binscatters", "summary_table.tex"))

# cross-table
foo <- tabyl(dat3, hhi_cdd_q, hhi_cdi_q)
datasummary_df(foo,
               fmt = 0,
               output = here("New Results", "LLM gap binscatters", "cross_table.tex"))

# Regression

reg_cut_off1 <- 3.4
reg_cut_off2 <- -0.5
reg5 <- feols(data = dat3[m_l_vaht_bw_res4 > reg_cut_off2], m_lhw_res4_cdd ~ m_l_vaht_bw_res4, split = ~comb_fac_cdi1)
reg6 <- feols(data = dat3[m_l_vaht_bw_res4 > reg_cut_off2], m_lhw_res4_cdi ~ m_l_vaht_bw_res4, split = ~comb_fac_cdd1)
# reg5 <- feols(data = dat3[m_l_vaht_bw_res4 > reg_cut_off2], m_lhw_res4_cdd ~ m_l_vaht_bw_res4, weights = ~n_cdd, split = ~comb_fac_cdi1)
# reg6 <- feols(data = dat3[m_l_vaht_bw_res4 > reg_cut_off2], m_lhw_res4_cdi ~ m_l_vaht_bw_res4, weights = ~n_cdi, split = ~comb_fac_cdd1)
etable(reg5,
       title = "Regression of log hourly FTC wage on log VA per worker, by FTC and OEC quartiles",
       dict = c(m_lhw_cdd = "FTC log hourly wage", m_lhw_cdi = "OEC log hourly wage", m_lhw_res4_cdd = "Residualized FTC log hourly wage",
                m_lhw_res4_cdi = "Residualized OEC log hourly wage", m_l_vaht_bw = "Log Value Added per worker", m_l_vaht_bw_res4 = "Residualized Log Value Added per worker"),
       drop = "Constant",
       cluster = ~siren,
       tex = TRUE, file = here("New Results", "LLM gap binscatters", paste0("reg_w_cdd_va_byCrossQHHI", sav_suf, ".tex")), style.tex = style.tex("aer"))
etable(reg6,
       title = "Regression of log hourly OEC wage on log VA per worker, by FTC and OEC quartiles",
       dict = c(m_lhw_cdd = "FTC log hourly wage", m_lhw_cdi = "OEC log hourly wage", m_lhw_res4_cdd = "Residualized FTC log hourly wage",
                m_lhw_res4_cdi = "Residualized OEC log hourly wage", m_l_vaht_bw = "Log Value Added per worker", m_l_vaht_bw_res4 = "Residualized Log Value Added per worker"),
       drop = "Constant",
       cluster = ~siren,
       tex = TRUE, file = here("New Results", "LLM gap binscatters", paste0("reg_w_cdd_va_byCrossQHHI", sav_suf, ".tex")), style.tex = style.tex("aer"))

# reg <- feols(dat3[m_l_vaht_bw_res4 > reg_cut_off2], m_lhw_res4_cdi ~ m_l_vaht_bw_res4*hhi_cdi)
# reg <- feols(dat3[m_l_vaht_bw_res4 > reg_cut_off2], m_lhw_res4_cdi ~ m_l_vaht_bw_res4*hhi_cdd)
# reg <- feols(dat3[m_l_vaht_bw_res4 > reg_cut_off2], m_lhw_res4_cdi ~ m_l_vaht_bw_res4*hhi_cdd*hhi_cdi)
# reg <- feols(dat3[m_l_vaht_bw_res4 > reg_cut_off2], m_lhw_res4_cdi ~ m_l_vaht_bw_res4*hhi_cdi, split = ~hhi_cdd_q)
# etable(reg)
# 
# ,
#        tex = TRUE, file = here("Results", "Firm gap binscatters", paste0("table_rest_lhw_va", sav_suf, ".tex")), style.tex = style.tex("aer"))






# -------------------------------------------------------------------------


# old ---------------------------------------------------------------------


# -------------------------------------------------------------------------


# # Average CDD length ----------------------------------------------
# 
# # Levels plot
# est_va_cdd_raw <- binsreg(data = dat, y = m_lhw_cdd, x = m_cdd_dur, nbins = 20)
# est_va_cdi_raw <- binsreg(data = dat, y = m_lhw_cdi, x = m_cdd_dur, nbins = 20)
# 
# est_va_cdd_res <- binsreg(data = dat, y = m_lhw_res4_cdd, x = m_cdd_dur, nbins = 20)
# est_va_cdi_res <- binsreg(data = dat, y = m_lhw_res4_cdi, x = m_cdd_dur, nbins = 20)
# 
# est_va_cdd_res2 <- binsreg(data = dat, y = m_lhw_res4_cdd, x = m_cdd_dur_res4, nbins = 20)
# est_va_cdi_res2 <- binsreg(data = dat, y = m_lhw_res4_cdi, x = m_cdd_dur_res4, nbins = 20)
# 
# tp0 <- est_va_cdd_raw$data.plot$`Group Full Sample` 
# tp1 <- est_va_cdi_raw$data.plot$`Group Full Sample`
# tp2 <- est_va_cdd_res$data.plot$`Group Full Sample`
# tp3 <- est_va_cdi_res$data.plot$`Group Full Sample`
# tp4 <- est_va_cdd_res2$data.plot$`Group Full Sample`
# tp5 <- est_va_cdi_res2$data.plot$`Group Full Sample`
# 
# l_text <- c("FTC", "OEC")
# 
# tp_p1 <- setDT(tp0$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
# tp_p1 <- rbind(tp_p1, setDT(tp1$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])
# 
# tp_p2 <- setDT(tp2$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
# tp_p2 <- rbind(tp_p2, setDT(tp3$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])
# 
# tp_p3 <- setDT(tp4$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
# tp_p3 <- rbind(tp_p3, setDT(tp5$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])
# 
# p_th <- 0
# 
# p1 <- ggplot(data = tp_p1[bin >p_th], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Average FTC length", y = "Log hourly wage") +
#   geom_point() +
#   geom_smooth(method = lm, se = FALSE) +
#   scale_color_discrete(breaks = l_text) + scale_fill_discrete(breaks = l_text) +
#   theme_classic() +
#   theme(
#     text = element_text(size = 14),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     legend.text = element_text(size = 14),
#     legend.position = "bottom", legend.title = element_blank()) +
#   ggtitle("Raw")
# p1
# 
# p2 <- ggplot(data = tp_p2[bin >p_th], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Average FTC length", y = "Log hourly wage (residualized)") +
#   geom_point() +
#   geom_smooth(method = lm, se = FALSE) +
#   scale_color_discrete(breaks = l_text) + scale_fill_discrete(breaks = l_text) +
#   theme_classic() + 
#   theme(
#     text = element_text(size = 14),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     legend.text = element_text(size = 14),
#     legend.position = "bottom", legend.title = element_blank()) +
#   ggtitle("Wage residualized")
# p2
# 
# p3 <- ggplot(data = tp_p3[bin >p_th], aes(x = (x), y = fit, color = factor(group))) + labs(x = "Average FTC length (residualized)", y = "Log hourly wage (residualized)") +
#   geom_point() +
#   geom_smooth(method = lm, se = FALSE) +
#   scale_color_discrete(breaks = l_text) + scale_fill_discrete(breaks = l_text) +
#   theme_classic() + 
#   theme(
#     text = element_text(size = 14),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     legend.text = element_text(size = 14),
#     legend.position = "bottom", legend.title = element_blank()) +
#   ggtitle("Length residualized")
# p3
# 
# p <- p1 + p2 + p3 + plot_layout(guides = "collect") & theme(legend.position = "bottom")
# p
# 
# # Overall
# est_l <- binsreg(data = dat, y = gap, x = m_cdd_dur_res4, line = c(3,3), ci = c(3,3), cb = c(3,3))
# 
# tp <- est_l$data.plot$`Group Full Sample` 
# tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
# tp_ci <- setDT(tp$data.ci)
# tp_cb <- setDT(tp$data.cb)
# tp_cb <- tp_cb[between(x, min(tp_p$x) - 2, max(tp_p$x) + 10)]
# 
# p <- ggplot() + labs(x = "Average FTC length in days", y = "Firm gaps in log hourly wage") +
#   geom_point(data = tp_p, aes(x = (x), y = fit)) +
#   geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
#   geom_hline(aes(yintercept = 0)) +
#   theme_classic()
# p
# ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lcdd", sav_suf, ".png")))
# 
# 
# est_l <- binsreg(data = dat, y = gap, x = m_cdd_dur, weights = n, line = c(3,3), ci = c(3,3), cb = c(3,3))
# 
# tp <- est_l$data.plot$`Group Full Sample` 
# tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
# tp_ci <- setDT(tp$data.ci)
# tp_cb <- setDT(tp$data.cb)
# tp_cb <- tp_cb[between(x, min(tp_p$x) - 2, max(tp_p$x) + 10)]
# 
# p <- ggplot() + labs(x = "Average FTC length in days", y = "Firm gaps in log hourly wage") +
#   geom_point(data = tp_p, aes(x = (x), y = fit)) +
#   geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
#   geom_hline(aes(yintercept = 0)) +
#   theme_classic()
# p
# ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lcdd_w", sav_suf, ".png")))
# 
# # By log VA per worker quartiles
# est <- binsreg(data = dat, y = gap, x = m_cdd_dur, by = l_vaht_bw_q, line = c(3,3), ci = c(3,3), cb = c(3,3))
# 
# tp <- data.table()
# tp2 <- data.table()
# tp3 <- data.table()
# for(i in 1:length(est$data.plot)){
#   tp <- rbind(tp, est$data.plot[[i]]$data.dots)
#   tp2 <- rbind(tp2, est$data.plot[[i]]$data.ci)
#   tp3 <- rbind(tp3, est$data.plot[[i]]$data.cb)
# }
# 
# tmp <- tp2[, .(x_min = min(x), x_max = max(x)), by = group]
# tp3 <- merge(tp3, tmp, by = "group", all = TRUE)
# tp3 <- tp3[between(x, x_min - 10, x_max + 10)]
# 
# tp[, group := factor(group, levels = levels(dat$l_vaht_bw_q))]
# tp2[, group := factor(group, levels = levels(dat$l_vaht_bw_q))]
# tp3[, group := factor(group, levels = levels(dat$l_vaht_bw_q))]
# 
# p <- tp %>% ggplot() + 
#   labs(x = "Average FTC length in days", y = "Firm gaps in log hourly wage") +
#   geom_point(data = tp, aes(x = (x), y = fit)) +
#   geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
#   geom_hline(aes(yintercept = 0)) +
#   theme_classic() + 
#   theme(
#     text = element_text(size = 14),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     legend.text = element_text(size = 14)) +
#   facet_wrap(~group, nrow = 2, scales = "free_y")
# p
# ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lcdd_byvaht", sav_suf, ".png")))
# 
# p <- tp %>% ggplot() + 
#   geom_point(data = tp, aes(x = (x), y = fit, color = group)) +
#   geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r, fill = group), alpha = 0.2) +
#   geom_hline(aes(yintercept = 0)) +
#   labs(x = "Average FTC length in days", y = "Firm gaps in log hourly wage", color = "Log VA per worker", fill = "Log VA per worker") +
#   theme_classic() +
#   theme(
#     text = element_text(size = 14),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     legend.text = element_text(size = 12))
# p
# ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lcdd_byvaht", sav_suf, ".png")))
# 
# # Single year
# est_l <- binsreg(data = dat[year == 2012], y = gap, x = m_cdd_dur, line = c(3,3), ci = c(3,3), cb = c(3,3))
# 
# tp <- est_l$data.plot$`Group Full Sample` 
# tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
# tp_ci <- setDT(tp$data.ci)
# tp_cb <- setDT(tp$data.cb)
# tp_cb <- tp_cb[between(x, min(tp_p$x) - 2, max(tp_p$x) + 10)]
# 
# p <- ggplot() + labs(x = "Average FTC length in days", y = "Firm gaps in log hourly wage") +
#   geom_point(data = tp_p, aes(x = (x), y = fit)) +
#   geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
#   geom_hline(aes(yintercept = 0)) +
#   theme_classic()
# p
# ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lcdd_y2012", sav_suf, ".png")))
# 
# # By industry
# est_byind <- binsreg(data = dat[a10 != "AZ"],
#                      y = gap, x = m_cdd_dur, by = name_eng, dots = c(3,3), line = c(3,3), ci = c(3,3), cb = c(3,3))
# 
# tp <- data.table()
# tp2 <- data.table()
# tp3 <- data.table()
# for(i in 1:length(est_byind$data.plot)){
#   tp <- rbind(tp, est_byind$data.plot[[i]]$data.dots)
#   tp2 <- rbind(tp2, est_byind$data.plot[[i]]$data.ci)
#   tp3 <- rbind(tp3, est_byind$data.plot[[i]]$data.cb)
# }
# 
# tmp <- tp2[, .(x_min = min(x), x_max = max(x)), by = group]
# tp3 <- merge(tp3, tmp, by = "group", all = TRUE)
# tp3 <- tp3[between(x, x_min-10, x_max + 20)]
# 
# p <- tp %>% ggplot() + 
#   labs(x = "Average FTC length in days", y = "Firm gaps in log hourly wage") +
#   geom_point(data = tp, aes(x = (x), y = fit)) +
#   geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
#   geom_hline(aes(yintercept = 0)) +
#   theme_classic() + 
#   theme(
#     text = element_text(size = 14),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     legend.text = element_text(size = 14)) +
#   facet_wrap(~group, nrow = 3, scales = "free_y", labeller = labeller(group = label_wrap_gen(30)))
# p
# ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lcdd_byind", sav_suf, ".png")), width = 10, height = 10)
# 
# 
# 
# # Log value-added per worker - residualized ----------------------------------------------
# 
# # Overall
# est_va <- binsreg(data = dat, y = gap, x = m_l_vaht_bw_res4, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)
# 
# tp <- est_va$data.plot$`Group Full Sample` 
# tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
# tp_ci <- setDT(tp$data.ci)
# tp_cb <- setDT(tp$data.cb)
# 
# tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]
# 
# p <- ggplot() + labs(x = "Log VA per worker (residualized)", y = "Firm gaps in log hourly wage") +
#   geom_point(data = tp_p, aes(x = (x), y = fit)) +
#   # geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
#   geom_hline(aes(yintercept = 0)) +
#   theme_classic() +
#   theme(
#     text = element_text(size = 14),
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
#     axis.text.y = element_text(size = 14))
# p
# ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_res", sav_suf, ".png")))
# 
# est_va_w <- binsreg(data = dat, y = gap, x = m_l_vaht_bw_res2, weights = n, line = c(3,3), ci = c(3,3), cb = c(3,3))
# 
# tp <- est_va_w$data.plot$`Group Full Sample` 
# tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
# tp_ci <- setDT(tp$data.ci)
# tp_cb <- setDT(tp$data.cb)
# 
# tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]
# 
# p <- ggplot() + labs(x = "Log VA per worker", y = "Firm gaps in log hourly wage") +
#   geom_point(data = tp_p, aes(x = (x), y = fit)) +
#   geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
#   geom_hline(aes(yintercept = 0)) +
#   theme_classic() +
#   theme(
#     text = element_text(size = 14),
#     axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
#     axis.text.y = element_text(size = 14))
# p
# ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_w", sav_suf, ".png")))
# 
# 
# # By year
# est <- binsreg(data = dat, y = gap, x = m_l_vaht_bw_res4, by = year, line = c(3,3), ci = c(3,3), cb = c(3,3))
# 
# tp <- data.table()
# tp2 <- data.table()
# tp3 <- data.table()
# for(i in 1:length(est$data.plot)){
#   tp <- rbind(tp, est$data.plot[[i]]$data.dots)
#   tp2 <- rbind(tp2, est$data.plot[[i]]$data.ci)
#   tp3 <- rbind(tp3, est$data.plot[[i]]$data.cb)
# }
# 
# tmp <- tp2[, .(x_min = min(x), x_max = max(x)), by = group]
# tp3 <- merge(tp3, tmp, by = "group", all = TRUE)
# tp3 <- tp3[between(x, x_min - 0.2, x_max + 0.2)]
# 
# tp[, group := factor(group)]
# # tp[, group := factor(group, levels = levels(dat$year))]
# # tp2[, group := factor(group, levels = levels(dat$year))]
# # tp3[, group := factor(group, levels = levels(dat$year))]
# 
# p <- tp %>% ggplot() + 
#   geom_point(data = tp, aes(x = (x), y = fit, color = group)) +
#   # geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r, fill = group), alpha = 0.2) +
#   geom_hline(aes(yintercept = 0)) +
#   labs(x = "Log VA per worker (residualized)", y = "Firm gaps in log hourly wage", color = "Year") +
#   theme_classic() +
#   theme(
#     text = element_text(size = 14),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     legend.text = element_text(size = 12))
# p
# ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_byyear", sav_suf, ".png")))
# 
# 
# # By FTC length quartiles
# est <- binsreg(data = dat, y = gap, x = l_vaht_bw, by = m_cdd_dur_q, line = c(3,3), ci = c(3,3), cb = c(3,3))
# 
# tp <- data.table()
# tp2 <- data.table()
# tp3 <- data.table()
# for(i in 1:length(est$data.plot)){
#   tp <- rbind(tp, est$data.plot[[i]]$data.dots)
#   tp2 <- rbind(tp2, est$data.plot[[i]]$data.ci)
#   tp3 <- rbind(tp3, est$data.plot[[i]]$data.cb)
# }
# 
# tmp <- tp2[, .(x_min = min(x), x_max = max(x)), by = group]
# tp3 <- merge(tp3, tmp, by = "group", all = TRUE)
# tp3 <- tp3[between(x, x_min - 0.2, x_max + 0.2)]
# 
# tp[, group := factor(group, levels = levels(dat$m_cdd_dur_q))]
# tp2[, group := factor(group, levels = levels(dat$m_cdd_dur_q))]
# tp3[, group := factor(group, levels = levels(dat$m_cdd_dur_q))]
# 
# p <- tp %>% ggplot() + 
#   labs(x = "Log VA per worker", y = "Firm gaps in log hourly wage") +
#   geom_point(data = tp, aes(x = (x), y = fit)) +
#   geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
#   geom_hline(aes(yintercept = 0)) +
#   theme_classic() + 
#   theme(
#     text = element_text(size = 14),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     legend.text = element_text(size = 14)) +
#   facet_wrap(~group, nrow = 2, scales = "free_y")
# p
# ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_byFTCl", sav_suf, ".png")))
# 
# p <- tp %>% ggplot() + 
#   geom_point(data = tp, aes(x = (x), y = fit, color = group)) +
#   geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r, fill = group), alpha = 0.2) +
#   geom_hline(aes(yintercept = 0)) +
#   labs(x = "Log VA per worker", y = "Firm gaps in log hourly wage", color = "FTC length", fill = "FTC length") +
#   theme_classic() +
#   theme(
#     text = element_text(size = 14),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     legend.text = element_text(size = 12))
# p
# ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_byFTCl2", sav_suf, ".png")))
# 
# # By industry
# est_byind <- binsreg(data = dat[a10 != "AZ"],
#                      y = gap, x = l_vaht_bw, by = name_eng, dots = c(3,3), line = c(3,3), ci = c(3,3), cb = c(3,3))
# 
# tp <- data.table()
# tp2 <- data.table()
# tp3 <- data.table()
# for(i in 1:length(est_byind$data.plot)){
#   tp <- rbind(tp, est_byind$data.plot[[i]]$data.dots)
#   tp2 <- rbind(tp2, est_byind$data.plot[[i]]$data.ci)
#   tp3 <- rbind(tp3, est_byind$data.plot[[i]]$data.cb)
# }
# 
# tmp <- tp2[, .(x_min = min(x), x_max = max(x)), by = group]
# tp3 <- merge(tp3, tmp, by = "group", all = TRUE)
# tp3 <- tp3[between(x, x_min-0.5, x_max + 0.5)]
# 
# p <- tp %>% ggplot() + 
#   labs(x = "Log VA per worker", y = "Firm gaps in log hourly wage") +
#   geom_point(data = tp, aes(x = (x), y = fit)) +
#   geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
#   geom_hline(aes(yintercept = 0)) +
#   theme_classic() + 
#   theme(
#     text = element_text(size = 14),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     legend.text = element_text(size = 14)) +
#   facet_wrap(~group, nrow = 3, scales = "free_y", labeller = labeller(group = label_wrap_gen(30)))
# p
# ggsave(p, filename = here("New Results", "Firm gap binscatters", paste0("hir_wage_gap_lvahtbw_byind", sav_suf, ".png")), width = 10, height = 10)
# 
# # By a38 industry
# tmp_dat <- dat
# tmp_dat[, n_ind := .N, by = a38]
# 
# est_byind <- binsreg(data = tmp_dat[a10 != "AZ" & n_ind > 1e2],
#                      y = gap, x = m_l_vaht_bw_res4, by = a38, dots = c(3,3), line = c(3,3), ci = c(3,3), cb = c(3,3))
# 
# tp <- data.table()
# tp2 <- data.table()
# tp3 <- data.table()
# for(i in 1:length(est_byind$data.plot)){
#   tp <- rbind(tp, est_byind$data.plot[[i]]$data.dots)
#   tp2 <- rbind(tp2, est_byind$data.plot[[i]]$data.ci)
#   tp3 <- rbind(tp3, est_byind$data.plot[[i]]$data.cb)
# }
# 
# tmp <- tp2[, .(x_min = min(x), x_max = max(x)), by = group]
# tp3 <- merge(tp3, tmp, by = "group", all = TRUE)
# tp3 <- tp3[between(x, x_min-0.5, x_max + 0.5)]
# 
# p <- tp %>% ggplot() + 
#   labs(x = "Log VA per worker", y = "Firm gaps in log hourly wage") +
#   geom_point(data = tp, aes(x = (x), y = fit)) +
#   geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
#   geom_hline(aes(yintercept = 0)) +
#   theme_classic() + 
#   theme(
#     text = element_text(size = 14),
#     axis.text.x = element_text(size = 14),
#     axis.text.y = element_text(size = 14),
#     legend.text = element_text(size = 14)) +
#   facet_wrap(~group, nrow = 3, scales = "free_y", labeller = labeller(group = label_wrap_gen(30)))
# p
# 
# rm(tmp_dat)
# 
# 
# # log VA - control version ------------------------------------------------
# 
# # est_va <- binsreg(data = dat, y = gap0, x = m_l_vaht_bw, line = c(3,3), ci = c(3,3), cb = c(3,3))
# # 
# # tp <- est_va$data.plot$`Group Full Sample` 
# # tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
# # tp_ci <- setDT(tp$data.ci)
# # tp_cb <- setDT(tp$data.cb)
# # 
# # tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]
# # 
# # p <- ggplot() + labs(x = "Log VA per worker (residualized)", y = "Firm gaps in log hourly wage") +
# #   geom_point(data = tp_p, aes(x = (x), y = fit)) +
# #   geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
# #   geom_hline(aes(yintercept = 0)) +
# #   theme_classic() +
# #   theme(
# #     text = element_text(size = 14),
# #     axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
# #     axis.text.y = element_text(size = 14))
# # p
# # 
# # 
# # sexe_cols <- grep("^sexe_", names(dat), value = TRUE)
# # age_cols <- grep("^age_bin_", names(dat), value = TRUE)
# # exp_cs_cols <- grep("^exp_cs_", names(dat), value = TRUE)
# # cs_cols <- grep("^cs_", names(dat), value = TRUE)
# # 
# # # control_form <- as.formula(paste("~ year + ", paste0("`", c(sexe_cols, age_cols, exp_cs_cols, cs_cols), "`", collapse = "+")))
# # control_form <- as.formula(paste("~ year + ", paste0("`", c(cs_cols), "`", collapse = "+")))
# # 
# # # est_va <- binsreg(data = dat, y = gap0, x = m_l_vaht_bw, w = control_form, line = c(3,3), ci = c(3,3), cb = c(3,3))
# # est_va <- binsreg(data = dat, y = gap0, x = m_l_vaht_bw, w = ~sexe_1 + sexe_2 + `age_bin_(17,22]` + `age_bin_(22,27]` + `age_bin_(27,32]` +   
# #                    `age_bin_(32,37]` + `age_bin_(37,42]` + `age_bin_(42,47]` + `age_bin_(47,52]` + `age_bin_(52,57]` + `age_bin_(57,62]` + `age_bin_(62,Inf]` +
# #                     cs_0 + cs_10 + cs_21 + cs_22 + cs_23 + cs_31 + cs_33 + cs_34 + cs_35 + cs_37 + cs_38 + cs_42 + cs_43 + cs_44 + cs_45 + cs_46 + cs_47 + cs_48 + cs_52 + cs_53 + cs_54 + cs_55 + cs_56 + cs_62 + cs_63 + cs_64 + cs_65 + cs_67 + cs_68 + cs_69 +
# #                     factor(a38), 
# #                   line = c(3,3), ci = c(3,3), cb = c(3,3))
# # 
# # 
# # est_va <- binsreg(data = dat, y = gap0, x = m_l_vaht_bw, w = ~cs_0 + cs_10 + cs_21 + cs_22 + cs_23 + cs_31 + cs_33 + cs_34 + cs_35 + cs_37 + cs_38 + cs_42 + cs_43 + cs_44 + cs_45 + cs_46 + cs_47 + cs_48 + cs_52 + cs_53 + cs_54 + cs_55 + cs_56 + cs_62 + cs_63 + cs_64 + cs_65 + cs_67 + cs_68 + cs_69, line = c(3,3), ci = c(3,3), cb = c(3,3))
# # est_va <- binsreg(data = dat, y = gap0, x = m_l_vaht_bw, w = ~cs_0 + cs_10 + cs_21 + cs_22 + cs_23 + cs_31 + cs_33, line = c(3,3), ci = c(3,3), cb = c(3,3))
# # 
# # tp <- est_va$data.plot$`Group Full Sample` 
# # tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
# # tp_ci <- setDT(tp$data.ci)
# # tp_cb <- setDT(tp$data.cb)
# # 
# # tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]
# # 
# # p <- ggplot() + labs(x = "Log VA per worker (residualized)", y = "Firm gaps in log hourly wage") +
# #   geom_point(data = tp_p, aes(x = (x), y = fit)) +
# #   geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
# #   geom_hline(aes(yintercept = 0)) +
# #   theme_classic() +
# #   theme(
# #     text = element_text(size = 14),
# #     axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
# #     axis.text.y = element_text(size = 14))
# # p
# 

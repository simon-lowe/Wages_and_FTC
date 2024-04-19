rm(list = ls())
gc()

# Restriction parameter ---------------------------------------------------

n_rest <- 1000

# Saving suffix -----------------------------------------------------------

sav_suf <- paste0("_rest", n_rest)

# Load packages -----------------------------------------------------------

list.of.packages <- c(
  "tidyverse", "data.table", "haven", "fst", "expss", "janitor", "here",
  "DescTools", "tictoc", "collapse",
  "fixest", "binsreg", "did", "ggplot2", "patchwork", "modelsummary"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(list.of.packages, library, character.only = TRUE))
rm(list.of.packages, new.packages)

setDTthreads(0)
setFixest_nthreads(8)


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# CS ----------------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Load data ---------------------------------------------------------------

# Descriptive stats on data merging dataset initialization
desc_dat <- data.table()

# DADS data
# dat <- read.fst(here("Data", "ss_panel_1014_collapse_csLLM_hiring_wage.fst"), as.data.table = TRUE)
dat <- read.fst(here("Data", "ss_panel_1014_collapse_csLLM_hiring_wage_ya.fst"), as.data.table = TRUE)

desc_dat <- rbind(desc_dat, data.table(data = "Full data", n_obs = nrow(dat), 
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

# Restrict to firms which have a FTC-OEC gap (ie in a given year have hired both)
dat <- dat[!is.na(m_lhw_res4_cdd) & !is.na(m_lhw_res4_cdi)]

desc_dat <- rbind(desc_dat, data.table(data = "FTC-OEC gap", n_obs = nrow(dat), 
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

# HHI data
# hhi <- read.fst(here("Data", "hhi_cs_sir.fst"), as.data.table = TRUE)
# hhi_cdd <- read.fst(here("Data", "hhi_cs_sir_cdd.fst"), as.data.table = TRUE)
# hhi_cdi <- read.fst(here("Data", "hhi_cs_sir_cdi.fst"), as.data.table = TRUE)
hhi <- read.fst(here("Data", "hhi_cs_sir_ya1014.fst"), as.data.table = TRUE)
hhi_cdd <- read.fst(here("Data", "hhi_cs_sir_cdd_ya1014.fst"), as.data.table = TRUE)
hhi_cdi <- read.fst(here("Data", "hhi_cs_sir_cdi_ya1014.fst"), as.data.table = TRUE)
setnames(hhi_cdd, "hhi", "hhi_cdd")
setnames(hhi_cdi, "hhi", "hhi_cdi")

# dat <- merge(dat, hhi, by = c("zempt10", "cs_clean", "year"))
dat <- merge(dat, hhi, by = c("zempt10", "cs_clean"))
desc_dat <- rbind(desc_dat, data.table(data = "HHI", n_obs = nrow(dat), 
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

# dat_cdd <- merge(dat, hhi_cdd, by = c("zempt10", "cs_clean", "year"))
dat_cdd <- merge(dat, hhi_cdd, by = c("zempt10", "cs_clean"))
desc_dat <- rbind(desc_dat, data.table(data = "HHI FTC", n_obs = nrow(dat), 
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

# dat_cdi <- merge(dat, hhi_cdi, by = c("zempt10", "cs_clean", "year"))
dat_cdi <- merge(dat, hhi_cdi, by = c("zempt10", "cs_clean"))
desc_dat <- rbind(desc_dat, data.table(data = "HHI OEC", n_obs = nrow(dat), 
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

# dat_cddi <- merge(dat, merge(hhi_cdi, hhi_cdd, by = c("zempt10", "cs_clean", "year")), by = c("zempt10", "cs_clean", "year"))
dat_cddi <- merge(dat, merge(hhi_cdi, hhi_cdd, by = c("zempt10", "cs_clean")), by = c("zempt10", "cs_clean"))
desc_dat <- rbind(desc_dat, data.table(data = "HHI FTC-OEC", n_obs = nrow(dat), 
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

# Save desc_dat
# datasummary_df(desc_dat[, mean_wage := round(mean_wage, 2)], 
#                fmt = NULL,
#                notes = "foo",
#                output = here("New Results", "LLM gap binscatters", paste0("tab_desc_dat_hhi_cs", sav_suf, ".tex")))

# Create variables --------------------------------------------------------

# Wage gap variable
dat[, gap := m_lhw_res4_cdd - m_lhw_res4_cdi]
dat_cdd[, gap := m_lhw_res4_cdd - m_lhw_res4_cdi]
dat_cdi[, gap := m_lhw_res4_cdd - m_lhw_res4_cdi]
dat_cddi[, gap := m_lhw_res4_cdd - m_lhw_res4_cdi]



# HHI FTC quantiles
y_quant <- quantile(dat_cddi$hhi_cdd, probs = (0:4)/4)
y_quant[1] <- -Inf
y_quant[5] <- +Inf

dat_cddi[, hhi_cdd_q := cut(hhi_cdd, breaks = y_quant)]

# HHI FTC quantiles
y_quant <- quantile(dat_cddi$hhi_cdi, probs = (0:4)/4)
y_quant[1] <- -Inf
y_quant[5] <- +Inf

dat_cddi[, hhi_cdi_q := cut(hhi_cdi, breaks = y_quant)]

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Gaps --------------------------------------------------------------------

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------




# Overall HHI ----------------------------------------------

# Levels
est1 <- binsreg(data = dat_cddi[n > n_rest & cs_clean > 0], y = m_lhw_cdd, x = hhi, nbins = 20)
est2 <- binsreg(data = dat_cddi[n > n_rest & cs_clean > 0], y = m_lhw_cdi, x = hhi, nbins = 20)

est3 <- binsreg(data = dat_cddi[n > n_rest & cs_clean > 0], y = m_lhw_res4_cdd, x = hhi, nbins = 20)
est4 <- binsreg(data = dat_cddi[n > n_rest & cs_clean > 0], y = m_lhw_res4_cdi, x = hhi, nbins = 20)

tp0 <- est1$data.plot$`Group Full Sample` 
tp1 <- est2$data.plot$`Group Full Sample`
tp2 <- est3$data.plot$`Group Full Sample`
tp3 <- est4$data.plot$`Group Full Sample`

l_text <- c("FTC", "OEC")

tp_p1 <- setDT(tp0$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p1 <- rbind(tp_p1, setDT(tp1$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])

tp_p2 <- setDT(tp2$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p2 <- rbind(tp_p2, setDT(tp3$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])


bin_cutoff <- 0

p1 <- ggplot(data = tp_p1[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + 
  labs(x = "HHI for hiring by CZ x Occupation(2-digit)", y = "Log hourly wage") +
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

p2 <- ggplot(data = tp_p2[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + labs(x = "HHI", y = "Log hourly wage (residualized)") +
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
ggsave(p, filename = here("New Results", "LLM gap binscatters", paste0("llm_wage_levels_hhi", sav_suf, ".png")),
       width = 10, height = 8)

# Gap

est <- binsreg(data = dat_cddi[n > n_rest & cs_clean > 0], y = gap, x = hhi, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.01, max(tp_p$x) + 0.01)]

p <- ggplot() + labs(x = "HHI for hiring by CZ x Occupation(2-digit)", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
p
ggsave(p, filename = here("New Results", "LLM gap binscatters", paste0("llm_wage_gap_hhi", sav_suf, ".png")))

# FTC HHI ----------------------------------------------

# Levels
est1 <- binsreg(data = dat_cddi[n > n_rest & cs_clean > 0], y = m_lhw_cdd, x = hhi_cdd, nbins = 20)
est2 <- binsreg(data = dat_cddi[n > n_rest & cs_clean > 0], y = m_lhw_cdi, x = hhi_cdd, nbins = 20)

est3 <- binsreg(data = dat_cddi[n > n_rest & cs_clean > 0], y = m_lhw_res4_cdd, x = hhi_cdd, nbins = 20)
est4 <- binsreg(data = dat_cddi[n > n_rest & cs_clean > 0], y = m_lhw_res4_cdi, x = hhi_cdd, nbins = 20)

tp0 <- est1$data.plot$`Group Full Sample` 
tp1 <- est2$data.plot$`Group Full Sample`
tp2 <- est3$data.plot$`Group Full Sample`
tp3 <- est4$data.plot$`Group Full Sample`

l_text <- c("FTC", "OEC")

tp_p1 <- setDT(tp0$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p1 <- rbind(tp_p1, setDT(tp1$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])

tp_p2 <- setDT(tp2$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p2 <- rbind(tp_p2, setDT(tp3$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])


bin_cutoff <- 0

p1 <- ggplot(data = tp_p1[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + 
  labs(x = "HHI for FTC hiring by CZ x Occupation(2-digit)", y = "Log hourly wage") +
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

p2 <- ggplot(data = tp_p2[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + 
  labs(x = "HHI for FTC hiring by CZ x Occupation(2-digit)", y = "Log hourly wage (residualized)") +
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
ggsave(p, filename = here("New Results", "LLM gap binscatters", paste0("llm_wage_levels_hhi_cdd", sav_suf, ".png")),
       width = 10, height = 8)


# Gap
est <- binsreg(data = dat_cdd[n > n_rest], y = gap, x = hhi_cdd, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.01, max(tp_p$x) + 0.01)]

p <- ggplot() + labs(x = "HHI for FTC hiring by CZ x Occupation(2-digit)", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
p
ggsave(p, filename = here("New Results", "LLM gap binscatters", paste0("llm_wage_gap_hhi_cdd", sav_suf, ".png")))

# OEC HHI ----------------------------------------------

# Levels
est1 <- binsreg(data = dat_cddi[n > n_rest], y = m_lhw_cdd, x = hhi_cdi, nbins = 20)
est2 <- binsreg(data = dat_cddi[n > n_rest], y = m_lhw_cdi, x = hhi_cdi, nbins = 20)

est3 <- binsreg(data = dat_cddi[n > n_rest], y = m_lhw_res4_cdd, x = hhi_cdi, nbins = 20)
est4 <- binsreg(data = dat_cddi[n > n_rest], y = m_lhw_res4_cdi, x = hhi_cdi, nbins = 20)

tp0 <- est1$data.plot$`Group Full Sample` 
tp1 <- est2$data.plot$`Group Full Sample`
tp2 <- est3$data.plot$`Group Full Sample`
tp3 <- est4$data.plot$`Group Full Sample`

l_text <- c("FTC", "OEC")

tp_p1 <- setDT(tp0$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p1 <- rbind(tp_p1, setDT(tp1$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])

tp_p2 <- setDT(tp2$data.dots)[, c("x", "fit", "bin")][, group := l_text[1]]
tp_p2 <- rbind(tp_p2, setDT(tp3$data.dots)[, c("x", "fit", "bin")][, group := l_text[2]])


bin_cutoff <- 0

p1 <- ggplot(data = tp_p1[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + 
  labs(x = "HHI for OEC hiring by CZ x Occupation(2-digit)", y = "Log hourly wage") +
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

p2 <- ggplot(data = tp_p2[bin > bin_cutoff], aes(x = (x), y = fit, color = factor(group))) + 
  labs(x = "HHI for OEC hiring by CZ x Occupation(2-digit)", y = "Log hourly wage (residualized)") +
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
ggsave(p, filename = here("New Results", "LLM gap binscatters", paste0("llm_wage_levels_hhi_cdi", sav_suf, ".png")),
       width = 10, height = 8)



# Gap
est <- binsreg(data = dat_cdi[n > n_rest], y = gap, x = hhi_cdi, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.01, max(tp_p$x) + 0.01)]

p <- ggplot() + labs(x = "HHI for OEC hiring by CZ x Occupation(2-digit)", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
p
ggsave(p, filename = here("New Results", "LLM gap binscatters", paste0("llm_wage_gap_hhi_cdi", sav_suf, ".png")))



# Cross HHI ---------------------------------------------------------------

# By FTC HHI - levels
est1 <- binsreg(data = dat_cddi[n > n_rest & cs_clean > 0], y = m_lhw_res4_cdd, x = hhi_cdi, by = hhi_cdd_q, nbins = 20)
est2 <- binsreg(data = dat_cddi[n > n_rest & cs_clean > 0], y = m_lhw_res4_cdi, x = hhi_cdi, by = hhi_cdd_q, nbins = 20)

tp1 <- data.table()
for(i in 1:length(est1$data.plot)){
  tp1 <- rbind(tp1, est1$data.plot[[i]]$data.dots)
}

tp1[, group := factor(group, levels = levels(dat_cddi$hhi_cdd_q))]
tp1[, source := "FTC"]

tp2 <- data.table()
for(i in 1:length(est2$data.plot)){
  tp2 <- rbind(tp2, est2$data.plot[[i]]$data.dots)
}

tp2[, group := factor(group, levels = levels(dat_cddi$hhi_cdd_q))]
tp2[, source := "OEC"]

tp <- rbind(tp1, tp2)

p <- tp %>% ggplot(aes(x = (x), y = fit, color = source)) + 
  labs(x = "HHI for OEC hiring by CZ x Occupation(2-digit)", y = "Firm gaps in log hourly wage") +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom", legend.title = element_blank()) +
  facet_wrap(~group, nrow = 2, scales = "free")
p
ggsave(p, filename = here("New Results", "LLM gap binscatters", paste0("llm_wage_levels_hhi_cdi_byHHIcddq", sav_suf, ".png")),
       width = 10, height = 8)


# By OEC HHI - levels
est1 <- binsreg(data = dat_cddi[n > n_rest], y = m_lhw_res4_cdd, x = hhi_cdd, by = hhi_cdi_q, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)
est2 <- binsreg(data = dat_cddi[n > n_rest], y = m_lhw_res4_cdi, x = hhi_cdd, by = hhi_cdi_q, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

tp1 <- data.table()
for(i in 1:length(est1$data.plot)){
  tp1 <- rbind(tp1, est1$data.plot[[i]]$data.dots)
}

tp1[, group := factor(group, levels = levels(dat_cddi$hhi_cdi_q))]
tp1[, source := "FTC"]

tp2 <- data.table()
for(i in 1:length(est2$data.plot)){
  tp2 <- rbind(tp2, est2$data.plot[[i]]$data.dots)
}

tp2[, group := factor(group, levels = levels(dat_cddi$hhi_cdi_q))]
tp2[, source := "OEC"]

tp <- rbind(tp1, tp2)

p <- tp %>% ggplot(aes(x = (x), y = fit, color = source)) + 
  labs(x = "HHI for FTC hiring by CZ x Occupation(2-digit)", y = "Log hourly wage (residualized)") +
  geom_point() +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom", legend.title = element_blank()) +
  facet_wrap(~group, nrow = 2, scales = "free")
p
ggsave(p, filename = here("New Results", "LLM gap binscatters", paste0("llm_wage_levels_hhi_cdi_byHHIcdiq", sav_suf, ".png")),
       width = 10, height = 8)

# By FTC HHI
est <- binsreg(data = dat_cddi[n > n_rest], y = gap, x = hhi_cdi, by = hhi_cdd_q, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

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
tp3 <- tp3[between(x, x_min - 0.01, x_max + 0.01)]

tp[, group := factor(group, levels = levels(dat_cddi$hhi_cdd_q))]
tp2[, group := factor(group, levels = levels(dat_cddi$hhi_cdd_q))]
tp3[, group := factor(group, levels = levels(dat_cddi$hhi_cdd_q))]

p <- tp %>% ggplot() + 
  labs(x = "HHI for OEC hiring by CZ x Occupation(2-digit)", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14)) +
  facet_wrap(~group, nrow = 2, scales = "free")
p
ggsave(p, filename = here("New Results", "LLM gap binscatters", paste0("llm_wage_gap_hhi_cdi_byHHIcddq", sav_suf, ".png")))

# By OEC HHI
est <- binsreg(data = dat_cddi[n > n_rest], y = gap, x = hhi_cdd, by = hhi_cdi_q, line = c(3,3), ci = c(3,3), cb = c(3,3), nbins = 20)

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
tp3 <- tp3[between(x, x_min - 0.01, x_max + 0.01)]

tp[, group := factor(group, levels = levels(dat_cddi$hhi_cdi_q))]
tp2[, group := factor(group, levels = levels(dat_cddi$hhi_cdi_q))]
tp3[, group := factor(group, levels = levels(dat_cddi$hhi_cdi_q))]

p <- tp %>% ggplot() + 
  labs(x = "HHI for FTC hiring by CZ x Occupation(2-digit)", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14)) +
  facet_wrap(~group, nrow = 2, scales = "free")
p
ggsave(p, filename = here("New Results", "LLM gap binscatters", paste0("llm_wage_gap_hhi_cdd_byHHIcdiq", sav_suf, ".png")))


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# PCS ----------------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Load data ---------------------------------------------------------------

# Descriptive stats on data merging dataset initialization
desc_dat <- data.table()

# DADS data
dat <- read.fst(here("Data", "ss_panel_1014_collapse_pcsLLM_hiring_wage.fst"), as.data.table = TRUE)

desc_dat <- rbind(desc_dat, data.table(data = "Full data", n_obs = nrow(dat), 
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

# Restrict to firms which have a FTC-OEC gap (ie in a given year have hired both)
dat <- dat[!is.na(m_lhw_res4_cdd) & !is.na(m_lhw_res4_cdi)]

desc_dat <- rbind(desc_dat, data.table(data = "FTC-OEC gap", n_obs = nrow(dat), 
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

# HHI data
hhi <- read.fst(here("Data", "hhi_pcs_sir.fst"), as.data.table = TRUE)
hhi_cdd <- read.fst(here("Data", "hhi_pcs_sir_cdd.fst"), as.data.table = TRUE)
setnames(hhi_cdd, "hhi", "hhi_cdd")
hhi_cdi <- read.fst(here("Data", "hhi_pcs_sir_cdi.fst"), as.data.table = TRUE)
setnames(hhi_cdi, "hhi", "hhi_cdi")

dat <- merge(dat, hhi, by = c("zempt10", "pcs", "year"))
desc_dat <- rbind(desc_dat, data.table(data = "HHI", n_obs = nrow(dat), 
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

dat_cdd <- merge(dat, hhi_cdd, by = c("zempt10", "pcs", "year"))
desc_dat <- rbind(desc_dat, data.table(data = "HHI FTC", n_obs = nrow(dat), 
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

dat_cdi <- merge(dat, hhi_cdi, by = c("zempt10", "pcs", "year"))
desc_dat <- rbind(desc_dat, data.table(data = "HHI OEC", n_obs = nrow(dat), 
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

dat_cddi <- merge(dat, merge(hhi_cdi, hhi_cdd, by = c("zempt10", "pcs", "year")), by = c("zempt10", "pcs", "year"))
desc_dat <- rbind(desc_dat, data.table(data = "HHI OEC", n_obs = nrow(dat), 
                                       mean_wage = mean(dat$m_lhw, na.rm = TRUE)))

# Save desc_dat
datasummary_df(desc_dat[, mean_wage := round(mean_wage, 2)], 
               fmt = NULL,
               notes = "foo",
               output = here("New Results", "LLM gap binscatters", paste0("tab_desc_dat_hhi_pcs", sav_suf, ".tex")))

# Create variables --------------------------------------------------------

# Wage gap variable
dat[, gap := m_lhw_res4_cdd - m_lhw_res4_cdi]
dat_cdd[, gap := m_lhw_res4_cdd - m_lhw_res4_cdi]
dat_cdi[, gap := m_lhw_res4_cdd - m_lhw_res4_cdi]
dat_cddi[, gap := m_lhw_res4_cdd - m_lhw_res4_cdi]



# HHI FTC quantiles
y_quant <- quantile(dat_cddi$hhi_cdd, probs = (0:4)/4)
y_quant[1] <- -Inf
y_quant[5] <- +Inf

dat_cddi[, hhi_cdd_q := cut(hhi_cdd, breaks = y_quant)]

# HHI FTC quantiles
y_quant <- quantile(dat_cddi$hhi_cdi, probs = (0:4)/4)
y_quant[1] <- -Inf
y_quant[5] <- +Inf

dat_cddi[, hhi_cdi_q := cut(hhi_cdi, breaks = y_quant)]

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Gaps --------------------------------------------------------------------

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------



# Overall HHI ----------------------------------------------

est <- binsreg(data = dat, y = gap, x = hhi, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.01, max(tp_p$x) + 0.01)]

p <- ggplot() + labs(x = "HHI for hiring by CZ x Occupation(4-digit)", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
p
ggsave(p, filename = here("New Results", "LLM gap binscatters", paste0("llm_wage_gap_hhi_pcs", sav_suf, ".png")))

# FTC HHI ----------------------------------------------

est <- binsreg(data = dat_cdd, y = gap, x = hhi_cdd, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.01, max(tp_p$x) + 0.01)]

p <- ggplot() + labs(x = "HHI for FTC hiring by CZ x Occupation(4-digit)", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
p
ggsave(p, filename = here("New Results", "LLM gap binscatters", paste0("llm_wage_gap_hhi_cdd_pcs", sav_suf, ".png")))

# OEC HHI ----------------------------------------------

est <- binsreg(data = dat_cdi, y = gap, x = hhi_cdi, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.01, max(tp_p$x) + 0.01)]

p <- ggplot() + labs(x = "HHI for OEC hiring by CZ x Occupation(4-digit)", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
p
ggsave(p, filename = here("New Results", "LLM gap binscatters", paste0("llm_wage_gap_hhi_cdi_pcs", sav_suf, ".png")))



# Cross HHI ---------------------------------------------------------------

# By FTC HHI
est <- binsreg(data = dat_cddi, y = gap, x = hhi_cdi, by = hhi_cdd_q, line = c(3,3), ci = c(3,3), cb = c(3,3))

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
tp3 <- tp3[between(x, x_min - 0.01, x_max + 0.01)]

tp[, group := factor(group, levels = levels(dat_cddi$hhi_cdd_q))]
tp2[, group := factor(group, levels = levels(dat_cddi$hhi_cdd_q))]
tp3[, group := factor(group, levels = levels(dat_cddi$hhi_cdd_q))]

p <- tp %>% ggplot() + 
  labs(x = "HHI for OEC hiring by CZ x Occupation(4-digit)", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14)) +
  facet_wrap(~group, nrow = 2, scales = "free")
p
ggsave(p, filename = here("New Results", "LLM gap binscatters", paste0("llm_wage_gap_hhi_cdi_byHHIcddq_pcs", sav_suf, ".png")))

# By OEC HHI
est <- binsreg(data = dat_cddi, y = gap, x = hhi_cdd, by = hhi_cdi_q, line = c(3,3), ci = c(3,3), cb = c(3,3))

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
tp3 <- tp3[between(x, x_min - 0.01, x_max + 0.01)]

tp[, group := factor(group, levels = levels(dat_cddi$hhi_cdi_q))]
tp2[, group := factor(group, levels = levels(dat_cddi$hhi_cdi_q))]
tp3[, group := factor(group, levels = levels(dat_cddi$hhi_cdi_q))]

p <- tp %>% ggplot() + 
  labs(x = "HHI for FTC hiring by CZ x Occupation(4-digit)", y = "Firm gaps in log hourly wage") +
  geom_point(data = tp, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp3, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14)) +
  facet_wrap(~group, nrow = 2, scales = "free")
p
ggsave(p, filename = here("New Results", "LLM gap binscatters", paste0("llm_wage_gap_hhi_cdd_byHHIcdiq_pcs", sav_suf, ".png")))

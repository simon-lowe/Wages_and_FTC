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

# HHI - Siren level -------------------------------------------------------
# PCS
sir_dat <- dat[, .(nj_pcs = .N), by = .(siren, zempt10, pcs, year)]
sir_dat[, nj_tot_pcs := sum(nj_pcs), by = .(zempt10, pcs, year)]
sir_dat[, sj_pcs := nj_pcs/nj_tot_pcs]


hhi_pcs_sir <- sir_dat[, .(hhi = sum(sj_pcs^2)), by = .(zempt10, pcs, year)]
write.fst(hhi_pcs_sir, here("Data", "hhi_pcs_sir.fst"))

# CS
sir_dat <- dat[, .(nj_pcs = .N), by = .(siren, zempt10, cs_clean, year)]
sir_dat[, nj_tot_pcs := sum(nj_pcs), by = .(zempt10, cs_clean, year)]
sir_dat[, sj_pcs := nj_pcs/nj_tot_pcs]


hhi_cs_sir <- sir_dat[, .(hhi = sum(sj_pcs^2)), by = .(zempt10, cs_clean, year)]
write.fst(hhi_cs_sir, here("Data", "hhi_cs_sir.fst"))


# Plot --------------------------------------------------------------------

p1 <- hhi_pcs_sir %>%
  ggplot() +
  geom_histogram(aes(x = hhi)) +
  # geom_vline(aes(xintercept = mean(hhi_pcs_sir$hhi)), color = "red") +
  geom_vline(aes(xintercept = median(hhi_pcs_sir$hhi)), color = "blue", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(hhi_pcs_sir$hhi, probs = 0.25)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(hhi_pcs_sir$hhi, probs = 0.75)), color = "red", linetype = "dashed") +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14)) +
  annotate(geom = "text",
           label = c(paste0("Median = ", round(median(hhi_pcs_sir$hhi), 2)), "1st Quartile", "3rd Quartile"),
           x = c(median(hhi_pcs_sir$hhi), quantile(hhi_pcs_sir$hhi, probs = 0.25), quantile(hhi_pcs_sir$hhi, probs = 0.75)),
           y = rep(5e4, 3),
           angle = 90,
           vjust = 1,
           color = c("blue", "red", "red")) +
  xlab("HHI for hiring by CZ x Occupation(4-digit)") +
  ylab("Count")
ggsave(p1, filename = here("Results", "LLM gap binscatters", "hhi_pcs_sir_hist.png"))

p2 <- hhi_cs_sir %>%
  ggplot() +
  geom_histogram(aes(x = hhi)) +
  geom_vline(aes(xintercept = median(hhi_cs_sir$hhi)), color = "blue", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(hhi_cs_sir$hhi, probs = 0.25)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(hhi_cs_sir$hhi, probs = 0.75)), color = "red", linetype = "dashed") +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14)) +
  annotate(geom = "text",
           label = c(paste0("Median = ", round(median(hhi_cs_sir$hhi), 2)), "1st Quartile", "3rd Quartile"),
           x = c(median(hhi_cs_sir$hhi), quantile(hhi_cs_sir$hhi, probs = 0.25), quantile(hhi_cs_sir$hhi, probs = 0.75)),
           y = rep(0.9e4, 3),
           angle = 90,
           vjust = 1,
           color = c("blue", "red", "red")) +
  xlab("HHI for hiring by CZ x Occupation(2-digit)") +
  ylab("Count")
ggsave(p2, filename = here("Results", "LLM gap binscatters", "hhi_cs_sir_hist.png"))

# Computing the HHI - Siren-contract level -------------------------------------------------------

sir_dat <- dat[, .(nj_pcs = .N), by = .(siren, zempt10, pcs, cdd, year)]
sir_dat[, nj_tot_pcs := sum(nj_pcs), by = .(zempt10, pcs, cdd, year)]
sir_dat[, sj_pcs := nj_pcs/nj_tot_pcs]


hhi_pcs_sir_cdd <- sir_dat[cdd == TRUE, .(hhi = sum(sj_pcs^2)), by = .(zempt10, pcs, year)]
hhi_pcs_sir_cdi <- sir_dat[cdd == FALSE, .(hhi = sum(sj_pcs^2)), by = .(zempt10, pcs, year)]
write.fst(hhi_pcs_sir_cdd, here("Data", "hhi_pcs_sir_cdd.fst"))
write.fst(hhi_pcs_sir_cdi, here("Data", "hhi_pcs_sir_cdi.fst"))



sir_dat <- dat[, .(nj_pcs = .N), by = .(siren, zempt10, cs_clean, cdd, year)]
sir_dat[, nj_tot_pcs := sum(nj_pcs), by = .(zempt10, cs_clean, cdd, year)]
sir_dat[, sj_pcs := nj_pcs/nj_tot_pcs]


hhi_cs_sir_cdd <- sir_dat[cdd == TRUE, .(hhi = sum(sj_pcs^2)), by = .(zempt10, cs_clean, year)]
hhi_cs_sir_cdi <- sir_dat[cdd == FALSE, .(hhi = sum(sj_pcs^2)), by = .(zempt10, cs_clean, year)]
write.fst(hhi_cs_sir_cdd, here("Data", "hhi_cs_sir_cdd.fst"))
write.fst(hhi_cs_sir_cdi, here("Data", "hhi_cs_sir_cdi.fst"))


# Industry
sir_dat <- dat[, .(nj_pcs = .N), by = .(siren, zempt10, a38, cdd, year)]
sir_dat[, nj_tot_pcs := sum(nj_pcs), by = .(zempt10, a38, cdd, year)]
sir_dat[, sj_pcs := nj_pcs/nj_tot_pcs]


hhi_cs_sir_cdd <- sir_dat[cdd == TRUE, .(hhi = sum(sj_pcs^2)), by = .(zempt10, a38, year)]
hhi_cs_sir_cdi <- sir_dat[cdd == FALSE, .(hhi = sum(sj_pcs^2)), by = .(zempt10, a38, year)]
write.fst(hhi_cs_sir_cdd, here("Data", "hhi_ind_sir_cdd.fst"))
write.fst(hhi_cs_sir_cdi, here("Data", "hhi_ind_sir_cdi.fst"))


# Plot --------------------------------------------------------------------

# Histograms
p1 <- hhi_cs_sir_cdd %>%
  ggplot() +
  geom_histogram(aes(x = hhi)) +
  geom_vline(aes(xintercept = median(hhi_cs_sir_cdd$hhi)), color = "blue", linetype = "dashed") +
  annotate(geom = "text",
           x = median(hhi_cs_sir_cdd$hhi), y = 6000,
           label = paste0("Median = ", round(median(hhi_cs_sir_cdd$hhi), 2)), 
           color = "blue",
           angle = 90,
           vjust = 1) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  xlab("HHI for hiring by CZ x Occupation(2-digit)") +
  ylab("Count") +
  ggtitle("FTC")

p2 <- hhi_cs_sir_cdi %>%
  ggplot() +
  geom_histogram(aes(x = hhi)) +
  geom_vline(aes(xintercept = median(hhi_cs_sir_cdi$hhi)), color = "blue", linetype = "dashed") +
  annotate(geom = "text",
           x = median(hhi_cs_sir_cdi$hhi), y = 7700,
           label = paste0("Median = ", round(median(hhi_cs_sir_cdi$hhi), 2)), 
           color = "blue",
           angle = 90,
           vjust = 1) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  xlab("HHI for hiring by CZ x Occupation(2-digit)") +
  ylab("Count") +
  ggtitle("OEC")

p <- p1 + p2 & xlab(NULL)
pf <- wrap_elements(panel = p) + labs(tag = "HHI for hiring by CZ x Occupation(2-digit)") +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )
ggsave(pf, filename = here("Results", "LLM gap binscatters", "hhi_cs_sir_cddi_hist.png"))


# Regression plot - PCS
tmp <- merge(hhi_pcs_sir_cdd, hhi_pcs_sir_cdi, by = c("zempt10", "pcs", "year"))

tmp[, hhi.x.q := cut(hhi.x, breaks= unique(quantile(hhi.x, probs = 0:100/100)))]

tmp2 <- tmp[, .(y = mean(hhi.y), x = mean(hhi.x)), by = hhi.x.q]

reg <- lm(data = tmp2, y ~ x)

p <- ggplot() +
  geom_point(data = tmp[sample(.N, round(.N * 0.2))], aes(x = hhi.x, y = hhi.y), alpha = 0.05) +
  stat_smooth(data = tmp2, aes(x = x, y = y), method = "lm", se = FALSE, linetype = "dashed") +
  xlab("HHI for FTC") +
  ylab("HHI for OEC") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
ggsave(p, filename = here("Results", "LLM gap binscatters", "hhi_pcs_sir_cddi_reg.png"))


# Regression plot - CS
tmp <- merge(hhi_cs_sir_cdd, hhi_cs_sir_cdi, by = c("zempt10", "cs_clean", "year"))

tmp[, hhi.x.q := cut(hhi.x, breaks= unique(quantile(hhi.x, probs = 0:100/100)))]

tmp2 <- tmp[, .(y = mean(hhi.y), x = mean(hhi.x)), by = hhi.x.q]

reg <- lm(data = tmp2, y ~ x)

p <- ggplot() +
  geom_point(data = tmp[sample(.N, round(.N * 0.2))], aes(x = hhi.x, y = hhi.y), alpha = 0.05) +
  stat_smooth(data = tmp2, aes(x = x, y = y), method = "lm", se = FALSE, linetype = "dashed") +
  xlab("HHI for FTC") +
  ylab("HHI for OEC") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
ggsave(p, filename = here("Results", "LLM gap binscatters", "hhi_cs_sir_cddi_reg.png"))


# HHI - Siren level - year aggregated -------------------------------------------------------

# CS
sir_dat <- dat[between(year, 2010, 2014), .(nj_pcs = .N), by = .(siren, zempt10, cs_clean)]
sir_dat[, nj_tot_pcs := sum(nj_pcs), by = .(zempt10, cs_clean)]
sir_dat[, sj_pcs := nj_pcs/nj_tot_pcs]


hhi_cs_sir <- sir_dat[, .(hhi = sum(sj_pcs^2)), by = .(zempt10, cs_clean)]
write.fst(hhi_cs_sir, here("Data", "hhi_cs_sir_ya1014.fst"))


# Plot --------------------------------------------------------------------

p1 <- hhi_pcs_sir %>%
  ggplot() +
  geom_histogram(aes(x = hhi)) +
  # geom_vline(aes(xintercept = mean(hhi_pcs_sir$hhi)), color = "red") +
  geom_vline(aes(xintercept = median(hhi_pcs_sir$hhi)), color = "blue", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(hhi_pcs_sir$hhi, probs = 0.25)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(hhi_pcs_sir$hhi, probs = 0.75)), color = "red", linetype = "dashed") +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14)) +
  annotate(geom = "text",
           label = c(paste0("Median = ", round(median(hhi_pcs_sir$hhi), 2)), "1st Quartile", "3rd Quartile"),
           x = c(median(hhi_pcs_sir$hhi), quantile(hhi_pcs_sir$hhi, probs = 0.25), quantile(hhi_pcs_sir$hhi, probs = 0.75)),
           y = rep(5e4, 3),
           angle = 90,
           vjust = 1,
           color = c("blue", "red", "red")) +
  xlab("HHI for hiring by CZ x Occupation(4-digit)") +
  ylab("Count")
ggsave(p1, filename = here("Results", "LLM gap binscatters", "hhi_pcs_sir_hist_ya1014.png"))

p2 <- hhi_cs_sir %>%
  ggplot() +
  geom_histogram(aes(x = hhi)) +
  geom_vline(aes(xintercept = median(hhi_cs_sir$hhi)), color = "blue", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(hhi_cs_sir$hhi, probs = 0.25)), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = quantile(hhi_cs_sir$hhi, probs = 0.75)), color = "red", linetype = "dashed") +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14)) +
  annotate(geom = "text",
           label = c(paste0("Median = ", round(median(hhi_cs_sir$hhi), 2)), "1st Quartile", "3rd Quartile"),
           x = c(median(hhi_cs_sir$hhi), quantile(hhi_cs_sir$hhi, probs = 0.25), quantile(hhi_cs_sir$hhi, probs = 0.75)),
           y = rep(0.2e4, 3),
           angle = 90,
           vjust = 1,
           color = c("blue", "red", "red")) +
  xlab("HHI for hiring by CZ x Occupation(2-digit)") +
  ylab("Count")
ggsave(p2, filename = here("New Results", "LLM gap binscatters", "hhi_cs_sir_hist_ya1014.png"), width = 6, height = 7)


# Computing the HHI - Siren-contract level - year aggregated -------------------------------------------------------

sir_dat <- dat[between(year, 2010, 2014), .(nj_pcs = .N), by = .(siren, zempt10, cs_clean, cdd)]
sir_dat[, nj_tot_pcs := sum(nj_pcs), by = .(zempt10, cs_clean, cdd)]
sir_dat[, sj_pcs := nj_pcs/nj_tot_pcs]


hhi_cs_sir_cdd <- sir_dat[cdd == TRUE, .(hhi = sum(sj_pcs^2)), by = .(zempt10, cs_clean)]
hhi_cs_sir_cdi <- sir_dat[cdd == FALSE, .(hhi = sum(sj_pcs^2)), by = .(zempt10, cs_clean)]
write.fst(hhi_cs_sir_cdd, here("Data", "hhi_cs_sir_cdd_ya1014.fst"))
write.fst(hhi_cs_sir_cdi, here("Data", "hhi_cs_sir_cdi_ya1014.fst"))



# Plot --------------------------------------------------------------------

# Histograms
p1 <- hhi_cs_sir_cdd %>%
  ggplot() +
  geom_histogram(aes(x = hhi)) +
  geom_vline(aes(xintercept = median(hhi_cs_sir_cdd$hhi)), color = "blue", linetype = "dashed") +
  annotate(geom = "text",
           x = median(hhi_cs_sir_cdd$hhi), y = 1500,
           label = paste0("Median = ", round(median(hhi_cs_sir_cdd$hhi), 2)), 
           color = "blue",
           angle = 90,
           vjust = 1) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  xlab("HHI for hiring by CZ x Occupation(2-digit)") +
  ylab("Count") +
  ggtitle("FTC")

p2 <- hhi_cs_sir_cdi %>%
  ggplot() +
  geom_histogram(aes(x = hhi)) +
  geom_vline(aes(xintercept = median(hhi_cs_sir_cdi$hhi)), color = "blue", linetype = "dashed") +
  annotate(geom = "text",
           x = median(hhi_cs_sir_cdi$hhi), y = 1700,
           label = paste0("Median = ", round(median(hhi_cs_sir_cdi$hhi), 2)), 
           color = "blue",
           angle = 90,
           vjust = 1) +
  theme_classic() + 
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  xlab("HHI for hiring by CZ x Occupation(2-digit)") +
  ylab("Count") +
  ggtitle("OEC")

p <- p1 + p2 & xlab(NULL)
pf <- wrap_elements(panel = p) + labs(tag = "HHI for hiring by CZ x Occupation(2-digit)") +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )
ggsave(pf, filename = here("New Results", "LLM gap binscatters", "hhi_cs_sir_cddi_hist_ya1014.png"), width = 6, height = 6)


# Regression plot - PCS
tmp <- merge(hhi_pcs_sir_cdd, hhi_pcs_sir_cdi, by = c("zempt10", "pcs", "year"))

tmp[, hhi.x.q := cut(hhi.x, breaks= unique(quantile(hhi.x, probs = 0:100/100)))]

tmp2 <- tmp[, .(y = mean(hhi.y), x = mean(hhi.x)), by = hhi.x.q]

reg <- lm(data = tmp2, y ~ x)

p <- ggplot() +
  geom_point(data = tmp[sample(.N, round(.N * 0.2))], aes(x = hhi.x, y = hhi.y), alpha = 0.05) +
  stat_smooth(data = tmp2, aes(x = x, y = y), method = "lm", se = FALSE, linetype = "dashed") +
  xlab("HHI for FTC") +
  ylab("HHI for OEC") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
ggsave(p, filename = here("Results", "LLM gap binscatters", "hhi_pcs_sir_cddi_reg_ya1014.png"))


# Regression plot - CS
tmp <- merge(hhi_cs_sir_cdd, hhi_cs_sir_cdi, by = c("zempt10", "cs_clean"))

tmp[, hhi.x.q := cut(hhi.x, breaks= unique(quantile(hhi.x, probs = 0:100/100)))]

tmp2 <- tmp[, .(y = mean(hhi.y), x = mean(hhi.x)), by = hhi.x.q]

reg <- lm(data = tmp2, y ~ x)

p <- ggplot() +
  geom_point(data = tmp[sample(.N, round(.N * 0.2))], aes(x = hhi.x, y = hhi.y), alpha = 0.05) +
  stat_smooth(data = tmp2, aes(x = x, y = y), method = "lm", se = FALSE, linetype = "dashed") +
  xlab("HHI for FTC") +
  ylab("HHI for OEC") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14))
ggsave(p, filename = here("New Results", "LLM gap binscatters", "hhi_cs_sir_cddi_reg_ya1014.png"))


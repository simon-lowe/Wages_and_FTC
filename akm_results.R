rm(list = ls())
gc()

# Load packages -----------------------------------------------------------

list.of.packages <- c(
  "tidyverse", "data.table", "haven", "fst", "expss", "janitor", "here",
  "DescTools", "tictoc", "collapse", "Hmisc",
  "fixest", "binsreg", "did", "ggplot2", "patchwork"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(list.of.packages, library, character.only = TRUE))
rm(list.of.packages, new.packages)


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Separate estimation, no controls, siren ---------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------




# Load data ---------------------------------------------------------------

# Load separate AKMs
dat_cdd <- read.csv(here("Data", "akm_1014_ftc_noc_siren.csv"))
dat_cdi <- read.csv(here("Data", "akm_1014_oec_noc_siren.csv"))
setDT(dat_cdd)
setDT(dat_cdi)

dat_cdd2 <- dat_cdd[, .(original_j, psi_hat)][, .(psi_cdd = mean(psi_hat), n_cdd = .N), by = original_j]
dat_cdi2 <- dat_cdi[, .(original_j, psi_hat)][, .(psi_cdi = mean(psi_hat), n_cdi = .N), by = original_j]

dat <- merge(dat_cdd2, dat_cdi2, by = "original_j")
setnames(dat, "original_j", "siren")

# Load variance decomposition estimates
vd_cdd <- read.csv(here("Results", "AKM approach", "akm_1014_ftc_noc_siren_fe_summary.txt"), header = FALSE)
vd_cdi <- read.csv(here("Results", "AKM approach", "akm_1014_oec_noc_siren_fe_summary.txt"), header = FALSE)
setDT(vd_cdd)
setDT(vd_cdi)

correction_factor <- as.numeric(vd_cdi[V1 == "var(psi)_fe"]$V2)/as.numeric(vd_cdi[V1 == "var(psi)_he"]$V2)



# Binscatter --------------------------------------------------------------

dat[, w_tot := n_cdd + n_cdi]

est <- binsreg(data = dat, y = psi_cdd, x = psi_cdi, weights = w_tot, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]

p <- ggplot() + 
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  geom_abline(aes(intercept = -tp_p[which.min(abs(tp_p$fit))]$x, slope = 1), linetype = "dashed") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  xlab("OEC Firm Effects") + ylab("FTC Firm Effects")
p
ggsave(p, filename = here("Results", "AKM approach", "akm_1014_sep_noc_siren_binscatter.png"))

# Graph -------------------------------------------------------------------

# Restriction given the binscatter
dat_tp <- dat[psi_cdi > tp_p[which.min(tp_p$fit)]$x]

# Psi_cdi quantiles
dat_tp[, c_psi_cdi := cut(psi_cdi, breaks = c(-Inf, quantile(psi_cdi, probs = 0:20/20)[2:20], Inf), labels = FALSE)]
# dat[, c_psi_cdi := cut(psi_cdi, breaks = c(-Inf, quantile(psi_cdi, probs = 0:100/100)[2:100], Inf), labels = FALSE)]

# Collapsed data by quantile
tp <- dat_tp[, .(ffe_cdd = weighted.mean(psi_cdd, n_cdd + n_cdi), ffe_cdi = weighted.mean(psi_cdi, n_cdd + n_cdi)), by = c_psi_cdi]

# Recenter
tp[, ffe_cdd := ffe_cdd - ffe_cdd[c_psi_cdi == 1]]
tp[, ffe_cdi := ffe_cdi - ffe_cdi[c_psi_cdi == 1]]

reg <- lm(data = tp, ffe_cdd ~ ffe_cdi)

lim_end <- pmax(max(tp$ffe_cdd), max(tp$ffe_cdi))

p <- tp %>%
  ggplot(aes(x = ffe_cdi, y = ffe_cdd)) +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_abline(aes(intercept = 0, slope = 1), linetype = "dashed") +
  geom_abline(aes(intercept = 0, slope = reg$coefficients[[2]]), color = "red") +
  geom_abline(aes(intercept = 0, slope = reg$coefficients[[2]]*correction_factor), color = "darkgreen") +
  # annotate("text", x = min(tp$ffe_cdi), y = max(tp$ffe_cdd), 
  annotate("text", x = 0, y = lim_end, 
           label = paste0("PI slope: ", round(reg$coefficients[[2]], 3), 
                          "\nKSS slope: ", round(reg$coefficients[[2]]*correction_factor, 3)), 
           hjust = 0, vjust = 0.5, size = 5) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  xlab("OEC Firm Effects") + ylab("FTC Firm Effects") +
 xlim(0,lim_end) + ylim(0, lim_end)
p
ggsave(p, filename = here("Results", "AKM approach", "akm_1014_sep_noc_siren.png"))


# Regressions -------------------------------------------------------------

reg1 <- feols(data = dat, psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)
reg2 <- feols(data = dat_tp, psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)

etable(reg1, reg2, title = "Separate AKM regressions",
       dict = c(psi_cdd = "FTC-FFE", psi_cdi = "OEC-FFE"),
       cluster = ~siren,
       tex = TRUE, 
       notes = paste0("Correction factor is ", correction_factor),
       file = here("Results", "AKM approach", "table_reg_separate_noc_siren.tex"), style.tex = style.tex("aer"))

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Joint estimation, no controls, siren ------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------



# Load joint AKM data -----------------------------------------------------

# Load data
dat <- read.csv(here("Data", "akm_1014_joint_noc_siren.csv"))
setDT(dat)

dat <- dat[, .(original_j, psi_hat)]

# Collapse data
f_dat <- dat[, .(psi = mean(psi_hat), n = .N), by = original_j]

# Split the Firm X Contract index
f_dat[, c("siren", "contract") := tstrsplit(original_j, ".", fixed = TRUE)]

# Clean up the variables
setorder(f_dat, siren)
f_dat[, original_j := NULL]
f_dat[, contract := ifelse(contract == TRUE, "cdi", "cdd")]

# Recast the data
f_dat <- dcast(f_dat, siren ~ contract, value.var = c("psi", "n")) 

# Keep only firms with both FE
f_dat <- na.omit(f_dat)


# Load variance decomposition data ----------------------------------------

vd <- read.csv(here("Results", "AKM approach", "akm_1014_joint_noc_siren_fe_summary.txt"), header = FALSE)
setDT(vd)

correction_factor <- as.numeric(vd[V1 == "var(psi)_fe"]$V2)/as.numeric(vd[V1 == "var(psi)_he"]$V2)


# Load joint AKM data - split sample 1 -----------------------------------------------------
# Load data
dat_s1 <- read.csv(here("Data", "akm_1014_joint_s1_noc_siren.csv"))
setDT(dat_s1)

dat_s1 <- dat_s1[, .(original_j, psi_hat)]

# Collapse data
f_dat_s1 <- dat_s1[, .(psi = mean(psi_hat), n = .N), by = original_j]

# Split the Firm X Contract index
f_dat_s1[, c("siren", "contract") := tstrsplit(original_j, ".", fixed = TRUE)]

# Clean up the variables
setorder(f_dat_s1, siren)
f_dat_s1[, original_j := NULL]
f_dat_s1[, contract := ifelse(contract == TRUE, "cdi", "cdd")]

# Recast the data
f_dat_s1 <- dcast(f_dat_s1, siren ~ contract, value.var = c("psi", "n")) 

# Keep only firms with both FE
f_dat_s1 <- na.omit(f_dat_s1)
names(f_dat_s1)[-1] <- paste0(names(f_dat_s1)[-1], "_s1")

# Load joint AKM data - split sample 2 -----------------------------------------------------
# Load data
dat_s2 <- read.csv(here("Data", "akm_1014_joint_s2_noc_siren.csv"))
setDT(dat_s2)

dat_s2 <- dat_s2[, .(original_j, psi_hat)]

# Collapse data
f_dat_s2 <- dat_s2[, .(psi = mean(psi_hat), n = .N), by = original_j]

# Split the Firm X Contract index
f_dat_s2[, c("siren", "contract") := tstrsplit(original_j, ".", fixed = TRUE)]

# Clean up the variables
setorder(f_dat_s2, siren)
f_dat_s2[, original_j := NULL]
f_dat_s2[, contract := ifelse(contract == TRUE, "cdi", "cdd")]

# Recast the data
f_dat_s2 <- dcast(f_dat_s2, siren ~ contract, value.var = c("psi", "n")) 

# Keep only firms with both FE
f_dat_s2 <- na.omit(f_dat_s2)
names(f_dat_s2)[-1] <- paste0(names(f_dat_s2)[-1], "_s2")




# Merging the data --------------------------------------------------------

f_dat_merge_s1s2 <- merge(f_dat_s1, f_dat_s2, by = "siren")
f_dat_merge_all <- merge(f_dat, f_dat_merge_s1s2, by = "siren")


# Binscatter - full ---------------------------------------------------------

f_dat[, w_tot := n_cdd + n_cdi]

est <- binsreg(data = f_dat, y = psi_cdd, x = psi_cdi, weights = w_tot, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]

p <- ggplot() + 
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  geom_abline(aes(intercept = -tp_p[which.min(abs(tp_p$fit))]$x, slope = 1), linetype = "dashed") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  xlab("OEC Firm Effects") + ylab("FTC Firm Effects")
p
ggsave(p, filename = here("Results", "AKM approach", "akm_1014_joint_noc_siren_binscatter.png"))


# Binscatter - merge sample ---------------------------------------------

f_dat_merge_all[, w_tot := n_cdd + n_cdi]

est <- binsreg(data = f_dat_merge_all, y = psi_cdd_s1, x = psi_cdi_s1, weights = w_tot, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]

p <- ggplot() + 
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  geom_abline(aes(intercept = 0.1, slope = 1), linetype = "dashed") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  xlab("OEC Firm Effects") + ylab("FTC Firm Effects")
p
ggsave(p, filename = here("Results", "AKM approach", "akm_1014_joint_merge_noc_siren_binscatter.png"))


# Regressions -------------------------------------------------------------

reg1 <- feols(data = f_dat, psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)
reg2 <- feols(data = f_dat_merge_all, psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)
reg3 <- feols(data = f_dat_merge_all, psi_cdd_s1 ~ psi_cdi_s1, weights = ~n_cdi + n_cdd)
reg4 <- feols(data = f_dat_merge_all, psi_cdd_s1 ~ 1 | psi_cdi_s1 ~ psi_cdi_s2, weights = ~n_cdi + n_cdd)

etable(reg1,  reg2, reg3, reg4,
       title = "Joint AKM regressions",
       dict = c(psi_cdd = "FTC-FFE", psi_cdi = "OEC-FFE",
                psi_cdd_s1 = "FTC-FFE, 1st split", psi_cdi_s1 = "OEC-FFE, 1st split",
                psi_cdi_s2 = "OEC-FFE, 2nd split"),
       cluster = ~siren,
       tex = TRUE, file = here("Results", "AKM approach", "table_reg_joint_noc_siren.tex"), style.tex = style.tex("aer"))


reg1 <- feols(data = f_dat[psi_cdi > -0.2], psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)
reg2 <- feols(data = f_dat_merge_all[psi_cdi_s1 > -0.35], psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)
reg3 <- feols(data = f_dat_merge_all[psi_cdi_s1 > -0.35], psi_cdd_s1 ~ psi_cdi_s1, weights = ~n_cdi + n_cdd)
reg4 <- feols(data = f_dat_merge_all[psi_cdi_s1 > -0.35], psi_cdd_s1 ~ 1 | psi_cdi_s1 ~ psi_cdi_s2, weights = ~n_cdi + n_cdd)
etable(reg1,  reg2, reg3, reg4, 
       title = "Joint AKM regressions - Restricted",
       dict = c(psi_cdd = "FTC-FFE", psi_cdi = "OEC-FFE",
                psi_cdd_s1 = "FTC-FFE, 1st split", psi_cdi_s1 = "OEC-FFE, 1st split",
                psi_cdi_s2 = "OEC-FFE, 2nd split"),
       cluster = ~siren,
       tex = TRUE, file = here("Results", "AKM approach", "table_reg_joint_noc_siren_rest.tex"), style.tex = style.tex("aer"))
# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Separate estimation, CONTROLS, siren ---------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------




# Load data ---------------------------------------------------------------

# Load separate AKMs
dat_cdd <- read.csv(here("Data", "akm_1014_ftc_withc_siren.csv"))
dat_cdi <- read.csv(here("Data", "akm_1014_oec_withc_siren.csv"))
setDT(dat_cdd)
setDT(dat_cdi)

dat_cdd2 <- dat_cdd[, .(original_j, psi_hat)][, .(psi_cdd = mean(psi_hat), n_cdd = .N), by = original_j]
dat_cdi2 <- dat_cdi[, .(original_j, psi_hat)][, .(psi_cdi = mean(psi_hat), n_cdi = .N), by = original_j]

dat <- merge(dat_cdd2, dat_cdi2, by = "original_j")
setnames(dat, "original_j", "siren")

# Load variance decomposition estimates
vd_cdd <- read.csv(here("Results", "AKM approach", "akm_1014_ftc_withc_siren_fe_summary.txt"), header = FALSE, sep = ";")
vd_cdi <- read.csv(here("Results", "AKM approach", "akm_1014_oec_withc_siren_fe_summary.txt"), header = FALSE, sep = ";")
setDT(vd_cdd)
setDT(vd_cdi)

correction_factor <- as.numeric(vd_cdi[V1 == "var(psi)_fe"]$V2)/as.numeric(vd_cdi[V1 == "var(psi)_he"]$V2)



# Binscatter --------------------------------------------------------------

dat[, w_tot := n_cdd + n_cdi]

est <- binsreg(data = dat, y = psi_cdd, x = psi_cdi, weights = w_tot, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]

p <- ggplot() + 
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  geom_abline(aes(intercept = -tp_p[which.min(abs(tp_p$fit))]$x, slope = 1), linetype = "dashed") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  xlab("OEC Firm Effects") + ylab("FTC Firm Effects")
p
ggsave(p, filename = here("Results", "AKM approach", "akm_1014_sep_withc_siren_binscatter.png"))

# Graph -------------------------------------------------------------------

# Restriction given the binscatter
dat_tp <- dat[psi_cdi > tp_p[which.min(tp_p$fit)]$x]

# Psi_cdi quantiles
dat_tp[, c_psi_cdi := cut(psi_cdi, breaks = c(-Inf, quantile(psi_cdi, probs = 0:20/20)[2:20], Inf), labels = FALSE)]
# dat[, c_psi_cdi := cut(psi_cdi, breaks = c(-Inf, quantile(psi_cdi, probs = 0:100/100)[2:100], Inf), labels = FALSE)]

# Collapsed data by quantile
tp <- dat_tp[, .(ffe_cdd = weighted.mean(psi_cdd, n_cdd + n_cdi), ffe_cdi = weighted.mean(psi_cdi, n_cdd + n_cdi)), by = c_psi_cdi]

# Recenter
tp[, ffe_cdd := ffe_cdd - ffe_cdd[c_psi_cdi == 1]]
tp[, ffe_cdi := ffe_cdi - ffe_cdi[c_psi_cdi == 1]]

reg <- lm(data = tp, ffe_cdd ~ ffe_cdi)

lim_end <- pmax(max(tp$ffe_cdd), max(tp$ffe_cdi))

p <- tp %>%
  ggplot(aes(x = ffe_cdi, y = ffe_cdd)) +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_abline(aes(intercept = 0, slope = 1), linetype = "dashed") +
  geom_abline(aes(intercept = 0, slope = reg$coefficients[[2]]), color = "red") +
  geom_abline(aes(intercept = 0, slope = reg$coefficients[[2]]*correction_factor), color = "darkgreen") +
  # annotate("text", x = min(tp$ffe_cdi), y = max(tp$ffe_cdd), 
  annotate("text", x = 0, y = lim_end, 
           label = paste0("PI slope: ", round(reg$coefficients[[2]], 3), 
                          "\nKSS slope: ", round(reg$coefficients[[2]]*correction_factor, 3)), 
           hjust = 0, vjust = 0.5, size = 5) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  xlab("OEC Firm Effects") + ylab("FTC Firm Effects") +
  xlim(0,lim_end) + ylim(0, lim_end)
p
ggsave(p, filename = here("Results", "AKM approach", "akm_1014_sep_withc_siren.png"))


# Regressions -------------------------------------------------------------

reg1 <- feols(data = dat, psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)
reg2 <- feols(data = dat_tp, psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)

etable(reg1, reg2, title = "Separate AKM regressions",
       dict = c(psi_cdd = "FTC-FFE", psi_cdi = "OEC-FFE"),
       cluster = ~siren,
       tex = TRUE, 
       notes = paste0("Correction factor is ", correction_factor),
       file = here("Results", "AKM approach", "table_reg_separate_withc_siren.tex"), style.tex = style.tex("aer"))

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Joint estimation, CONTROLS, siren ------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------



# Load joint AKM data -----------------------------------------------------

# Load data
dat <- read.csv(here("Data", "akm_1014_joint_withc_siren.csv"))
setDT(dat)

dat <- dat[, .(original_j, psi_hat)]

# Collapse data
f_dat <- dat[, .(psi = mean(psi_hat), n = .N), by = original_j]

# Split the Firm X Contract index
f_dat[, c("siren", "contract") := tstrsplit(original_j, ".", fixed = TRUE)]

# Clean up the variables
setorder(f_dat, siren)
f_dat[, original_j := NULL]
f_dat[, contract := ifelse(contract == TRUE, "cdi", "cdd")]

# Recast the data
f_dat <- dcast(f_dat, siren ~ contract, value.var = c("psi", "n")) 

# Keep only firms with both FE
f_dat <- na.omit(f_dat)


# Load variance decomposition data ----------------------------------------

vd <- read.csv(here("Results", "AKM approach", "akm_1014_joint_withc_siren_fe_summary.txt"), header = FALSE)
setDT(vd)

correction_factor <- as.numeric(vd[V1 == "var(psi)_fe"]$V2)/as.numeric(vd[V1 == "var(psi)_he"]$V2)


# Load joint AKM data - split sample 1 -----------------------------------------------------
# Load data
dat_s1 <- read.csv(here("Data", "akm_1014_joint_s1_withc_siren.csv"))
setDT(dat_s1)

dat_s1 <- dat_s1[, .(original_j, psi_hat)]

# Collapse data
f_dat_s1 <- dat_s1[, .(psi = mean(psi_hat), n = .N), by = original_j]

# Split the Firm X Contract index
f_dat_s1[, c("siren", "contract") := tstrsplit(original_j, ".", fixed = TRUE)]

# Clean up the variables
setorder(f_dat_s1, siren)
f_dat_s1[, original_j := NULL]
f_dat_s1[, contract := ifelse(contract == TRUE, "cdi", "cdd")]

# Recast the data
f_dat_s1 <- dcast(f_dat_s1, siren ~ contract, value.var = c("psi", "n")) 

# Keep only firms with both FE
f_dat_s1 <- na.omit(f_dat_s1)
names(f_dat_s1)[-1] <- paste0(names(f_dat_s1)[-1], "_s1")

# Load joint AKM data - split sample 2 -----------------------------------------------------
# Load data
dat_s2 <- read.csv(here("Data", "akm_1014_joint_s2_withc_siren.csv"))
setDT(dat_s2)

dat_s2 <- dat_s2[, .(original_j, psi_hat)]

# Collapse data
f_dat_s2 <- dat_s2[, .(psi = mean(psi_hat), n = .N), by = original_j]

# Split the Firm X Contract index
f_dat_s2[, c("siren", "contract") := tstrsplit(original_j, ".", fixed = TRUE)]

# Clean up the variables
setorder(f_dat_s2, siren)
f_dat_s2[, original_j := NULL]
f_dat_s2[, contract := ifelse(contract == TRUE, "cdi", "cdd")]

# Recast the data
f_dat_s2 <- dcast(f_dat_s2, siren ~ contract, value.var = c("psi", "n")) 

# Keep only firms with both FE
f_dat_s2 <- na.omit(f_dat_s2)
names(f_dat_s2)[-1] <- paste0(names(f_dat_s2)[-1], "_s2")




# Merging the data --------------------------------------------------------

f_dat_merge_s1s2 <- merge(f_dat_s1, f_dat_s2, by = "siren")
f_dat_merge_all <- merge(f_dat, f_dat_merge_s1s2, by = "siren")


# Binscatter - full ---------------------------------------------------------

f_dat[, w_tot := n_cdd + n_cdi]

est <- binsreg(data = f_dat, y = psi_cdd, x = psi_cdi, weights = w_tot, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]

p <- ggplot() + 
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  geom_abline(aes(intercept = -tp_p[which.min(abs(tp_p$fit))]$x, slope = 1), linetype = "dashed") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  xlab("OEC Firm Effects") + ylab("FTC Firm Effects")
p
ggsave(p, filename = here("Results", "AKM approach", "akm_1014_joint_withc_siren_binscatter.png"))


# Binscatter - merge sample ---------------------------------------------

f_dat_merge_all[, w_tot := n_cdd + n_cdi]

est <- binsreg(data = f_dat_merge_all, y = psi_cdd_s1, x = psi_cdi_s1, weights = w_tot, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]

p <- ggplot() + 
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  geom_abline(aes(intercept = 0.1, slope = 1), linetype = "dashed") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  xlab("OEC Firm Effects") + ylab("FTC Firm Effects")
p
ggsave(p, filename = here("Results", "AKM approach", "akm_1014_joint_merge_withc_siren_binscatter.png"))


# Regressions -------------------------------------------------------------

reg1 <- feols(data = f_dat, psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)
reg2 <- feols(data = f_dat_merge_all, psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)
reg3 <- feols(data = f_dat_merge_all, psi_cdd_s1 ~ psi_cdi_s1, weights = ~n_cdi + n_cdd)
reg4 <- feols(data = f_dat_merge_all, psi_cdd_s1 ~ 1 | psi_cdi_s1 ~ psi_cdi_s2, weights = ~n_cdi + n_cdd)

etable(reg1,  reg2, reg3, reg4,
       title = "Joint AKM regressions",
       dict = c(psi_cdd = "FTC-FFE", psi_cdi = "OEC-FFE",
                psi_cdd_s1 = "FTC-FFE, 1st split", psi_cdi_s1 = "OEC-FFE, 1st split",
                psi_cdi_s2 = "OEC-FFE, 2nd split"),
       cluster = ~siren,
       tex = TRUE, file = here("Results", "AKM approach", "table_reg_joint_withc_siren.tex"), style.tex = style.tex("aer"))


reg1 <- feols(data = f_dat[psi_cdi > -0.2], psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)
reg2 <- feols(data = f_dat_merge_all[psi_cdi_s1 > -0.35], psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)
reg3 <- feols(data = f_dat_merge_all[psi_cdi_s1 > -0.35], psi_cdd_s1 ~ psi_cdi_s1, weights = ~n_cdi + n_cdd)
reg4 <- feols(data = f_dat_merge_all[psi_cdi_s1 > -0.35], psi_cdd_s1 ~ 1 | psi_cdi_s1 ~ psi_cdi_s2, weights = ~n_cdi + n_cdd)
etable(reg1,  reg2, reg3, reg4, 
       title = "Joint AKM regressions - Restricted",
       dict = c(psi_cdd = "FTC-FFE", psi_cdi = "OEC-FFE",
                psi_cdd_s1 = "FTC-FFE, 1st split", psi_cdi_s1 = "OEC-FFE, 1st split",
                psi_cdi_s2 = "OEC-FFE, 2nd split"),
       cluster = ~siren,
       tex = TRUE, file = here("Results", "AKM approach", "table_reg_joint_withc_siren_rest.tex"), style.tex = style.tex("aer"))

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Separate estimation, no controls, SIRET ---------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------




# Load data ---------------------------------------------------------------

# Load separate AKMs
dat_cdd <- read.csv(here("Data", "akm_1014_ftc_noc_siret.csv"))
dat_cdi <- read.csv(here("Data", "akm_1014_oec_noc_siret.csv"))
setDT(dat_cdd)
setDT(dat_cdi)

dat_cdd2 <- dat_cdd[, .(original_j, psi_hat)][, .(psi_cdd = mean(psi_hat), n_cdd = .N), by = original_j]
dat_cdi2 <- dat_cdi[, .(original_j, psi_hat)][, .(psi_cdi = mean(psi_hat), n_cdi = .N), by = original_j]

dat <- merge(dat_cdd2, dat_cdi2, by = "original_j")
setnames(dat, "original_j", "siret")

# Load variance decomposition estimates
vd_cdd <- read.csv(here("Results", "AKM approach", "akm_1014_ftc_noc_siret_fe_summary.txt"), header = FALSE)
vd_cdi <- read.csv(here("Results", "AKM approach", "akm_1014_oec_noc_siret_fe_summary.txt"), header = FALSE)
setDT(vd_cdd)
setDT(vd_cdi)

correction_factor <- as.numeric(vd_cdi[V1 == "var(psi)_fe"]$V2)/as.numeric(vd_cdi[V1 == "var(psi)_he"]$V2)



# Binscatter --------------------------------------------------------------

dat[, w_tot := n_cdd + n_cdi]

est <- binsreg(data = dat, y = psi_cdd, x = psi_cdi, weights = w_tot, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]

p <- ggplot() + 
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  geom_abline(aes(intercept = -tp_p[which.min(abs(tp_p$fit))]$x, slope = 1), linetype = "dashed") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  xlab("OEC Firm Effects") + ylab("FTC Firm Effects")
p
ggsave(p, filename = here("Results", "AKM approach", "akm_1014_sep_noc_siret_binscatter.png"))

# Graph -------------------------------------------------------------------

# Restriction given the binscatter
dat_tp <- dat[psi_cdi > tp_p[which.min(tp_p$fit)]$x]

# Psi_cdi quantiles
dat_tp[, c_psi_cdi := cut(psi_cdi, breaks = c(-Inf, quantile(psi_cdi, probs = 0:20/20)[2:20], Inf), labels = FALSE)]
# dat[, c_psi_cdi := cut(psi_cdi, breaks = c(-Inf, quantile(psi_cdi, probs = 0:100/100)[2:100], Inf), labels = FALSE)]

# Collapsed data by quantile
tp <- dat_tp[, .(ffe_cdd = weighted.mean(psi_cdd, n_cdd + n_cdi), ffe_cdi = weighted.mean(psi_cdi, n_cdd + n_cdi)), by = c_psi_cdi]

# Recenter
tp[, ffe_cdd := ffe_cdd - ffe_cdd[c_psi_cdi == 1]]
tp[, ffe_cdi := ffe_cdi - ffe_cdi[c_psi_cdi == 1]]

reg <- lm(data = tp, ffe_cdd ~ ffe_cdi)

lim_end <- pmax(max(tp$ffe_cdd), max(tp$ffe_cdi))

p <- tp %>%
  ggplot(aes(x = ffe_cdi, y = ffe_cdd)) +
  geom_point() +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_abline(aes(intercept = 0, slope = 1), linetype = "dashed") +
  geom_abline(aes(intercept = 0, slope = reg$coefficients[[2]]), color = "red") +
  geom_abline(aes(intercept = 0, slope = reg$coefficients[[2]]*correction_factor), color = "darkgreen") +
  # annotate("text", x = min(tp$ffe_cdi), y = max(tp$ffe_cdd), 
  annotate("text", x = 0, y = lim_end, 
           label = paste0("PI slope: ", round(reg$coefficients[[2]], 3), 
                          "\nKSS slope: ", round(reg$coefficients[[2]]*correction_factor, 3)), 
           hjust = 0, vjust = 0.5, size = 5) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  xlab("OEC Firm Effects") + ylab("FTC Firm Effects") +
  xlim(0,lim_end) + ylim(0, lim_end)
p
ggsave(p, filename = here("Results", "AKM approach", "akm_1014_sep_noc_siret.png"))


# Regressions -------------------------------------------------------------

reg1 <- feols(data = dat, psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)
reg2 <- feols(data = dat_tp, psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)

etable(reg1, reg2, title = "Separate AKM regressions",
       dict = c(psi_cdd = "FTC-FFE", psi_cdi = "OEC-FFE"),
       cluster = ~siret,
       tex = TRUE, 
       notes = paste0("Correction factor is ", correction_factor),
       file = here("Results", "AKM approach", "table_reg_separate_noc_siret.tex"), style.tex = style.tex("aer"))

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Joint estimation, no controls, SIRET ------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------



# Load joint AKM data -----------------------------------------------------

# Load data
dat <- read.csv(here("Data", "akm_1014_joint_noc_siret.csv"))
setDT(dat)

dat <- dat[, .(original_j, psi_hat)]

# Collapse data
f_dat <- dat[, .(psi = mean(psi_hat), n = .N), by = original_j]

# Split the Firm X Contract index
f_dat[, c("siret", "contract") := tstrsplit(original_j, ".", fixed = TRUE)]

# Clean up the variables
setorder(f_dat, siret)
f_dat[, original_j := NULL]
f_dat[, contract := ifelse(contract == TRUE, "cdi", "cdd")]

# Recast the data
f_dat <- dcast(f_dat, siret ~ contract, value.var = c("psi", "n")) 

# Keep only firms with both FE
f_dat <- na.omit(f_dat)


# Load variance decomposition data ----------------------------------------

vd <- read.csv(here("Results", "AKM approach", "akm_1014_joint_noc_siret_fe_summary.txt"), header = FALSE)
setDT(vd)

correction_factor <- as.numeric(vd[V1 == "var(psi)_fe"]$V2)/as.numeric(vd[V1 == "var(psi)_he"]$V2)


# Load joint AKM data - split sample 1 -----------------------------------------------------
# Load data
dat_s1 <- read.csv(here("Data", "akm_1014_joint_s1_noc_siret.csv"))
setDT(dat_s1)

dat_s1 <- dat_s1[, .(original_j, psi_hat)]

# Collapse data
f_dat_s1 <- dat_s1[, .(psi = mean(psi_hat), n = .N), by = original_j]

# Split the Firm X Contract index
f_dat_s1[, c("siret", "contract") := tstrsplit(original_j, ".", fixed = TRUE)]

# Clean up the variables
setorder(f_dat_s1, siret)
f_dat_s1[, original_j := NULL]
f_dat_s1[, contract := ifelse(contract == TRUE, "cdi", "cdd")]

# Recast the data
f_dat_s1 <- dcast(f_dat_s1, siret ~ contract, value.var = c("psi", "n")) 

# Keep only firms with both FE
f_dat_s1 <- na.omit(f_dat_s1)
names(f_dat_s1)[-1] <- paste0(names(f_dat_s1)[-1], "_s1")

# Load joint AKM data - split sample 2 -----------------------------------------------------
# Load data
dat_s2 <- read.csv(here("Data", "akm_1014_joint_s2_noc_siret.csv"))
setDT(dat_s2)

dat_s2 <- dat_s2[, .(original_j, psi_hat)]

# Collapse data
f_dat_s2 <- dat_s2[, .(psi = mean(psi_hat), n = .N), by = original_j]

# Split the Firm X Contract index
f_dat_s2[, c("siret", "contract") := tstrsplit(original_j, ".", fixed = TRUE)]

# Clean up the variables
setorder(f_dat_s2, siret)
f_dat_s2[, original_j := NULL]
f_dat_s2[, contract := ifelse(contract == TRUE, "cdi", "cdd")]

# Recast the data
f_dat_s2 <- dcast(f_dat_s2, siret ~ contract, value.var = c("psi", "n")) 

# Keep only firms with both FE
f_dat_s2 <- na.omit(f_dat_s2)
names(f_dat_s2)[-1] <- paste0(names(f_dat_s2)[-1], "_s2")




# Merging the data --------------------------------------------------------

f_dat_merge_s1s2 <- merge(f_dat_s1, f_dat_s2, by = "siret")
f_dat_merge_all <- merge(f_dat, f_dat_merge_s1s2, by = "siret")


# Binscatter - full ---------------------------------------------------------

f_dat[, w_tot := n_cdd + n_cdi]

est <- binsreg(data = f_dat, y = psi_cdd, x = psi_cdi, weights = w_tot, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]

p <- ggplot() + 
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  geom_abline(aes(intercept = -tp_p[which.min(abs(tp_p$fit))]$x, slope = 1), linetype = "dashed") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  xlab("OEC Firm Effects") + ylab("FTC Firm Effects")
p
ggsave(p, filename = here("Results", "AKM approach", "akm_1014_joint_noc_siret_binscatter.png"))


# Binscatter - merge sample ---------------------------------------------

f_dat_merge_all[, w_tot := n_cdd + n_cdi]

est <- binsreg(data = f_dat_merge_all, y = psi_cdd_s1, x = psi_cdi_s1, weights = w_tot, line = c(3,3), ci = c(3,3), cb = c(3,3))

tp <- est$data.plot$`Group Full Sample` 
tp_p <- setDT(tp$data.dots)[, c("x", "fit")]
tp_ci <- setDT(tp$data.ci)
tp_cb <- setDT(tp$data.cb)

tp_cb <- tp_cb[between(x, min(tp_p$x) -0.2, max(tp_p$x) + 0.2)]

p <- ggplot() + 
  geom_point(data = tp_p, aes(x = (x), y = fit)) +
  geom_ribbon(data = tp_cb, aes(x = (x), ymin = cb.l, ymax = cb.r), alpha = 0.2) +
  geom_hline(aes(yintercept = 0)) +
  geom_abline(aes(intercept = 0.1, slope = 1), linetype = "dashed") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  xlab("OEC Firm Effects") + ylab("FTC Firm Effects")
p
ggsave(p, filename = here("Results", "AKM approach", "akm_1014_joint_merge_noc_siret_binscatter.png"))


# Regressions -------------------------------------------------------------

reg1 <- feols(data = f_dat, psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)
reg2 <- feols(data = f_dat_merge_all, psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)
reg3 <- feols(data = f_dat_merge_all, psi_cdd_s1 ~ psi_cdi_s1, weights = ~n_cdi + n_cdd)
reg4 <- feols(data = f_dat_merge_all, psi_cdd_s1 ~ 1 | psi_cdi_s1 ~ psi_cdi_s2, weights = ~n_cdi + n_cdd)

etable(reg1,  reg2, reg3, reg4,
       title = "Joint AKM regressions",
       dict = c(psi_cdd = "FTC-FFE", psi_cdi = "OEC-FFE",
                psi_cdd_s1 = "FTC-FFE, 1st split", psi_cdi_s1 = "OEC-FFE, 1st split",
                psi_cdi_s2 = "OEC-FFE, 2nd split"),
       cluster = ~siret,
       tex = TRUE, file = here("Results", "AKM approach", "table_reg_joint_noc_siret.tex"), style.tex = style.tex("aer"))


reg1 <- feols(data = f_dat[psi_cdi > -0.2], psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)
reg2 <- feols(data = f_dat_merge_all[psi_cdi_s1 > -0.35], psi_cdd ~ psi_cdi, weights = ~n_cdi + n_cdd)
reg3 <- feols(data = f_dat_merge_all[psi_cdi_s1 > -0.35], psi_cdd_s1 ~ psi_cdi_s1, weights = ~n_cdi + n_cdd)
reg4 <- feols(data = f_dat_merge_all[psi_cdi_s1 > -0.35], psi_cdd_s1 ~ 1 | psi_cdi_s1 ~ psi_cdi_s2, weights = ~n_cdi + n_cdd)
etable(reg1,  reg2, reg3, reg4, 
       title = "Joint AKM regressions - Restricted",
       dict = c(psi_cdd = "FTC-FFE", psi_cdi = "OEC-FFE",
                psi_cdd_s1 = "FTC-FFE, 1st split", psi_cdi_s1 = "OEC-FFE, 1st split",
                psi_cdi_s2 = "OEC-FFE, 2nd split"),
       cluster = ~siret,
       tex = TRUE, file = here("Results", "AKM approach", "table_reg_joint_noc_siret_rest.tex"), style.tex = style.tex("aer"))
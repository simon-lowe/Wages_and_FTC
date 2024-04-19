rm(list = ls())
gc()

# Load packages -----------------------------------------------------------

list.of.packages <- c(
  "tidyverse", "data.table", "haven", "fst", "expss", "janitor", "here",
  "DescTools", "tictoc", "collapse",
  "fixest", "binsreg", "did", "ggplot2", "patchwork", "readxl"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(list.of.packages, library, character.only = TRUE))
rm(list.of.packages, new.packages)

setDTthreads(0)
setFixest_nthreads(8)


# Load data ---------------------------------------------------------------

dat <- read_fst(here("Data", "event_study_data.fst"), as.data.table = TRUE)

# Restrictions -----------------------------------------
# Restrict to individuals that switch at least once
dat <- dat[s_switch >= 1]

# Restrict to the first switch for each individual
dat <- dat[cs_switch <= 1]


# Create variables --------------------------------------------------------

# Create year of switch 
dat[, y_switch := min(year[cs_switch == 1], na.rm = TRUE), by = ident_all]

# Relative time
dat[, rel_t := year - y_switch]

dat[, min_rel_t := min(rel_t), by = ident_all]
dat[, max_rel_t := max(rel_t), by = ident_all]

# Gap between relative time -1 and 0
dat[, gap_j := datdeb_t[rel_t == 0] - datfin_t[rel_t == -1], by = ident_all]

# Indicator for within firm switch
dat[, within_firm_trans := siret[rel_t == 0] != siret[rel_t == -1], by = ident_all]

# Drop individuals which had a CDI before
dat[, cdi_before := any(rel_t < 0 & cdi == TRUE), by = ident_all]

# Drop individuals which had a CDD after
dat[, cdd_after := any(rel_t >= 0 & cdd == TRUE), by = ident_all]

# Balanced panel variable
dat[, ':=' (min_year = min(year), max_year = max(year)), by = ident_all]
dat[, non_bal := min_year != min(year) | max_year != max(year)]

# No holes in spell
dat[, dy := max(year) - min(year) + 1, by = ident_all][, n_obs := .N, by = ident_all]
dat[, cont_spell := dy == n_obs]

# No gap at switch
dat[, no_gap_switch := any(rel_t == 0) & any(rel_t == -1), by = ident_all]

# Base Event-study -------------------------------------------------------------
# Regressions
reg1 <- feols(data = dat, l_hwr_t ~ i(rel_t, ref = -1))
reg2 <- feols(data = dat, l_hwr_t ~ i(rel_t, ref = -1) | y_switch)
reg3 <- feols(data = dat, l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age)
reg4 <- feols(data = dat, l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age + cs_clean)
reg5 <- feols(data = dat, l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age + cs_clean + a_38)
reg6 <- feols(data = dat, l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)

regs <- list(reg1, reg2, reg3, reg4, reg5, reg6)
rm(reg1, reg2, reg3, reg4, reg5, reg6)

# Plot
res <- data.table()
name_regs <- c("1. None", "2. + Switch year", "3. + Min age", "4. + Occupation", "5. + Industry", "6. + Experience")

for(i in 1:length(name_regs)){
  # res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~siret)$prms, controls = name_regs[i]))
  res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~a_38^dept)$prms, controls = name_regs[i]))
}

p <- res %>%
  ggplot(aes(x = estimate_names, y = estimate, color = controls)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, position = position_dodge(0.5)) +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = -4:4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  xlab("Relative time") + ylab("Log hourly wage") + labs(color = "Controls")
p
ggsave(p, filename = here("Results", "Dynamics", "cdd_to_cdi_ES_all.png"), width = 6, height = 6)
gc()



# Continuous spell event-study -------------------------------------------------------------
# Regressions
reg1 <- feols(data = dat[cont_spell == TRUE], l_hwr_t ~ i(rel_t, ref = -1))
reg2 <- feols(data = dat[cont_spell == TRUE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch)
reg3 <- feols(data = dat[cont_spell == TRUE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age)
reg4 <- feols(data = dat[cont_spell == TRUE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age + cs_clean)
reg5 <- feols(data = dat[cont_spell == TRUE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age + cs_clean + a_38)
reg6 <- feols(data = dat[cont_spell == TRUE], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)

regs <- list(reg1, reg2, reg3, reg4, reg5, reg6)
rm(reg1, reg2, reg3, reg4, reg5, reg6)

# Plot
res <- data.table()
name_regs <- c("1. None", "2. + Switch year", "3. + Min age", "4. + Occupation", "5. + Industry", "6. + Experience")

for(i in 1:length(name_regs)){
  # res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~siret)$prms, controls = name_regs[i]))
  res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~a_38^dept)$prms, controls = name_regs[i]))
  # res <- rbind(res, data.table(iplot(regs[i])$prms, src = name_regs[i]))
}

p <- res %>%
  ggplot(aes(x = estimate_names, y = estimate, color = controls)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, position = position_dodge(0.5)) +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = -4:4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  xlab("Relative time") + ylab("Log hourly wage") + labs(color = "Controls")
p
ggsave(p, filename = here("Results", "Dynamics", "cdd_to_cdi_ES_cont_spell.png"), width = 6, height = 6)
gc()



# Continuous spell and within firm event-study -------------------------------------------------------------
# Regressions
reg1 <- feols(data = dat[cont_spell == TRUE & within_firm_trans == TRUE], l_hwr_t ~ i(rel_t, ref = -1))
reg2 <- feols(data = dat[cont_spell == TRUE & within_firm_trans == TRUE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch)
reg3 <- feols(data = dat[cont_spell == TRUE & within_firm_trans == TRUE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age)
reg4 <- feols(data = dat[cont_spell == TRUE & within_firm_trans == TRUE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age + cs_clean)
reg5 <- feols(data = dat[cont_spell == TRUE & within_firm_trans == TRUE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age + cs_clean + a_38)
reg6 <- feols(data = dat[cont_spell == TRUE & within_firm_trans == TRUE], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)

regs <- list(reg1, reg2, reg3, reg4, reg5, reg6)
rm(reg1, reg2, reg3, reg4, reg5, reg6)

# Plot
res <- data.table()
name_regs <- c("1. None", "2. + Switch year", "3. + Min age", "4. + Occupation", "5. + Industry", "6. + Experience")

for(i in 1:length(name_regs)){
  # res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~siret)$prms, controls = name_regs[i]))
  res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~a_38^dept)$prms, controls = name_regs[i]))
  # res <- rbind(res, data.table(iplot(regs[i])$prms, src = name_regs[i]))
}

p <- res %>%
  ggplot(aes(x = estimate_names, y = estimate, color = controls)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, position = position_dodge(0.5)) +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = -4:4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  xlab("Relative time") + ylab("Log hourly wage") + labs(color = "Controls")
p
ggsave(p, filename = here("Results", "Dynamics", "cdd_to_cdi_ES_cont_spell_within_firm.png"), width = 6, height = 6)
gc()



# Continuous spell and not cdi before -------------------------------------------------------------
# Regressions
reg1 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE], l_hwr_t ~ i(rel_t, ref = -1))
reg2 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch)
reg3 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age)
reg4 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age + cs_clean)
reg5 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age + cs_clean + a_38)
reg6 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)

regs <- list(reg1, reg2, reg3, reg4, reg5, reg6)
rm(reg1, reg2, reg3, reg4, reg5, reg6)

# Plot
res <- data.table()
name_regs <- c("1. None", "2. + Switch year", "3. + Min age", "4. + Occupation", "5. + Industry", "6. + Experience")

for(i in 1:length(name_regs)){
  # res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~siret)$prms, controls = name_regs[i]))
  res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~a_38^dept)$prms, controls = name_regs[i]))
  # res <- rbind(res, data.table(iplot(regs[i])$prms, src = name_regs[i]))
}

p <- res %>%
  ggplot(aes(x = estimate_names, y = estimate, color = controls)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, position = position_dodge(0.5)) +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = -4:4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  xlab("Relative time") + ylab("Log hourly wage") + labs(color = "Controls")
p
ggsave(p, filename = here("Results", "Dynamics", "cdd_to_cdi_ES_cont_spell_nocdibef.png"), width = 6, height = 6)
gc()



# Continuous spell and not cdi before 2 -------------------------------------------------------------
# Regressions
reg1 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3], l_hwr_t ~ i(rel_t, ref = -1))
reg2 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3], l_hwr_t ~ i(rel_t, ref = -1) | y_switch)
reg3 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age)
reg4 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age + cs_clean)
reg5 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age + cs_clean + a_38)
reg6 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)

regs <- list(reg1, reg2, reg3, reg4, reg5, reg6)
rm(reg1, reg2, reg3, reg4, reg5, reg6)

# Plot
res <- data.table()
name_regs <- c("1. None", "2. + Switch year", "3. + Min age", "4. + Occupation", "5. + Industry", "6. + Experience")

for(i in 1:length(name_regs)){
  # res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~siret)$prms, controls = name_regs[i]))
  res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-3:4), cluster = ~a_38^dept)$prms, controls = name_regs[i]))
  # res <- rbind(res, data.table(iplot(regs[i])$prms, src = name_regs[i]))
}

p <- res[estimate_names > -4] %>%
  ggplot(aes(x = estimate_names, y = estimate, color = controls)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, position = position_dodge(0.5)) +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = -4:4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  xlab("Relative time") + ylab("Log hourly wage") + labs(color = "Controls")
p
ggsave(p, filename = here("Results", "Dynamics", "cdd_to_cdi_ES_cont_spell_nocdibef_mrtm3.png"), width = 6, height = 6)
gc()

# Continuous spell and not cdi before 2 and within firm -------------------------------------------------------------
# Regressions
reg1 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3 & within_firm_trans == TRUE], l_hwr_t ~ i(rel_t, ref = -1))
reg2 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3 & within_firm_trans == TRUE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch)
reg3 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3 & within_firm_trans == TRUE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age)
reg4 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3 & within_firm_trans == TRUE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age + cs_clean)
reg5 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3 & within_firm_trans == TRUE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age + cs_clean + a_38)
reg6 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3 & within_firm_trans == TRUE], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)

regs <- list(reg1, reg2, reg3, reg4, reg5, reg6)
rm(reg1, reg2, reg3, reg4, reg5, reg6)

# Plot
res <- data.table()
name_regs <- c("1. None", "2. + Switch year", "3. + Min age", "4. + Occupation", "5. + Industry", "6. + Experience")

for(i in 1:length(name_regs)){
  # res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~siret)$prms, controls = name_regs[i]))
  res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-3:4), cluster = ~a_38^dept)$prms, controls = name_regs[i]))
  # res <- rbind(res, data.table(iplot(regs[i])$prms, src = name_regs[i]))
}

p <- res[estimate_names > -4] %>%
  ggplot(aes(x = estimate_names, y = estimate, color = controls)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, position = position_dodge(0.5)) +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = -4:4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  xlab("Relative time") + ylab("Log hourly wage") + labs(color = "Controls")
p
ggsave(p, filename = here("Results", "Dynamics", "cdd_to_cdi_ES_cont_spell_nocdibef_mrtm3_within.png"), width = 6, height = 6)
gc()

# Continuous spell and not cdi before 2 and not within firm -------------------------------------------------------------
# Regressions
reg1 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3 & within_firm_trans == FALSE], l_hwr_t ~ i(rel_t, ref = -1))
reg2 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3 & within_firm_trans == FALSE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch)
reg3 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3 & within_firm_trans == FALSE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age)
reg4 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3 & within_firm_trans == FALSE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age + cs_clean)
reg5 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3 & within_firm_trans == FALSE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age + cs_clean + a_38)
reg6 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3 & within_firm_trans == FALSE], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)

regs <- list(reg1, reg2, reg3, reg4, reg5, reg6)
rm(reg1, reg2, reg3, reg4, reg5, reg6)

# Plot
res <- data.table()
name_regs <- c("1. None", "2. + Switch year", "3. + Min age", "4. + Occupation", "5. + Industry", "6. + Experience")

for(i in 1:length(name_regs)){
  # res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~siret)$prms, controls = name_regs[i]))
  res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-3:4), cluster = ~a_38^dept)$prms, controls = name_regs[i]))
  # res <- rbind(res, data.table(iplot(regs[i])$prms, src = name_regs[i]))
}

p <- res[estimate_names > -4] %>%
  ggplot(aes(x = estimate_names, y = estimate, color = controls)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, position = position_dodge(0.5)) +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = -4:4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  xlab("Relative time") + ylab("Log hourly wage") + labs(color = "Controls")
p
ggsave(p, filename = here("Results", "Dynamics", "cdd_to_cdi_ES_cont_spell_nocdibef_mrtm3_notwithin.png"), width = 6, height = 6)
gc()


# No gap at switch and not cdi before -------------------------------------

# Regressions
reg1 <- feols(data = dat[no_gap_switch == TRUE & cdi_before == FALSE], l_hwr_t ~ i(rel_t, ref = -1))
reg2 <- feols(data = dat[no_gap_switch == TRUE & cdi_before == FALSE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch)
reg3 <- feols(data = dat[no_gap_switch == TRUE & cdi_before == FALSE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age)
reg4 <- feols(data = dat[no_gap_switch == TRUE & cdi_before == FALSE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age + cs_clean)
reg5 <- feols(data = dat[no_gap_switch == TRUE & cdi_before == FALSE], l_hwr_t ~ i(rel_t, ref = -1) | y_switch + min_age + cs_clean + a_38)
reg6 <- feols(data = dat[no_gap_switch == TRUE & cdi_before == FALSE], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)

regs <- list(reg1, reg2, reg3, reg4, reg5, reg6)
rm(reg1, reg2, reg3, reg4, reg5, reg6)

# Plot
res <- data.table()
name_regs <- c("1. None", "2. + Switch year", "3. + Min age", "4. + Occupation", "5. + Industry", "6. + Experience")

for(i in 1:length(name_regs)){
  # res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~siret)$prms, controls = name_regs[i]))
  res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~a_38^dept)$prms, controls = name_regs[i]))
}

p <- res %>%
  ggplot(aes(x = estimate_names, y = estimate, color = controls)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, position = position_dodge(0.5)) +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = -4:4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  xlab("Relative time") + ylab("Log hourly wage") + labs(color = "Controls")
p
ggsave(p, filename = here("Results", "Dynamics", "cdd_to_cdi_ES_nogapsw_nocdibef.png"), width = 6, height = 6)
gc()

# No gap at switch and not cdi before - different min rel_t -------------------------------------

# Regressions
reg1 <- feols(data = dat[no_gap_switch == TRUE & cdi_before == FALSE], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)
reg2 <- feols(data = dat[no_gap_switch == TRUE & cdi_before == FALSE & min_rel_t == -1], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)
reg3 <- feols(data = dat[no_gap_switch == TRUE & cdi_before == FALSE & min_rel_t == -2], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)
reg4 <- feols(data = dat[no_gap_switch == TRUE & cdi_before == FALSE & min_rel_t == -3], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)
reg5 <- feols(data = dat[no_gap_switch == TRUE & cdi_before == FALSE & min_rel_t <= -4], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)

regs <- list(reg1, reg2, reg3, reg4, reg5)
rm(reg1, reg2, reg3, reg4, reg5)

# Plot
res <- data.table()
name_regs <- c("All", "1 pre-period", "2 pre-periods", "3 pre-periods", "4+ pre-periods")

for(i in 1:length(name_regs)){
  # res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~siret)$prms, controls = name_regs[i]))
  res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~a_38^dept)$prms, controls = name_regs[i]))
}


p <- res %>%
  ggplot(aes(x = estimate_names, y = estimate, color = controls)) +
  geom_point(position = position_dodge(0.5)) +
  geom_line(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, position = position_dodge(0.5)) +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = -4:4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  xlab("Relative time") + ylab("Log hourly wage") + labs(color = "Periods in FTC before")
p
ggsave(p, filename = here("Results", "Dynamics", "cdd_to_cdi_ES_nogapsw_nocdibef_mrtdif.png"), width = 8, height = 6)
gc()

# No gap at switch and not cdi before and within firm - different min rel_t -------------------------------------

# Regressions
reg1 <- feols(data = dat[within_firm_trans == TRUE & no_gap_switch == TRUE & cdi_before == FALSE], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)
reg2 <- feols(data = dat[within_firm_trans == TRUE & no_gap_switch == TRUE & cdi_before == FALSE & min_rel_t == -1], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)
reg3 <- feols(data = dat[within_firm_trans == TRUE & no_gap_switch == TRUE & cdi_before == FALSE & min_rel_t == -2], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)
reg4 <- feols(data = dat[within_firm_trans == TRUE & no_gap_switch == TRUE & cdi_before == FALSE & min_rel_t == -3], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)
reg5 <- feols(data = dat[within_firm_trans == TRUE & no_gap_switch == TRUE & cdi_before == FALSE & min_rel_t <= -4], l_hwr_t ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38)

regs <- list(reg1, reg2, reg3, reg4, reg5)
rm(reg1, reg2, reg3, reg4, reg5)

# Plot
res <- data.table()
name_regs <- c("All", "1 pre-period", "2 pre-periods", "3 pre-periods", "4+ pre-periods")

for(i in 1:length(name_regs)){
  # res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~siret)$prms, controls = name_regs[i]))
  res <- rbind(res, data.table(iplot(regs[i], keep = as.character(-4:4), cluster = ~a_38^dept)$prms, controls = name_regs[i]))
}


p <- res %>%
  ggplot(aes(x = estimate_names, y = estimate, color = controls)) +
  geom_point(position = position_dodge(0.5)) +
  geom_line(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, position = position_dodge(0.5)) +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 12)) +
  scale_x_continuous(breaks = -4:4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  xlab("Relative time") + ylab("Log hourly wage") + labs(color = "Periods in FTC before")
p
ggsave(p, filename = here("Results", "Dynamics", "cdd_to_cdi_ES_nogapsw_nocdibef_withinfirm_mrtdif.png"), width = 8, height = 6)
gc()

# Event-study heterogeneity - By industry -----------------------------------------------

dat[, ind_switch := a_10[rel_t == 0], by = ident_all]

# All
reg1 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3],
              l_hwr_w ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38, split = ~ind_switch)

res <- data.table()

for(i in 1:length(reg1)){
  res <- rbind(res, data.table(iplot(reg1[i], cluster = ~a_38^dept)$prms, ind_switch = names(reg1)[i]))
}
res <- res[between(estimate_names, -3, 4)]

# Name of industry
foo <- read_excel("C:/Users/Public/Documents/sources/Industry homogeneization/MyConv.xls", 
                  sheet = "A10")
setDT(foo)

res <- merge(res, foo, by.x = "ind_switch", by.y = "code")

# plot
p <- res[ind_switch != "AZ"] %>%
  ggplot(aes(x = estimate_names, y = estimate, color = name_eng)) +
  geom_point(position = position_dodge(0.5)) +
  geom_line(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, position = position_dodge(0.5)) +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 10)) +
  theme(legend.position = "bottom") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  guides(color = guide_legend(nrow = 3)) +
  scale_color_discrete(name = "", labels = function(x) str_wrap(x, width = 50)) +
  xlab("Relative time") + ylab("Log hourly wage")
p
ggsave(p, filename = here("Results", "Dynamics", "cdd_to_cdi_ES_cont_spell_nocdibef_mrtm3_IndHet.png"), width = 10)

# # Event-study heterogeneity - By occupation -----------------------------------------------
# 
# # Arrival occupation
# dat[, occ_arrival := occ[rel_t == 0], by = ident_all]
# 
# # All
# reg1 <- feols(data = dat[cont_spell == TRUE & cdi_before == FALSE & min_rel_t <= -3],
#               l_hwr_w ~ i(rel_t, ref = -1) + poly(exp, 2) | y_switch + min_age + cs_clean + a_38, split = ~occ_arrival)
# 
# res <- data.table()
# 
# for(i in 1:length(reg1)){
#   # res <- rbind(res, data.table(iplot(reg1[i])$prms, occ_trans = names(reg1)[i]))
#   res <- rbind(res, data.table(iplot(reg1[i], cluster = ~a_38^dept)$prms, occ_trans = names(reg1)[i]))
#   # res <- rbind(res, data.table(iplot(regs[i])$prms, src = name_regs[i]))
# }
# 
# res <- res[between(estimate_names, -3, 4)]
# 
# # p <- res %>%
# p <- res[occ_trans %in% 2:6] %>%
#   # p <- res[occ_trans != "2.2"] %>%
#   ggplot(aes(x = estimate_names, y = estimate, color = occ_trans)) +
#   geom_point(position = position_dodge(0.5)) +
#   geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, position = position_dodge(0.5)) +
#   theme_bw() +
#   scale_color_discrete(name = "Occupation at 0", labels = c("CEOs", "Managers and intellectual professions",
#                                                             "Intermediary professions", "Employees", "Workers")) +
#   theme(legend.position = "bottom") +
#   guides(color = guide_legend(nrow = 2)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   xlab("Relative time") + ylab("Log hourly wage effect")
# p
# ggsave(p, filename = here("Results", "Dynamics", "cdd_to_cdi_ES_OccHet.png"), height = 6, width = 8)

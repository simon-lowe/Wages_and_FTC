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


# Load data ---------------------------------------------------------------

# Load separate AKMs
dat_cdd <- read.csv(here("Data", "CHK_diag_cdd.csv"))
dat_cdi <- read.csv(here("Data", "CHK_diag_cdi.csv"))
setDT(dat_cdd)
setDT(dat_cdi)

# dat_cdi <- dat_cdi[((t11 == 2010 & t42 == 2013) | (t11 == 2011 & t42 == 2014))]


# CDI ---------------------------------------------------------------------


# tp1 <- dat_cdi[g2 == 0 | g2 == 3]
tp1 <- dat_cdi[(g1 == 0 & g2 == 0) | (g1 == 3 & g2 == 3)]

tp1[, t_type := paste0(g2 +1, " to ", g3 +1)]

tp1[, g2 := g2+ 1][, g3 := g3+1]

tp1 <- tp1[, .(y1 = mean(y1), y2 = mean(y2), y3 = mean(y3), y4 = mean(y4)), by = .(t_type, g2, g3)]

tp1 <- melt(tp1, id.vars = 1:3)[, t := as.numeric(substr(variable, 2, 2))]

# setnames(tp1, c("g2", "g3"), c("Departure quantile", "Destination quantile"))

p <- tp1 %>%
  # ggplot(aes(x = t, y = value, color = t_type)) +
  ggplot(aes(x = t, y = value, color = as.factor(g3), linetype = as.factor(g2))) +
  geom_point() + 
  geom_line() +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom") + #, legend.title = element_blank()
  xlab("Relative time") + ylab("Log hourly wage") +labs(color = "Destination quartile", linetype = "Departure quartile")
p
ggsave(p, filename = here("New Results", "AKM approach", "CHK_ES_cdi.png"),
       width = 8, height = 6)

tp2 <- dat_cdi[g2 < g3]
tp2 <- tp2[, .(y_up = mean(y3 - y2)), by = .(g2, g3)]

tp3 <- dat_cdi[g2 > g3]
tp3 <- tp3[, .(y_down = mean(y3 - y2)), by = .(g2, g3)]
setnames(tp3, c("g2", "g3"), c("g3", "g2"))

tp <- merge(tp2, tp3, by = c("g2", "g3"))

p <- tp %>%
  ggplot(aes(x = y_up, y = y_down)) +
  geom_point() +
  geom_abline(intercept = 0, slope = -1, linetype = "dashed") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom") +
  xlab("Mean log hourly wage change for upward movers") +
  ylab("Mean log hourly wage change for downward movers") 
p
ggsave(p, filename = here("New Results", "AKM approach", "CHK_sym_cdi.png"))

# CDD ---------------------------------------------------------------------


tp1 <- dat_cdd[g2 == 0 | g2 == 3]

tp1[, t_type := paste0(g2 +1, " to ", g3 +1)]

tp1[, g2 := g2+ 1][, g3 := g3+1]

tp1 <- tp1[, .(y1 = mean(y1), y2 = mean(y2), y3 = mean(y3), y4 = mean(y4)), by = .(t_type, g2, g3)]

tp1 <- melt(tp1, id.vars = 1:3)[, t := as.numeric(substr(variable, 2, 2))]

p <- tp1 %>%
  ggplot(aes(x = t, y = value, color = as.factor(g3), linetype = as.factor(g2))) +
  geom_point() + 
  geom_line() +
  scale_linetype_manual(values = c("solid", "dashed")) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom") + #, legend.title = element_blank()
  xlab("Relative time") + ylab("Log hourly wage") +labs(color = "Destination quartile", linetype = "Departure quartile")
p
ggsave(p, filename = here("New Results", "AKM approach", "CHK_ES_cdd.png"),
       width = 8, height = 6)

tp2 <- dat_cdd[g2 < g3]
tp2 <- tp2[, .(y_up = mean(y3 - y2)), by = .(g2, g3)]

tp3 <- dat_cdd[g2 > g3]
tp3 <- tp3[, .(y_down = mean(y3 - y2)), by = .(g2, g3)]
setnames(tp3, c("g2", "g3"), c("g3", "g2"))

tp <- merge(tp2, tp3, by = c("g2", "g3"))

p <- tp %>%
  ggplot(aes(x = y_up, y = y_down)) +
  geom_point() +
  geom_abline(intercept = 0, slope = -1, linetype = "dashed") +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "bottom") +
  xlab("Mean log hourly wage change for upward movers") +
  ylab("Mean log hourly wage change for downward movers") 
p
ggsave(p, filename = here("New Results", "AKM approach", "CHK_sym_cdd.png"))
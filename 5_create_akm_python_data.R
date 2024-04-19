rm(list = ls())
gc()

# Load packages -----------------------------------------------------------

list.of.packages <- c(
  "tidyverse", "data.table", "haven", "fst", "expss", "janitor", "here",
  "DescTools", "tictoc", "igraph",
  "fixest", "binsreg", "ggplot2", "patchwork"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(list.of.packages, library, character.only = TRUE))
rm(list.of.packages, new.packages)


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# 2010-2014 ---------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Load data ---------------------------------------------------------------

dat <- read_fst(here("Data", "ss_dads_hiring_panel_1014.fst"), as.data.table = TRUE)
dat <- dat[hwr != Inf]

dat[, siren := substr(siret, 1, 9)]

# Prepare outcome variable ------------------------------------------------

dat[, l_hwr_agg := log(hwr_agg)]

# Trimming
trim_level <- 0.001
dat[, l_hwr_agg_w2 := ifelse(between(l_hwr_agg, quantile(l_hwr_agg, trim_level, na.rm = TRUE), quantile(l_hwr_agg, 1-trim_level, na.rm = TRUE)), 
                             l_hwr_agg, NA)]


# Create variable ---------------------------------------------------------

# Splitting variable
dat[, split := rbinom(1, 1, 0.5), by = ident_all]

# OEC variable
dat[, cdi := !cdd]

# Firm X contract identifier
dat[, sir.cdi := paste(siren, cdi, sep = ".")]
dat[, siret.cdi := paste(siret, cdi, sep = ".")]

# Joint data ----------------------------------------------------------------

dat <- dat[, .(l_hwr_agg_w2, ident_all, siren, siret, sir.cdi, siret.cdi, year, cdi, pcs, cs_clean, age_bin, exp_cs_bin, split)]
gc()

dat <- na.omit(dat)

setorder(dat, ident_all, year)

write.csv(dat, here("Data", "data_akm_python_1014_joint.csv"))

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# 2005-2009 ---------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Load data ---------------------------------------------------------------

dat <- read_fst(here("Data", "ss_dads_hiring_panel_0509.fst"), as.data.table = TRUE)
dat <- dat[hwr != Inf]

dat[, siren := substr(siret, 1, 9)]

# Prepare outcome variable ------------------------------------------------

dat[, l_hwr_agg := log(hwr_agg)]

# Trimming
trim_level <- 0.001
dat[, l_hwr_agg_w2 := ifelse(between(l_hwr_agg, quantile(l_hwr_agg, trim_level, na.rm = TRUE), quantile(l_hwr_agg, 1-trim_level, na.rm = TRUE)), 
                             l_hwr_agg, NA)]


# Create variable ---------------------------------------------------------

# Splitting variable
dat[, split := rbinom(1, 1, 0.5), by = ident_all]

# OEC variable
dat[, cdi := !cdd]

# Firm X contract identifier
dat[, sir.cdi := paste(siren, cdi, sep = ".")]
dat[, siret.cdi := paste(siret, cdi, sep = ".")]

# Joint data ----------------------------------------------------------------

dat <- dat[, .(l_hwr_agg_w2, ident_all, siren, siret, sir.cdi, siret.cdi, year, cdi, pcs, cs_clean, age_bin, exp_cs_bin, split)]
gc()

dat <- na.omit(dat)

setorder(dat, ident_all, year)

write.csv(dat, here("Data", "data_akm_python_0509_joint.csv"))

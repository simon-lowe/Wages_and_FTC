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

dat <- read.fst(here("Data", "DPAE0409.fst"), as.data.table = TRUE)

# Collapse ----------------------------------------------------------------

dat_col <- dat[, .(n_apet = .N), by = .(siret, year, ape_ins)][, n := .N, by = .(siret, year)]

setorder(dat_col, siret, year, -n_apet)

# Restrict cases with more than 1 apet per siret X year
dat_col[, m_nap := max(n_apet), by = .(siret, year)]

dat_col <- dat_col[n_apet == m_nap]

dat_col <- dat_col[, .(apet = ape_ins[1]), by = .(siret, year)]

write.fst(dat_col, here("Data", "siret_apet_DPAE.fst"))

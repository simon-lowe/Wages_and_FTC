rm(list = ls())
gc()

# Load packages -----------------------------------------------------------

list.of.packages <- c(
  "tidyverse", "data.table", "haven", "fst", "expss", "janitor", "here",
  "DescTools", "tictoc", "collapse",
  "fixest", "binsreg", "did", "ggplot2", "patchwork",
  "igraph", "readxl"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(list.of.packages, library, character.only = TRUE))
rm(list.of.packages, new.packages)

setDTthreads(0)


# Load data ---------------------------------------------------------------

dat <- read.fst(here("Data", "DPAE0409.fst"), as.data.table = TRUE)
dat <- dat[type_contrat %in% 1:2]
gc()

dat2 <- read.fst(here("Data", "DPAE1014.fst"), as.data.table = TRUE)
dat2 <- dat2[type_contrat %in% 1:2]
gc()

dat <- rbindlist(list(dat, dat2), fill = TRUE)
gc()
rm(dat2)
gc()


# Conversions -------------------------------------------------------------

setorder(dat, ident_indiv, siret, date_embauche)

dat[, n_cum_cdi := cumsum(cdi), by = .(ident_indiv, siret)]
dat[, n_cum_cdd := cumsum(cdd), by = .(ident_indiv, siret)]
dat[, n_match_cdi := sum(cdi), by = .(ident_indiv, siret)]
dat[, n_match_cdd := sum(cdd), by = .(ident_indiv, siret)]

setorder(dat, ident_indiv, siren, date_embauche)

dat[, n_cum_cdi_siren := cumsum(cdi), by = .(ident_indiv, siren)]
dat[, n_cum_cdd_siren := cumsum(cdd), by = .(ident_indiv, siren)]
dat[, n_match_cdi_siren := sum(cdi), by = .(ident_indiv, siren)]
dat[, n_match_cdd_siren := sum(cdd), by = .(ident_indiv, siren)]

# Collapse ----------------------------------------------------------------

# Siret
dat_col <- dat[between(age, 18, 66), .(n_h = .N, n_cdd_h = sum(cdd), n_cdi_h = sum(cdi),
                                                             n_cdd_short_h = sum(cdd_short, na.rm = TRUE), n_cdd_long_h = sum(cdd_long, na.rm = TRUE),
                                                             n_conv_cdi = sum(cdi == TRUE & n_cum_cdd > 0),
                                                             n_conv_cdd = sum(cdd == TRUE & n_cum_cdi == 0 & n_match_cdi > 0),
                                                             m_cdd_dur = as.numeric(mean(cdd_length, na.rm = TRUE))), by = .(siret, year)]

write.fst(dat_col, here("Data", "dpae_col_hiring_drop0.fst"))

dat_col <- dat[between(age, 18, 66) & drop_sample == FALSE, .(n_h = .N, n_cdd_h = sum(cdd), n_cdi_h = sum(cdi),
                                                              n_cdd_short_h = sum(cdd_short, na.rm = TRUE), n_cdd_long_h = sum(cdd_long, na.rm = TRUE),
                                                              n_conv_cdi = sum(cdi == TRUE & n_cum_cdd > 0),
                                                              n_conv_cdd = sum(cdd == TRUE & n_cum_cdi == 0 & n_match_cdi > 0),
                                                              m_cdd_dur = as.numeric(mean(cdd_length, na.rm = TRUE))), by = .(siret, year)]

write.fst(dat_col, here("Data", "dpae_col_hiring_drop1.fst"))

dat_col <- dat[between(age, 18, 66) & drop_sample == FALSE & drop_sample_id == FALSE, 
               .(n_h = .N, n_cdd_h = sum(cdd), n_cdi_h = sum(cdi),
                  n_cdd_short_h = sum(cdd_short, na.rm = TRUE), n_cdd_long_h = sum(cdd_long, na.rm = TRUE),
                  n_conv_cdi = sum(cdi == TRUE & n_cum_cdd > 0),
                  n_conv_cdd = sum(cdd == TRUE & n_cum_cdi == 0 & n_match_cdi > 0),
                  m_cdd_dur = as.numeric(mean(cdd_length, na.rm = TRUE))), by = .(siret, year)]

write.fst(dat_col, here("Data", "dpae_col_hiring_drop2.fst"))


# Siren
dat_col <- dat[between(age, 18, 66), .(n_h = .N, n_cdd_h = sum(cdd), n_cdi_h = sum(cdi),
                                       n_cdd_short_h = sum(cdd_short, na.rm = TRUE), n_cdd_long_h = sum(cdd_long, na.rm = TRUE),
                                       n_conv_cdi = sum(cdi == TRUE & n_cum_cdd > 0),
                                       n_conv_cdd = sum(cdd == TRUE & n_cum_cdi == 0 & n_match_cdi > 0),
                                       m_cdd_dur = as.numeric(mean(cdd_length, na.rm = TRUE))), by = .(siren, year)]

write.fst(dat_col, here("Data", "dpae_col_hiring_siren_drop0.fst"))

dat_col <- dat[between(age, 18, 66) & drop_sample == FALSE, .(n_h = .N, n_cdd_h = sum(cdd), n_cdi_h = sum(cdi),
                                                              n_cdd_short_h = sum(cdd_short, na.rm = TRUE), n_cdd_long_h = sum(cdd_long, na.rm = TRUE),
                                                              n_conv_cdi = sum(cdi == TRUE & n_cum_cdd > 0),
                                                              n_conv_cdd = sum(cdd == TRUE & n_cum_cdi == 0 & n_match_cdi > 0),
                                                              m_cdd_dur = as.numeric(mean(cdd_length, na.rm = TRUE))), by = .(siren, year)]

write.fst(dat_col, here("Data", "dpae_col_hiring_siren_drop1.fst"))

dat_col <- dat[between(age, 18, 66) & drop_sample == FALSE & drop_sample_id == FALSE, 
               .(n_h = .N, n_cdd_h = sum(cdd), n_cdi_h = sum(cdi),
                 n_cdd_short_h = sum(cdd_short, na.rm = TRUE), n_cdd_long_h = sum(cdd_long, na.rm = TRUE),
                 n_conv_cdi = sum(cdi == TRUE & n_cum_cdd > 0),
                 n_conv_cdd = sum(cdd == TRUE & n_cum_cdi == 0 & n_match_cdi > 0),
                 m_cdd_dur = as.numeric(mean(cdd_length, na.rm = TRUE))), by = .(siren, year)]

write.fst(dat_col, here("Data", "dpae_col_hiring_siren_drop2.fst"))

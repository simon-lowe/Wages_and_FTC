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

# Load 2005-2009 data
dat <- read_fst(here("Data", "postes_panel_0509_CDID.fst"), as.data.table = TRUE)
dat[, ident_all := as.numeric(ident_all)]

# Identify individuals with CDD
dat[, has_cdd_ever := any(cdd == TRUE), by = ident_all]

id_0509 <- unique(dat[has_cdd_ever == TRUE]$ident_all)
rm(dat)
gc()

# Load 2010-2014 data
dat <- read_fst(here("Data", "postes_panel_1014_CDID.fst"), as.data.table = TRUE)
dat[, ident_all := as.numeric(ident_all)]

# Identify individuals with CDD
dat[, has_cdd_ever := any(cdd == TRUE), by = ident_all]

id_1014 <- unique(dat[has_cdd_ever == TRUE]$ident_all)

id_full <- unique(c(id_0509, id_1014))

# Restrict to individuals who have at least one CDD
dat <- dat[ident_all %in% id_full]
gc()

# Load 2005-2009 data
dat2 <- read_fst(here("Data", "postes_panel_0509_CDID.fst"), as.data.table = TRUE)
dat2[, ident_all := as.numeric(ident_all)]

dat2 <- dat2[ident_all %in% id_full]
gc()

dat <- rbind(dat, dat2, fill = TRUE)

rm(dat2)
gc()

# Clean the industry variable
dat[is.na(a_88), a_88 := a88]
dat[is.na(a_38), a_38 := a38]

# Delete unwanted variables
dat[, c("a6", "a17", "a38", "a88", "a2", "a_64", "a_21", "a10", "has_cdd_ever") := NULL]

# Part-time
dat[, pt := 4]
dat[cpfd == "C", pt := 1]
dat[cpfd == "P", pt := 2]
dat[cpfd == "F", pt := 3]

# Save data
cols <- c("year", "comt", "sexe", "datdeb", "datfin", "dept", "duree", "eff_0101_et", "eff_3112_et", "eff_moy_et", "filt",
          "nbheur", "pcs", "regt", "s_brut", "contrat_travail", "ident_all", "est_entry", "siret", "domempl_hom", "zempt10",
          "sbr", "hwr", "l_sbr", "l_sbr_w", "l_hwr", "l_hwr_w", "cs_clean", "cdi", "cdd", "pt", "a_88", "a_38", "age")

write_fst(dat[, ..cols], here("Data", "postes_panel_0514_hasCDD.fst"))

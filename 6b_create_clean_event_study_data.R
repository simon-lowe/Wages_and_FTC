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

dat <- read_fst(here("Data", "postes_panel_0514_hasCDD.fst"), as.data.table = TRUE)

# Data preparation --------------------------------------------------------
# Drop observations with 0 hours and Restrict to private sector
dat <- dat[l_hwr != Inf & domempl_hom %in% c(8,9)]

# Restrict age 
dat <- dat[between(age, 18, 66)]

# Make ID numeric
dat[, ident_all := as.numeric(ident_all)]

# Create an estimated experience variable
dat[, exp := year - est_entry]

# Minimal age of appearance
dat[, min_age := min(age), by = ident_all]

# Individual has at least a cdd
dat[, has_cdd_ever := any(cdd == TRUE), by = ident_all]
dat <- dat[has_cdd_ever == TRUE]

# Creating a continuous time variable
dat[, datdeb_t := (year- 2005)*360 + datdeb]
dat[, datfin_t := (year- 2005)*360 + datfin]

# Add a_10 industry
ind_conv <- read_excel("C:/Users/Public/Documents/sources/Industry homogeneization/Conversion_Tables.xlsx", 
                       sheet = "Aggregation")
setDT(ind_conv)
ind_conv <- clean_names(ind_conv)
ind_conv <- ind_conv[, 5:9]
ind_conv <- ind_conv[, .(a_10 = a_10[1]), by = a_88]

dat <- merge(dat, ind_conv, by = "a_88")

# 1-digit Occupation
dat[, occ := floor(cs_clean/10)]

# Trimmed wage variable
trim_level <- 0.001
dat[, l_hwr_t := ifelse(between(l_hwr, quantile(l_hwr, trim_level, na.rm = TRUE), quantile(l_hwr, 1-trim_level, na.rm = TRUE)), 
                        l_hwr, NA)]

# Set order
setorder(dat, ident_all, year, datdeb, datfin, siret)

# Dominant job restriction ------------------------------------------------

# Keep only 1 observation a year
dat[, max_lhwr := max(l_hwr), by = .(ident_all, year)]
dat[, max_h := max(nbheur), by = .(ident_all, year)]
dat[, max_dur := max(duree), by = .(ident_all, year)]
# dat[, max_sb := max(s_brut), by = .(ident_all, year)]

# dat <- dat[max_sb == s_brut]
dat <- dat[max_lhwr == l_hwr]
dat <- dat[max_dur == duree]
dat <- dat[max_h == nbheur]
dat <- dat[, head(.SD, 1), by = .(ident_all, year)]


# Re-restrict -------------------------------------------------------------

# Individual has at least a cdd
dat[, has_cdd_ever := any(cdd == TRUE), by = ident_all]
dat <- dat[has_cdd_ever == TRUE]

# Compute switching variable ---------------------------------

# Set order
setorder(dat, ident_all, year, datdeb, datfin, siret)

# Switching
dat[, ':=' (switch = (cdd == FALSE) & (shift(cdd) == TRUE)), by = ident_all]
dat[is.na(switch), switch := FALSE]

# Total number of switches
dat[, s_switch := sum(switch), by = ident_all]

# Cumulative index of switches
setorder(dat, ident_all, year, datdeb, datfin, siret)
dat[, cs_switch := cumsum(switch), by = ident_all]

# Save data ---------------------------------------------------------------

write.fst(dat, here("Data", "event_study_data.fst"))

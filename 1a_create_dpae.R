rm(list = ls())
gc()

# Load packages -----------------------------------------------------------

list.of.packages <- c(
  "tidyverse", "data.table", "haven", "fst", "expss", "janitor", "here",
  "DescTools", "tictoc", "collapse",
  "fixest", "binsreg", "did", "ggplot2", "patchwork",
  "igraph"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(list.of.packages, library, character.only = TRUE))
rm(list.of.packages, new.packages)

setDTthreads(0)


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# 2004 to 2009 -------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------



# Load data ---------------------------------------------------------------

dat <- read.fst("C:/Users/Public/Documents/sources/DPAE/DPAE0409.fst", as.data.table = TRUE)

# Cleaning data -----------------------------------------------------------

dat <- clean_names(dat)
dat[, c("ape_dec", "cat_jur", "sequoia", "date_saisie", "date_declaration", "ident_decla") := NULL]

for(j in seq_len(ncol(dat))){
  set(dat, which(dat[[j]] == ""), j, NA)
}
dat[, lapply(.SD, \(x) sum(is.na(x)))]

# Drop NAs in firm and industry identifier
dat <- dat[!is.na(ape_ins)][ape_ins != "0000Z"]
dat <- dat[siret != "00000000000000"]


# Drop duplicates
dat <- unique(dat)


# Create date variable
dat[, date_embauche := as.Date(date_embauche, "%d/%m/%Y")]
dat[, date_fin_cdd := as.Date(date_fin_cdd, "%d/%m/%Y")]

setorder(dat, ident_indiv, date_embauche)


dat <- dat[is.na(date_fin_cdd) | (date_fin_cdd >= date_embauche)]
gc()


# When has a end of CDD date, force it into CDD
dat[!is.na(date_fin_cdd), type_contrat := 1]


# Create variables --------------------------------------------------------

dat[, year := year(date_embauche)]

# Contract variables
dat[, cdi := type_contrat == 2]
dat[, cdd := type_contrat == 1 | type_contrat == 5]

dat[, ever_cdi := any(cdi == TRUE), by = ident_indiv]
dat[, ever_cdd := any(cdd == TRUE), by = ident_indiv]

# CDD length
dat[, cdd_length := date_fin_cdd - date_embauche + 1]

# Deal with clearly wrong end dates
dat[cdd_length > 3*365, date_fin_cdd := NA]

# CDD length
dat[, cdd_length := date_fin_cdd - date_embauche + 1]


dat[, cdd_long := cdd_length >= 30]
dat[, cdd_short := cdd_length < 30]


# Part of data with CDD coding issues
dat[, drop_sample := cdd == TRUE & is.na(date_fin_cdd)]

# Part of data with ident_indiv coding issues
dat[, foo1 := any(sexe == "F") & any(sexe == "M"), by = ident_indiv]
dat[, foo2 := max(age, na.rm = TRUE) - min(age, na.rm = TRUE), by = .(ident_indiv, year)]
dat[, foo3 := any(foo2 > 1), by = ident_indiv]

dat[, drop_sample_id := foo1 == TRUE | foo3 == TRUE]

dat[, c("foo1", "foo2", "foo3") := NULL]

# Save data ---------------------------------------------------------------

write.fst(dat, here("Data", "DPAE0409.fst"))

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# 2010 to 2014 -------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------



# Load data ---------------------------------------------------------------

dat <- read.fst("C:/Users/Public/Documents/sources/DPAE/DPAE1014.fst", as.data.table = TRUE)

# Cleaning data -----------------------------------------------------------

dat <- clean_names(dat)
dat[, c("ape_dec", "cat_jur", "sequoia", "date_saisie", "date_declaration", "ident_decla") := NULL]

for(j in seq_len(ncol(dat))){
  set(dat, which(dat[[j]] == ""), j, NA)
}
dat[, lapply(.SD, \(x) sum(is.na(x)))]

# Drop NAs in firm and industry identifier
dat <- dat[!is.na(ape_ins)][ape_ins != "0000Z"]
dat <- dat[siret != "00000000000000"]


# Drop duplicates
dat <- unique(dat)


# Create date variable
dat[, date_embauche := as.Date(date_embauche, "%d/%m/%Y")]
dat[, date_fin_cdd := as.Date(date_fin_cdd, "%d/%m/%Y")]

setorder(dat, ident_indiv, date_embauche)


dat <- dat[is.na(date_fin_cdd) | (date_fin_cdd >= date_embauche)]
gc()


# When has a end of CDD date, force it into CDD
dat[!is.na(date_fin_cdd), type_contrat := 1]


# Create variables --------------------------------------------------------

dat[, year := year(date_embauche)]

# Contract variables
dat[, cdi := type_contrat == 2]
dat[, cdd := type_contrat == 1 | type_contrat == 5]

dat[, ever_cdi := any(cdi == TRUE), by = ident_indiv]
dat[, ever_cdd := any(cdd == TRUE), by = ident_indiv]

# CDD length
dat[, cdd_length := date_fin_cdd - date_embauche + 1]

# Deal with clearly wrong end dates
dat[cdd_length > 3*365, date_fin_cdd := NA]

# CDD length
dat[, cdd_length := date_fin_cdd - date_embauche + 1]


dat[, cdd_long := cdd_length >= 30]
dat[, cdd_short := cdd_length < 30]



# Part of data with CDD coding issues
dat[, drop_sample := cdd == TRUE & is.na(date_fin_cdd)]

# Part of data with ident_indiv coding issues
dat[, foo1 := any(sexe == "F") & any(sexe == "M"), by = ident_indiv]
dat[, foo2 := max(age, na.rm = TRUE) - min(age, na.rm = TRUE), by = .(ident_indiv, year)]
dat[, foo3 := any(foo2 > 1), by = ident_indiv]

dat[, drop_sample_id := foo1 == TRUE | foo3 == TRUE]

dat[, c("foo1", "foo2", "foo3") := NULL]

# Save data ---------------------------------------------------------------

write.fst(dat, here("Data", "DPAE1014.fst"))
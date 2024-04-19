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
setFixest_nthreads(8)


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# 2010-2014 ---------------------------------------------------------------


# -------------------------------------------------------------------------





# -------------------------------------------------------------------------

# Load data ---------------------------------------------------------------

mw <- read.fst("C:/Users/Public/Documents/sources/Minimum wage/MW.fst")

dat <- read_fst(here("Data", "postes_panel_1014_CDID.fst"), as.data.table = TRUE,
                columns = c("l_hwr_w", "ident_all", "siret", "year", "age", "cdd", "sexe", 
                            "a38", "pcs", "cs_clean", "nbheur", "eff_3112_et", "dept", "zempt10",
                            "datdeb", "datfin", "duree", "domempl_hom", "est_entry",
                            # "apet", "catjur",
                            "sbr", "hwr"))

# Domaine emploi
dat <- dat[l_hwr_w != Inf & domempl_hom %in% c(8,9)]

# Make ID numeric
dat[, ident_all := as.numeric(ident_all)]

# CDD spells
dat[, cont_datdeb := (year - 2010)*360 + datdeb]
dat[, cont_datfin := (year - 2010)*360 + datfin]

setorder(dat, ident_all, siret, cont_datdeb)

dat[cdd == TRUE, spell_ind := 1]
dat[cdd == TRUE, spell_ind := ifelse(cont_datdeb - shift(cont_datfin) < 5 | is.na(shift(cont_datfin)), 
                                     shift(spell_ind, fill = 1), 
                                     shift(spell_ind) + 1), by = .(ident_all, siret)]

# Spell aggregated hourly wage variable
dat[, hwr_agg := sum(sbr, na.rm = TRUE)/sum(nbheur, na.rm = TRUE), by = .(ident_all, siret, spell_ind)]
dat[cdd == FALSE, hwr_agg := hwr]

# Spell aggregated length variable
dat[, duree_agg := sum(duree, na.rm = TRUE), by = .(ident_all, siret, spell_ind)]
dat[cdd == FALSE, duree_agg := duree]

# Create an estimated experience variable
dat[, exp := year - est_entry]

setorder(dat, ident_all, siret, cont_datdeb)
dat[, obs_id := seq_len(.N), by = .(ident_all, siret)]

dat <- dat[exp == 0 & obs_id == 1]

# Age restriction
dat <- dat[between(age, 18, 66)]

# Add cs_entry information
cs_ent_dat <- read.fst(here("Data", "cs_entry_data.fst"), as.data.table = TRUE)
cs_ent_dat[, ident_all := as.numeric(ident_all)]

dat <- merge(dat, cs_ent_dat, by = c("ident_all", "cs_clean"), all.x = TRUE, all.y = FALSE)

dat[, exp_cs := year - cs_entry]

rm(cs_ent_dat)
gc()

# Deal with sub-MW observations
dat <- merge(dat, mw, by = "year", all.x = TRUE, all.y = FALSE)
gc()

dat[, hwr_agg_mw := hwr_agg/mwh]


# Create variables --------------------------------------------------------

# Age bin
dat[, age_bin := cut(age, breaks = c(seq(17, 66, by = 5), Inf))]

# Experience in cs bins
dat[, exp_cs_bin := cut(exp_cs, breaks = c(0:5, Inf), right = FALSE)]


# Save data ---------------------------------------------------------------

dat[, c("datdeb", "datfin", "duree", "domempl_hom", "est_entry", "sbr",
        "cont_datdeb", "cont_datfin", "spell_ind", "cs_entry", "mwh", "mwm") := NULL]

write.fst(dat, here("Data", "ss_dads_hiring_panel_1014.fst"))
write.fst(dat[hwr_agg_mw > 0.8], here("Data", "ss_dads_hiring_panel_1014_restMW.fst"))


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# 2005-2009 ---------------------------------------------------------------


# -------------------------------------------------------------------------





# -------------------------------------------------------------------------

# Load data ---------------------------------------------------------------

mw <- read.fst("C:/Users/Public/Documents/sources/Minimum wage/MW.fst")
dat <- read_fst(here("Data", "postes_panel_0509_CDID.fst"), as.data.table = TRUE,
                columns = c("l_hwr_w", "ident_all", "siret", "year", "age", "cdd", "sexe", 
                            "a_38", "pcs", "cs_clean", "nbheur", "eff_3112_et", "dept", "zempt10",
                            "datdeb", "datfin", "duree", "domempl_hom", "est_entry",
                            "sbr", "hwr"))

# Domaine emploi
dat <- dat[l_hwr_w != Inf & domempl_hom %in% c(8,9)]

setnames(dat, "a_38", "a38")

# Make ID numeric
dat[, ident_all := as.numeric(ident_all)]

# CDD spells
dat[, cont_datdeb := (year - 2005)*360 + datdeb]
dat[, cont_datfin := (year - 2005)*360 + datfin]

setorder(dat, ident_all, siret, cont_datdeb)

dat[cdd == TRUE, spell_ind := 1]
dat[cdd == TRUE, spell_ind := ifelse(cont_datdeb - shift(cont_datfin) < 5 | is.na(shift(cont_datfin)), shift(spell_ind, fill = 1), shift(spell_ind) + 1), by = .(ident_all, siret)]

# Spell aggregated hourly wage variable
dat[, hwr_agg := sum(sbr, na.rm = TRUE)/sum(nbheur, na.rm = TRUE), by = .(ident_all, siret, spell_ind)]
dat[cdd == FALSE, hwr_agg := hwr]

# Spell aggregated length variable
dat[, duree_agg := sum(duree, na.rm = TRUE), by = .(ident_all, siret, spell_ind)]
dat[cdd == FALSE, duree_agg := duree]

# Create an estimated experience variable
dat[, exp := year - est_entry]

setorder(dat, ident_all, siret, cont_datdeb)
dat[, obs_id := seq_len(.N), by = .(ident_all, siret)]

dat <- dat[exp == 0 & obs_id == 1]

# Age restriction
dat <- dat[between(age, 18, 66)]

# Add cs_entry information
cs_ent_dat <- read.fst(here("Data", "cs_entry_data.fst"), as.data.table = TRUE)
cs_ent_dat[, ident_all := as.numeric(ident_all)]

dat <- merge(dat, cs_ent_dat, by = c("ident_all", "cs_clean"), all.x = TRUE, all.y = FALSE)

dat[, exp_cs := year - cs_entry]

rm(cs_ent_dat)
gc()

# Deal with sub-MW observations
dat <- merge(dat, mw, by = "year", all.x = TRUE, all.y = FALSE)
gc()

dat[, hwr_agg_mw := hwr_agg/mwh]



# Create variables --------------------------------------------------------

# Age bin
dat[, age_bin := cut(age, breaks = c(seq(17, 66, by = 5), Inf))]

# Experience in cs bins
dat[, exp_cs_bin := cut(exp_cs, breaks = c(0:5, Inf), right = FALSE)]


# Save data ---------------------------------------------------------------

dat[, c("datdeb", "datfin", "duree", "domempl_hom", "est_entry", "sbr",
        "cont_datdeb", "cont_datfin", "spell_ind", "cs_entry", "mwh", "mwm") := NULL]

write.fst(dat, here("Data", "ss_dads_hiring_panel_0509.fst"))
write.fst(dat[hwr_agg_mw > 0.8], here("Data", "ss_dads_hiring_panel_0509_restMW.fst"))

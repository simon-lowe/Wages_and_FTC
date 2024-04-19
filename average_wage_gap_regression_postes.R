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


# 2010 - 2014 -------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# Load data ---------------------------------------------------------------

dat <- read_fst(here("Data", "postes_hiring_panel_1014.fst"), as.data.table = TRUE)

# Prepare outcome variable ------------------------------------------------

dat[, l_hwr := log(hwr)]
# dat[, l_hwr_agg := log(hwr_agg)]

# Trimming
trim_level <- 0.001
dat[, l_hwr_w2 := ifelse(between(l_hwr, quantile(l_hwr, trim_level, na.rm = TRUE), quantile(l_hwr, 1-trim_level, na.rm = TRUE)), 
                         l_hwr, NA)]
# dat[, l_hwr_agg_w2 := ifelse(between(l_hwr_agg, quantile(l_hwr_agg, trim_level, na.rm = TRUE), quantile(l_hwr_agg, 1-trim_level, na.rm = TRUE)), 
#                              l_hwr_agg, NA)]

# Regressions -------------------------------------------------------------
gc()



reg <- feols(data = dat, l_hwr_w2 ~ cdd | year + csw0(sexe, age_bin, a38, pcs, siret), lean = TRUE, cluster = ~siret)

etable(reg, title = "FTC-OEC wage gap regression",
       dict = c(cddTRUE = "FTC", l_hwr_w = "Log hourly wage", year = "Year",
                sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.",a38 = "Industry", pcs = "Occupation", 
                siret = "Establishment", ident_all = "Individual"),
       # cluster = ~siret,
       tex = TRUE, file = here("Results", "Average wage gap", "table_cdd_reg_hiringwage_postes.tex"), style.tex = style.tex("aer"))

rm(reg)
gc()

# reg <- feols(data = dat, l_hwr_agg_w2 ~ cdd | year + csw0(sexe, age_bin, a38, cs_clean, exp_cs_bin, siret, ident_all), lean = TRUE, cluster = ~siret)
# 
# etable(reg, title = "FTC-OEC wage gap regression",
#        dict = c(cddTRUE = "FTC", l_hwr_w = "Log hourly wage", year = "Year",
#                 sexe = "Sex", age_bin = "Age", exp_cs_bin = "Exp. in occ.",a38 = "Industry", cs_clean = "Occupation (2 digit)", 
#                 siret = "Establishment", ident_all = "Individual"),
#        # cluster = ~siret,
#        tex = TRUE, file = here("Results", "Average wage gap", "table_cdd_reg_hiringwage_occ2.tex"), style.tex = style.tex("aer"))
# 
# rm(reg)
# gc()


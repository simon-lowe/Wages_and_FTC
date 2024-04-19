rm(list = ls())
gc()

# Load packages -----------------------------------------------------------

list.of.packages <- c(
  "tidyverse", "data.table", "haven", "fst", "expss", "janitor", "here",
  # "igraph", "fixest", "lfe", "binsreg", "ivreg", "modelsummary",
  "DescTools", "tictoc", "collapse"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(list.of.packages, library, character.only = TRUE))
rm(list.of.packages, new.packages)

setDTthreads(0)


# FICUS -------------------------------------------------------------------

files <- list.files("C:/Users/Public/Documents/sources/FARE/", full.names = TRUE)

res <- data.table()

for(i in 2005:2018){
  
  subfile <- str_subset(files, paste0("$*_", i))
  
  dat <- setDT(fread(subfile))
  dat <- clean_names(dat)
  new_names <- c("siren", "ca_net_tot", "ebe", "dividend", "vaht", "vacf", "ch_expl_sal", "ch_expl_soc", "effsalm")
  
  if(i < 2008){
    old_names <- c("siren", "catotal", "ebe", "dividen", "vaht", "vabcf", "saltrai", "charsoc", "effsalm")
    data.table::setnames(dat, old_names, new_names)
  }
  if(i == 2008){
    old_names <- c("siren_08", "catotal_08", "ebe_08", "dividen_08", "vaht_08", "vabcf_08", "saltrai_08", "charsoc_08", "eff_etp_08")
    data.table::setnames(dat, old_names, new_names)
  }
  if(i > 2008){
    old_names <- c("siren", "redi_r310", "redi_r005", "b604", "redi_r003", "r004", "redi_r216", "redi_r217", "redi_e200")
    data.table::setnames(dat, old_names, new_names)
  }
  
  res <- rbind(res, dat[, ..new_names][, year := i])
  rm(dat)
}

write_fst(res, here("Data", "firm_fin_panel.fst"))


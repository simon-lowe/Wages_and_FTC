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



# Do the thing ------------------------------------------------------------

dat1 <- read_fst(here("Data", "postes_panel_0509_CDID.fst"), as.data.table = TRUE,
                 columns = c("ident_all", "year", "cs_clean"))

dat2 <- read_fst(here("Data", "postes_panel_1014_CDID.fst"), as.data.table = TRUE,
                 columns = c("ident_all", "year", "cs_clean"))

dat <- rbind(dat1, dat2)
rm(dat1, dat2)
gc()

dat <- dat[, .(cs_entry = min(year)), by = .(ident_all, cs_clean)]

write.fst(dat, here("Data", "cs_entry_data.fst"))

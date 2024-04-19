rm(list = ls())
gc()

# Load packages -----------------------------------------------------------

list.of.packages <- c(
  "tidyverse", "data.table", "haven", "fst", "expss", "janitor", "here",
  "DescTools", "tictoc", "collapse", "readxl",
  "fixest", "binsreg", "did", "ggplot2", "patchwork",
  "igraph"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(list.of.packages, library, character.only = TRUE))
rm(list.of.packages, new.packages)

setDTthreads(0)


# Load data ---------------------------------------------------------------
ipc <- read.fst("C:/Users/Public/Documents/sources/Inflation/ipc.fst", as.data.table = T)
ze_dico <- read.fst("C:/Users/Public/Documents/sources/ZE homogeneization/ze_dico.fst", as.data.table = TRUE)
ape_dico <- read.fst(here("Data", "siret_apet_DPAE.fst"), as.data.table = TRUE)

for(i in 2005:2014){
  print(i)
  dat <- fread(paste0("C:/Users/Public/Documents/sources/Pseudo_ID/dads_out/dads", i, ".csv"))
  setDT(dat)
  
  # Clean names
  dat <- clean_names(dat)

  
  # Remove duplicates
  dat <- dat[!duplicated(dat),][, ident_s := NULL]
  gc()

  # Convert dates to numeric
  dat[, ':=' (datdeb = as.numeric(datdeb), datfin = as.numeric(datfin))]
  
  # Create siret variable
  dat[, siret := paste0(siren, nic)]
  
  # Drop the unemployed
  dat <- dat[filt %in% 1:2]
  gc()

  # Drop "firms" that are private individuals employing individuals
  dat[, sir_fict := substr(siret, 1, 1) == "P" | substr(nic, 1, 1) == 9]
  dat <- dat[sir_fict == FALSE][, sir_fict := NULL]
  gc()
  
  # Sector restrictions
  dat <- dat[!is.na(catjur)]
  dat[, catjur := as.numeric(catjur)]
  
  # Here was the recoding of dompempl
  
  # Drop indidivual (micro) entrepreneurs
  dat <- dat[domempl_hom != 6][, domempl := NULL]

  # Exclude France extra-territorial
  dat <- dat[!(dept %in% c("97", "971", "972", "973", "974", "975", "977", "978", "98", "99"))]

  # Location data
  dat <- merge(dat[comt != ""], ze_dico, by = "comt", all.x = TRUE, all.y = FALSE)
  dat[, na_comt_sir := any(is.na(zempt10)), by = siret]
  dat[na_comt_sir == TRUE, zempt10 := ifelse(!is.na(zempt10), zempt10, unique(zempt10[!is.na(zempt10)])[1]), by = siret]

  # Hours variable
  dat <- dat[!is.na(nbheur) & nbheur >= 0]

  # Wages
  dat <- merge(dat, ipc[, .(year, inf_mult_12)], by = "year", all.x = TRUE, all.y = FALSE)
  dat[, sbr := s_brut*inf_mult_12][, inf_mult_12 := NULL]
  dat[, hwr := sbr/nbheur]
  
  dat[, l_sbr := log(sbr)][, l_sbr_w := Winsorize(l_sbr, probs = c(0.01, 0.99))]
  dat[, l_hwr := log(hwr)][, l_hwr_w := Winsorize(l_hwr, probs = c(0.01, 0.99), na.rm = TRUE)]
  
  # Industry
  dat <- dat[!is.na(apet)]
  dat[, a2 := substr(apet, 1, 2)]

  # Occupation
  dat[, pcs := toupper(pcs)]
  dat[, cs2 := substr(pcs, 1, 2)]
  dat[str_detect(cs2, "\\d\\d", negate = T), cs2 := ""]
  dat[, cs2 := as.numeric(cs2)][, cs_clean := ifelse(year <= 2008, cs, cs2)][cs_clean %in% c(84, 86, 99) | is.na(cs_clean), cs_clean := 0]
  
  # Type of contract
  dat[, cdi := contrat_travail == 1]
  dat[, cdd := contrat_travail == 2 | contrat_travail == 96]
  dat[, interim := contrat_travail == 3]
  
  # Part-time
  dat[, pt := 4]
  dat[cpfd == "C", pt := 1]
  dat[cpfd == "P", pt := 2]
  dat[cpfd == "F", pt := 3]
  
  write.fst(dat, paste0("C:/Users/Public/Documents/sources/Pseudo_ID/Cleaned/postes_panel", i, ".fst"))
  rm(dat)
}


# Merge years -------------------------------------------------------------

rm(list = ls())
gc()

dat <- data.table()

ape_dico <- read.fst(here("Data", "siret_apet_DPAE.fst"), as.data.table = TRUE)
setnames(ape_dico, "apet", "apet_hom")

apet_conv <- read_excel("C:/Users/Public/Documents/sources/Industry homogeneization/Conversion_Tables.xlsx", 
                        sheet = "APET conversion")
setDT(apet_conv)
apet_conv <- clean_names(apet_conv)

apet_conv <- apet_conv[, apet2 := substr(str_remove(apet2, "\\."), 1, 5)][, apet_pre08 := substr(str_remove(apet, "\\."), 1, 4)][, .(apet2, apet_pre08)] %>% unique()

apet_conv[, n := .N, by = apet_pre08]

setorder(apet_conv, apet_pre08)

apet_conv <- apet_conv[, .(apet2 = head(apet2, 1)), by = .(apet_pre08, n)]

apet_conv[, apet2_rand := apet2]
apet_conv[n == 1, apet2 := NA][, n := NULL]

for(i in 2005:2009){
  print(i)
  tmp <- read_fst(paste0("C:/Users/Public/Documents/sources/Pseudo_ID/Cleaned/postes_panel", i, ".fst"), as.data.table = TRUE)
  
  tmp <- merge(tmp[(cdd == TRUE | cdi == TRUE) & !is.na(age)], ape_dico, by = c("siret", "year"), all.x = TRUE, all.y = FALSE)
  
  if(i < 2008){
    tmp <- merge(tmp, apet_conv, by.x = "apet", by.y = "apet_pre08", all.x = TRUE, all.y = FALSE)
  }
  
  dat <- rbind(dat, tmp, fill = TRUE)
  rm(tmp)
  gc()
}

# Dealing with the industry code change
dat[year >= 2008, apet_full := apet]
dat[, mean(is.na(apet_full))]

# Use apet code changes which were 1:1
dat[!is.na(apet2) & is.na(apet_full), apet_full := apet2]
dat[, mean(is.na(apet_full))]

# For firms with an apet code in 2008 or after
dat[, apet_post08 := head(apet[!is.na(apet) & year >= 2008], 1), by = siret]
dat[!is.na(apet_post08) & is.na(apet_full), apet_full := apet_post08]
dat[, mean(is.na(apet_full))]

# Using the apet from DPAE
dat[is.na(apet_full) & !is.na(apet_hom), apet_full := apet_hom]
dat[, mean(is.na(apet_full))]

# Using an APET in the SIREN
dat[, apen_post08 := head(apet[!is.na(apet) & year >= 2008], 1), by = siren]
dat[is.na(apet_full) &!is.na(apen_post08) & year < 2008, apet_full := apen_post08]
dat[, mean(is.na(apet_full))]

# For the last %, do it simply by order:
dat[, apet_full2 := apet_full]

dat[!is.na(apet2_rand) & is.na(apet_full2), apet_full2 := apet2_rand]
dat[, mean(is.na(apet_full2))]

# Drop intermediary variables
dat[, c("apet_post08", "apen_post08", "apet2", "apet2_rand") := NULL]

# Create the aggregated industry codes
# Industry conversion table
ind_conv <- read_excel("C:/Users/Public/Documents/sources/Industry homogeneization/Conversion_Tables.xlsx", 
                       sheet = "Aggregation")
setDT(ind_conv)
ind_conv <- clean_names(ind_conv)
ind_conv[, apet_full2 := str_remove(apet2, "\\.")]
ind_conv <- ind_conv[, 5:10]

dat <- merge(dat, ind_conv, by = "apet_full2")

dat[, c("a88", "a2", "a6", "a17", "a38", "apet_hom", "nes16", "nes114", "nes36", "nes5") := NULL]


write_fst(dat, here("Data", "postes_panel_0509_CDID.fst"))


rm(list = ls())
gc()

dat <- data.table()

for(i in 2010:2014){
  print(i)
  tmp <- read_fst(paste0("C:/Users/Public/Documents/sources/Pseudo_ID/Cleaned/postes_panel", i, ".fst"), as.data.table = TRUE)
  
  dat <- rbind(dat, tmp[(cdd == TRUE | cdi == TRUE) & !is.na(age)], fill = TRUE)
  rm(tmp)
  gc()
}

write_fst(dat, here("Data", "postes_panel_1014_CDID.fst"))

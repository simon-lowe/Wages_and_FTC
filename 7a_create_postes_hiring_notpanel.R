rm(list = ls())
gc()

# Load packages -----------------------------------------------------------

list.of.packages <- c(
  "tidyverse", "data.table", "haven", "fst", "expss", "janitor", "here", "DescTools"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)

invisible(lapply(list.of.packages, library, character.only = TRUE))
rm(list.of.packages, new.packages)

setDTthreads(0)
# setFixest_nthreads(8)

# Aggregate data ---------------------------------------------------------------

# Load inflation adjustment
ipc <- read.fst("C:/Users/Public/Documents/sources/Inflation/ipc.fst", as.data.table = T)

# CZ adjustment
ze_dico <- read.fst("C:/Users/Public/Documents/sources/ZE homogeneization/ze_dico.fst", as.data.table = TRUE)

# Source for postes
postes_source <- "C:/Users/Public/Documents/sources/Poste_fst/"

files <- list.files(postes_source)

res <- data.table()

for(i in 12:14){
# i <- 10
print(i)

subfiles <- str_subset(files, paste0("post", sprintf("%02d", i), "_*"))

if(i > 7){
  sel_cols <- (c("ident_s", "siren", "filt", "nic", "sexe", "age",
                 "datdeb", "datdeb2", "datfin1", "datfin", "datdeb_1", "datfin_1", "duree", "duree_1", "cpfd",
                 "contrat_travail", "nbheur", "s_brut", "a88", "a38", "dept", "regt", "zempt", "domempl", "catjur",
                 "com_empl",
                 "pcs", "pcs_1",
                 "eff_0101_et", "eff_3112_et", "eff_moy_et",
                 "apen", "apet"))
}

dat <- data.table()

for(j in subfiles){
  
  # Load the data
  tmp_dat <- read.fst(paste0(postes_source, j), columns = (sel_cols), as.data.table = TRUE)
  
  # Remove duplicates
  tmp_dat <- tmp_dat[!duplicated(tmp_dat),]
  gc()
  
  # Remove NAs in age
  tmp_dat <- tmp_dat[!is.na(age)]
  tmp_dat <- tmp_dat[between(age, 18, 66)]
  
  # Convert dates to numeric
  tmp_dat[, ':=' (datdeb = as.numeric(datdeb), datfin = as.numeric(datfin))]
  
  # Create the plant identifier
  if(typeof(tmp_dat$nic) != "character"){
    tmp_dat[, siret := paste0(siren, sprintf("%05d", nic))]
  } else {
    tmp_dat[, siret := paste0(siren, nic)]
  }
  
  # Drop the unemployed
  tmp_dat <- tmp_dat[filt %in% 1:2]
  gc()
  
  # Drop "firms" that are private individuals employing individuals
  tmp_dat[, sir_fict := substr(siret, 1, 1) == "P" | substr(nic, 1, 1) == 9]
  tmp_dat <- tmp_dat[sir_fict == FALSE][, sir_fict := NULL]
  gc()
  
  # Sector restrictions
  tmp_dat <- tmp_dat[!is.na(catjur)]
  tmp_dat[, catjur := as.numeric(catjur)]
  
  tmp_dat[between(catjur, 2000, 3999) | between(catjur, 5000, 5497) | between(catjur, 5499, 6999), domempl_hom := 9]
  tmp_dat[between(catjur, 8000, 9999), domempl_hom := 8]
  tmp_dat[between(catjur, 1000, 1999) | catjur == 5498, domempl_hom := 6]
  tmp_dat[between(catjur, 4000, 4999), domempl_hom := 5]
  tmp_dat[(between(catjur, 7400, 7499) & siren != "180020026") | between(catjur, 7321, 7323) | catjur == 7381 |
        siren == "180035016" | siren == "180035065"|  siren == "180035024" | siren == "180035032", domempl_hom := 4]
  tmp_dat[between(catjur, 7364, 7366) | siren == "189400039" | siren == "180036063", domempl_hom := 3]
  tmp_dat[between(catjur, 7200, 7299) | between(catjur, 7312, 7314) | between(catjur, 7340, 7363) |
        between(catjur, 7371, 7379) | siren == "180014045", domempl_hom := 2]
  tmp_dat[(between(catjur, 7382, 7389) & !(siren %in% c("180014045", "180035016", "180035024", "180035032", "180035065", "180036063", "189400039"))) | 
        between(catjur, 7100, 7199) | catjur == 7331 | siren == "180020026", domempl_hom := 1]

  tmp_dat <- tmp_dat[domempl_hom %in% c(8,9)]
    
  # # Drop indidivual (micro) entrepreneurs
  # tmp_dat <- tmp_dat[domempl_hom != 6][, domempl := NULL]
  
  # Exclude France extra-territorial
  tmp_dat <- tmp_dat[!(dept %in% c("97", "971", "972", "973", "974", "975", "977", "978", "98", "99"))]
  
  # Location data
  setnames(tmp_dat, "com_empl", "comt") # ?????????????????????
  tmp_dat <- merge(tmp_dat[comt != ""], ze_dico, by = "comt", all.x = TRUE, all.y = FALSE)
  tmp_dat[, na_comt_sir := any(is.na(zempt10)), by = siret]
  tmp_dat[na_comt_sir == TRUE, zempt10 := ifelse(!is.na(zempt10), zempt10, unique(zempt10[!is.na(zempt10)])[1]), by = siret]
  
  # Hours variable
  tmp_dat <- tmp_dat[!is.na(nbheur) & nbheur >= 0]
  
  # Wages
  tmp_dat[, year := 2000 + i]
  tmp_dat <- merge(tmp_dat, ipc[, .(year, inf_mult_12)], by = "year", all.x = TRUE, all.y = FALSE)
  tmp_dat[, sbr := s_brut*inf_mult_12][, inf_mult_12 := NULL]
  tmp_dat[, hwr := sbr/nbheur]
  
  tmp_dat[, l_sbr := log(sbr)][, l_sbr_w := Winsorize(l_sbr, probs = c(0.01, 0.99), na.rm = TRUE)]
  tmp_dat[, l_hwr := log(hwr)][, l_hwr_w := Winsorize(l_hwr, probs = c(0.01, 0.99), na.rm = TRUE)]
  tmp_dat <- tmp_dat[l_hwr_w != Inf]
  
  # Industry
  tmp_dat <- tmp_dat[!is.na(apet)]
  
  # Occupation
  tmp_dat[, pcs := toupper(pcs)]
  tmp_dat[, cs2 := substr(pcs, 1, 2)]
  tmp_dat[str_detect(cs2, "\\d\\d", negate = T), cs2 := ""]
  tmp_dat[, cs2 := as.numeric(cs2)][, cs_clean := ifelse(year <= 2008, cs, cs2)][cs_clean %in% c(84, 86, 99) | is.na(cs_clean), cs_clean := 0]
  
  # Type of contract
  tmp_dat[, contrat_travail := as.numeric(contrat_travail)]
  tmp_dat[, cdi := contrat_travail == 1]
  tmp_dat[, cdd := contrat_travail == 2 | contrat_travail == 96]
  tmp_dat[, interim := contrat_travail == 3]
  
  # Part-time
  tmp_dat[, pt := 4]
  tmp_dat[cpfd == "C", pt := 1]
  tmp_dat[cpfd == "P", pt := 2]
  tmp_dat[cpfd == "F", pt := 3]
  
  dat <- rbind(dat, tmp_dat[cdd == TRUE | cdi == TRUE])
  
  rm(tmp_dat)
}

setorder(dat, ident_s, siret, datdeb)
dat[, obs_id := seq_len(.N), by = .(ident_s, siret)]

dat <- dat[is.na(datfin_1) & obs_id == 1]

res <- rbind(res, dat)
}


# Age bin
res[, age_bin := cut(age, breaks = c(seq(17, 66, by = 5), Inf))]

write.fst(res, here("Data", "postes_hiring_panel_1014.fst"))

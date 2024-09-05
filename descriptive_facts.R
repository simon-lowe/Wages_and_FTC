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


# DADS ---------------------------------------------------------------


# -------------------------------------------------------------------------





# -------------------------------------------------------------------------

# Load data - 2010 to 2014 ---------------------------------------------------------------

mw <- read.fst("C:/Users/Public/Documents/sources/Minimum wage/MW.fst")
dat <- read_fst(here("Data", "postes_panel_1014_CDID.fst"), as.data.table = TRUE,
                columns = c("l_hwr_w", "ident_all", "siret", "year", "age", "cdd", "sexe", 
                            "a38", "pcs", "cs_clean", "nbheur", "eff_3112_et", "dept", "zempt10",
                            "datdeb", "datfin", "duree", "domempl_hom", "est_entry",
                            "sbr", "hwr"))

# Domaine emploi
dat <- dat[l_hwr_w != Inf & domempl_hom %in% c(8,9)]

# Make ID numeric
dat[, ident_all := as.numeric(ident_all)]

# Create an estimated experience variable
dat[, exp := year - est_entry]

# Age restriction
dat <- dat[between(age, 18, 66)]


# Load data - 2005 to 2009 ---------------------------------------------------------------

mw <- read.fst("C:/Users/Public/Documents/sources/Minimum wage/MW.fst")
dat2 <- read_fst(here("Data", "postes_panel_0509_CDID.fst"), as.data.table = TRUE,
                 columns = c("l_hwr_w", "ident_all", "siret", "year", "age", "cdd", "sexe", 
                             "a_38", "pcs", "cs_clean", "nbheur", "eff_3112_et", "dept", "zempt10",
                             "datdeb", "datfin", "duree", "domempl_hom", "est_entry",
                             "sbr", "hwr"))

setnames(dat2, "a_38", "a38")

# Domaine emploi
dat2 <- dat2[l_hwr_w != Inf & domempl_hom %in% c(8,9)]

# Make ID numeric
dat2[, ident_all := as.numeric(ident_all)]

# Create an estimated experience variable
dat2[, exp := year - est_entry]

# Age restriction
dat2 <- dat2[between(age, 18, 66)]

dat <- rbind(dat, dat2)
rm(dat2)
gc()

# Create variables --------------------------------------------------------

# Age bins
dat[, age_bin := cut(age, breaks = c(seq(17, 66, by = 5), Inf))]

# PCS
dat[, pcs := toupper(pcs)]


# CDD fraction in employment time series ----------------------------------


res <- data.table()
for(i in 1:12){
  dat[, spell_at := between(1 + (i - 1)*30, datdeb, datfin)]
  tmp <- dat[spell_at == TRUE, .(f_cdd = mean(cdd)), keyby = year]
  res <- rbind(res, tmp[, at := i])
}

res2 <- res[, .(m_f_cdd = mean(f_cdd)), by = year]

p <- res2 %>%
  ggplot(aes(x = year, y = m_f_cdd)) +
  geom_point(size = 2.5, color = "#00008B") +
  # geom_line() +
  xlab(NULL) + ylab("Fraction of FTC in employment") +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14)) +
  # xlim() +
  scale_x_continuous(breaks = 2005:2014) +
  ylim(0, NA)
p
ggsave(p, filename = here("Results", "Descriptive facts", "FTC_employment_ts.png"))
# res3 <- dat[datdeb == 1, .(m_f_cdd = mean(cdd)), by = year]

# by age ------------------------------------------------------------------

res_a <- data.table()
for(i in 1:12){
  dat[, spell_at := between(1 + (i - 1)*30, datdeb, datfin)]
  tmp <- dat[spell_at == TRUE, .(f_cdd = mean(cdd)), keyby = age]
  # tmp <- dat[spell_at == TRUE, .(f_cdd = mean(cdd)), keyby = .(year, age)]
  res_a <- rbind(res_a, tmp[, at := i])
}

res2_a <- res_a[, .(m_f_cdd = mean(f_cdd)), by = age]
# res2_a <- res_a[, .(m_f_cdd = mean(f_cdd)), by = .(year, age)]

p <- res2_a %>%
  ggplot(aes(x = age, y = m_f_cdd)) +
  # ggplot(aes(x = age, y = m_f_cdd, color = as.factor(year))) +
  geom_point(size = 2.5, color = "#00008B") +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14)) + 
  xlab("Age") + ylab("Fraction of employment in FTC") +
  scale_y_continuous(breaks = seq(0, 0.7, 0.1), limits = c(0, 0.7))
p
ggsave(p, filename = here("Results", "Descriptive facts", "cdd_employment_by_age_0514.png"))

# by occupation -----------------------------------------------------------

# CS
res_cs <- data.table()
for(i in 1:12){
  dat[, spell_at := between(1 + (i - 1)*30, datdeb, datfin)]
  tmp <- dat[spell_at == TRUE, .(f_cdd = mean(cdd)), keyby = cs_clean]
  # res[, paste0("f_cdd", i) := tmp$f_cdd]
  res_cs <- rbind(res_cs, tmp[, at := i])
}

res2_cs <- res_cs[, .(m_f_cdd = mean(f_cdd)), by = cs_clean]

hist(res2_cs$m_f_cdd, breaks = )


foo <- dat[, .(m_hw = mean(l_hwr_w)), by = cs_clean]

foo2 <- merge(res2_cs, foo)

summary(lm(data = foo2, m_f_cdd ~ m_hw))

plot(foo2$m_hw, foo2$m_f_cdd)

# PCS
res_pcs <- data.table()
for(i in 1:12){
  dat[, spell_at := between(1 + (i - 1)*30, datdeb, datfin)]
  tmp <- dat[year %in% 2010:2014 & spell_at == TRUE, .(f_cdd = mean(cdd), n = .N), keyby = pcs]
  res_pcs <- rbind(res_pcs, tmp[, at := i])
}

res2_pcs <- res_pcs[, .(m_f_cdd = mean(f_cdd)), by = pcs]

res2_pcs[, cs := substr(pcs, 1, 2)]

p <- res2_pcs[pcs != "" & cs != "10"] %>%
  ggplot(aes(x = cs, y = m_f_cdd)) +
  geom_boxplot() +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14)) +
  xlab("2-digit Occupation code") + ylab("Fraction of employment in FTC by 4-digit occupation code")
p
ggsave(p, filename = here("Results", "Descriptive facts", "cdd_employment_by_occupation_0514.png"), width = 7, height = 7)


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# DPAE --------------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------

# Load data ---------------------------------------------------------------

rm(list = ls())
gc()

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


# Restriction --------------------------------------------------------------

dat <- dat[age %between% c(18, 66)]
dat <- dat[year %between% c(2005, 2014)]

# Hiring time series ------------------------------------------------------

# Drop 0

tp <- dat[, .(FTC = mean(cdd), OEC = mean(cdi)), by = year]

tp <- melt(tp, id.vars = "year")

setnames(tp, "variable", "Contract type")

p <- tp %>%
  ggplot(aes(x = year, y = value, color = `Contract type`)) +
  geom_point(size = 2.5) +
  # geom_line(size = 1) + 
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0, 9, 0.1), limits = c(0, 0.9)) +
  scale_x_continuous(breaks = 2005:2014) +
  xlab(NULL) + ylab("Fraction of hiring")
p
ggsave(p, filename = here("Results", "Descriptive facts", "cdd_hiring_0414_drop0.png"))

# Drop 1

tp <- dat[drop_sample == FALSE, .(FTC = mean(cdd), OEC = mean(cdi)), by = year]

tp <- melt(tp, id.vars = "year")

setnames(tp, "variable", "Contract type")

p <- tp %>%
  ggplot(aes(x = year, y = value, color = `Contract type`)) +
  geom_point(size = 2.5) +
  # geom_line(size = 1) + 
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0, 9, 0.1), limits = c(0, 0.9)) +
  scale_x_continuous(breaks = 2005:2014) +
  xlab(NULL) + ylab("Fraction of hiring")
p
ggsave(p, filename = here("Results", "Descriptive facts", "cdd_hiring_0414_drop1.png"))

# Drop 2

tp <- dat[drop_sample == FALSE & drop_sample_id == FALSE, .(FTC = mean(cdd), OEC = mean(cdi)), by = year]

tp <- melt(tp, id.vars = "year")

setnames(tp, "variable", "Contract type")

p <- tp %>%
  ggplot(aes(x = year, y = value, color = `Contract type`)) +
  geom_point(size = 2.5) +
  # geom_line(size = 1) + 
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0, 9, 0.1), limits = c(0, 0.9)) +
  scale_x_continuous(breaks = 2005:2014) +
  xlab(NULL) + ylab("Fraction of hiring")
p
ggsave(p, filename = here("Results", "Descriptive facts", "cdd_hiring_0414_drop2.png"))



# by age ------------------------------------------------------------------

# hiring
tp <- dat[drop_sample == FALSE & drop_sample_id == FALSE, .(f_cdd = mean(cdd)), by = age]

p <- tp %>%
  ggplot(aes(x = age, y = f_cdd)) +
  geom_point(size = 2.5, color = "#00008B") +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14)) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), limits = c(0, 1)) +
  xlab("Age") + ylab("Fraction of hiring")
p
ggsave(p, filename = here("Results", "Descriptive facts", "cdd_hiring_by_age_0414_drop2.png"))


# CDD length
tp1 <- dat[cdd == TRUE & drop_sample == FALSE & drop_sample_id == FALSE, .(cdd_length = mean(as.numeric(cdd_length), na.rm = TRUE),
                                                            col_t = "Mean"), by = age]
tp2 <- dat[cdd == TRUE & drop_sample == FALSE & drop_sample_id == FALSE, .(cdd_length = median(as.numeric(cdd_length), na.rm = TRUE),
                                                            col_t = "Median"), by = age]

tp <- rbind(tp1, tp2)

p <- tp %>%
  ggplot(aes(x = age, y = cdd_length, color = col_t)) +
  geom_point() +
  theme_classic() +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.position = "bottom") +
  ylim(0, NA) +
  xlab("Age") + ylab("FTC length in days") + labs(color = NULL)
p
ggsave(p, filename = here("Results", "Descriptive facts", "cdd_length_by_age_0414_drop2.png"))

# Firm level --------------------------------------

# Hiring dispersion
dat2 <- dat[, .(f_cdd = mean(cdd)), by = .(siren)]
dat3 <- dat[effent > 20, .(f_cdd = mean(cdd)), by = .(siren)]

dat4 <- rbind(dat2[, source := "All firms"], dat3[, source := "Firms > 20"])

# p <- dat2 %>%
p <- dat4 %>%
  # ggplot(aes(x = f_cdd)) +
  ggplot(aes(x = f_cdd, color = source)) +
  stat_ecdf()+
  theme_bw() +
  ylab("Fraction of firms") + xlab("Fraction of hiring in FTC") + 
  theme(legend.title = element_blank())
p
ggsave(p, filename = here("Results", "Descriptive facts", "cdd_hiring_firm_allyearcombined_distribution_0514.png"))


# Length of CDD
dat2 <- dat[cdd == TRUE, .(m_cdd_dur = as.numeric(mean(cdd_length, na.rm = TRUE)),
                           f_cdd = mean(cdd)), by = .(siren, year)]

p <- dat2 %>%
  ggplot(aes(x = m_cdd_dur)) +
  stat_ecdf()+
  theme_bw() +
  geom_vline(aes(xintercept = 182), linetype = 'dashed')+ 
  geom_vline(aes(xintercept = 365), linetype = 'dashed')+ 
  geom_vline(aes(xintercept = 365 + 182), linetype = 'dashed')+ 
  geom_vline(aes(xintercept = 2*365), linetype = 'dashed') +
  geom_vline(aes(xintercept = 90), linetype = 'dashed') +
  ylab("Fraction of firms") + xlab("Mean length of FTC")
p
ggsave(p, filename = here("Results", "Descriptive facts", "cdd_length_firm_distribution_0514.png"))

# Length of CDD - all year combined
dat2 <- dat[cdd == TRUE, .(m_cdd_dur = as.numeric(mean(cdd_length, na.rm = TRUE)),
                           f_cdd = mean(cdd)), by = .(siren)]

p <- dat2 %>%
  ggplot(aes(x = m_cdd_dur)) +
  stat_ecdf()+
  theme_bw() +
  geom_vline(aes(xintercept = 182), linetype = 'dashed')+ 
  geom_vline(aes(xintercept = 365), linetype = 'dashed')+ 
  geom_vline(aes(xintercept = 365 + 182), linetype = 'dashed')+ 
  geom_vline(aes(xintercept = 2*365), linetype = 'dashed') +
  geom_vline(aes(xintercept = 90), linetype = 'dashed') +
  ylab("Fraction of firms") + xlab("Mean length of FTC")
p
ggsave(p, filename = here("Results", "Descriptive facts", "cdd_length_firm_allyearcombined_distribution_0514.png"))


# Conversions -------------------------------------------------------------

# Individual level

dat[, n_match_cdi := sum(cdi), by = .(ident_indiv, siret)]
dat[, n_match_cdd := sum(cdd), by = .(ident_indiv, siret)]

dat_col <- dat[, .(has_conv = any(n_match_cdi > 0 & n_match_cdd > 0)), by = ident_indiv]

write(dat_col[, mean(has_conv)], file = here("Results", "Descriptive facts", "mean_conv_indiv.txt"))

# Match level
dat_col <- dat[, .(n_match_cdd = sum(cdd), n_match_cdi = sum(cdi)), by = .(ident_indiv, siret)]

write(dat_col[n_match_cdd > 0, mean(n_match_cdi > 0)], file = here("Results", "Descriptive facts", "mean_conv_match.txt"))

# Siren level - CDI - Drop 0 ---------------------

dat_tp <- read.fst(here("Data", "dpae_col_hiring_siren_drop0.fst"), as.data.table = TRUE)

dat_tp[, y := n_conv_cdi/n_cdi_h]

p1 <- dat_tp[y > 0] %>%
  ggplot(aes(x = y)) +
  geom_histogram(aes(y = stat(count/sum(count)))) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  # ggtitle(paste0("All - 0 conv: ", round(dat_tp[, mean(y == 0)]*100, 2), "%")) +
  ggtitle(paste0("All - 0 conv: ", round(dat_tp[, mean(n_conv_cdi == 0)]*100, 2), "%")) +
  xlab("Fraction of OEC hires that come from FTC-conversion") +
  ylab("Percent")
p1

p2 <- dat_tp[y > 0 & n_h > 20] %>%
  ggplot(aes(x = y)) +
  geom_histogram(aes(y = stat(count/sum(count)))) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  # ggtitle(paste0("# hires > 20 - 0 conv:", round(dat_tp[n_h > 20, mean(y == 0)]*100, 2), "%")) +
  ggtitle(paste0("# hires > 20 - 0 conv: ", round(dat_tp[n_h > 20, mean(n_conv_cdi == 0)]*100, 2), "%")) +
  # xlab("Fraction of OEC hires that come from FTC-conversion") + 
  ylab("Percent")
p2


p3 <- dat_tp[y > 0 & n_h > 100] %>%
  ggplot(aes(x = y)) +
  geom_histogram(aes(y = stat(count/sum(count)))) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  # ggtitle(paste0("# hires > 100 - 0 conv:", round(dat_tp[n_h > 100, mean(y == 0)]*100, 2), "%")) +
  ggtitle(paste0("# hires > 100 - 0 conv: ", round(dat_tp[n_h > 100, mean(n_conv_cdi == 0)]*100, 2), "%")) +
  # xlab("Fraction of OEC hires that come from FTC-conversion") + 
  ylab("Percent")
p3

p <- p1 + p2 + p3 & xlab(NULL)
pf <- wrap_elements(panel = p) + labs(tag = "Fraction of OEC hires that come from FTC-conversion") +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )
pf
ggsave(pf, filename = here("Results", "Descriptive facts", "f_conv_cdi_siren_drop0.png"), width = 15, height = 8)


# Siren level - CDI - Drop 2 --------------------

dat_tp <- read.fst(here("Data", "dpae_col_hiring_siren_drop2.fst"), as.data.table = TRUE)

dat_tp[, y := n_conv_cdi/n_cdi_h]

p1 <- dat_tp[y > 0] %>%
  ggplot(aes(x = y)) +
  geom_histogram(aes(y = stat(count/sum(count)))) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  # ggtitle(paste0("All - 0 conv: ", round(dat_tp[, mean(y == 0)]*100, 2), "%")) +
  ggtitle(paste0("All - 0 conv: ", round(dat_tp[, mean(n_conv_cdi == 0)]*100, 2), "%")) +
  xlab("Fraction of OEC hires that come from FTC-conversion") +
  ylab("Percent")
p1

p2 <- dat_tp[y > 0 & n_h > 20] %>%
  ggplot(aes(x = y)) +
  geom_histogram(aes(y = stat(count/sum(count)))) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  # ggtitle(paste0("# hires > 20 - 0 conv:", round(dat_tp[n_h > 20, mean(y == 0)]*100, 2), "%")) +
  ggtitle(paste0("# hires > 20 - 0 conv: ", round(dat_tp[n_h > 20, mean(n_conv_cdi == 0)]*100, 2), "%")) +
  # xlab("Fraction of OEC hires that come from FTC-conversion") + 
  ylab("Percent")
p2


p3 <- dat_tp[y > 0 & n_h > 100] %>%
  ggplot(aes(x = y)) +
  geom_histogram(aes(y = stat(count/sum(count)))) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  # ggtitle(paste0("# hires > 100 - 0 conv:", round(dat_tp[n_h > 100, mean(y == 0)]*100, 2), "%")) +
  ggtitle(paste0("# hires > 100 - 0 conv: ", round(dat_tp[n_h > 100, mean(n_conv_cdi == 0)]*100, 2), "%")) +
  # xlab("Fraction of OEC hires that come from FTC-conversion") + 
  ylab("Percent")
p3

p <- p1 + p2 + p3 & xlab(NULL)
pf <- wrap_elements(panel = p) + labs(tag = "Fraction of OEC hires that come from FTC-conversion") +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )
pf
ggsave(pf, filename = here("Results", "Descriptive facts", "f_conv_cdi_siren_drop2.png"), width = 15, height = 8)


# Siren level - CDD - Drop 0 ---------------------

dat_tp <- read.fst(here("Data", "dpae_col_hiring_siren_drop0.fst"), as.data.table = TRUE)

dat_tp[, y := n_conv_cdd/n_cdd_h]

p1 <- dat_tp[y > 0] %>%
  ggplot(aes(x = y)) +
  geom_histogram(aes(y = stat(count/sum(count)))) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  # ggtitle(paste0("All - 0 conv: ", round(dat_tp[, mean(y == 0)]*100, 2), "%")) +
  ggtitle(paste0("All - 0 conv: ", round(dat_tp[, mean(n_conv_cdd == 0)]*100, 2), "%")) +
  xlab("Fraction of FTC hires that will get converted to OEC") +
  ylab("Percent")
p1

p2 <- dat_tp[y > 0 & n_h > 20] %>%
  ggplot(aes(x = y)) +
  geom_histogram(aes(y = stat(count/sum(count)))) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  # ggtitle(paste0("# hires > 20 - 0 conv:", round(dat_tp[n_h > 20, mean(y == 0)]*100, 2), "%")) +
  ggtitle(paste0("# hires > 20 - 0 conv: ", round(dat_tp[n_h > 20, mean(n_conv_cdd == 0)]*100, 2), "%")) +
  # xlab("Fraction of OEC hires that come from FTC-conversion") + 
  ylab("Percent")
p2


p3 <- dat_tp[y > 0 & n_h > 100] %>%
  ggplot(aes(x = y)) +
  geom_histogram(aes(y = stat(count/sum(count)))) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  # ggtitle(paste0("# hires > 100 - 0 conv:", round(dat_tp[n_h > 100, mean(y == 0)]*100, 2), "%")) +
  ggtitle(paste0("# hires > 100 - 0 conv: ", round(dat_tp[n_h > 100, mean(n_conv_cdd == 0)]*100, 2), "%")) +
  # xlab("Fraction of OEC hires that come from FTC-conversion") + 
  ylab("Percent")
p3

p <- p1 + p2 + p3 & xlab(NULL)
pf <- wrap_elements(panel = p) + labs(tag = "Fraction of FTC hires that will get converted to OEC") +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )
pf
ggsave(pf, filename = here("Results", "Descriptive facts", "f_conv_cdd_siren_drop0.png"), width = 15, height = 8)


# Siren level - CDD - Drop 2 --------------------

dat_tp <- read.fst(here("Data", "dpae_col_hiring_siren_drop2.fst"), as.data.table = TRUE)

dat_tp[, y := n_conv_cdd/n_cdd_h]

p1 <- dat_tp[y > 0] %>%
  ggplot(aes(x = y)) +
  geom_histogram(aes(y = stat(count/sum(count)))) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  # ggtitle(paste0("All - 0 conv: ", round(dat_tp[, mean(y == 0)]*100, 2), "%")) +
  ggtitle(paste0("All - 0 conv: ", round(dat_tp[, mean(n_conv_cdd == 0)]*100, 2), "%")) +
  xlab("Fraction of FTC hires that will get converted to OEC") +
  ylab("Percent")
p1

p2 <- dat_tp[y > 0 & n_h > 20] %>%
  ggplot(aes(x = y)) +
  geom_histogram(aes(y = stat(count/sum(count)))) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  # ggtitle(paste0("# hires > 20 - 0 conv:", round(dat_tp[n_h > 20, mean(y == 0)]*100, 2), "%")) +
  ggtitle(paste0("# hires > 20 - 0 conv: ", round(dat_tp[n_h > 20, mean(n_conv_cdi == 0)]*100, 2), "%")) +
  ylab("Percent")
p2


p3 <- dat_tp[y > 0 & n_h > 100] %>%
  ggplot(aes(x = y)) +
  geom_histogram(aes(y = stat(count/sum(count)))) +
  theme_classic() +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14)) +
  # ggtitle(paste0("# hires > 100 - 0 conv:", round(dat_tp[n_h > 100, mean(y == 0)]*100, 2), "%")) +
  ggtitle(paste0("# hires > 100 - 0 conv: ", round(dat_tp[n_h > 100, mean(n_conv_cdi == 0)]*100, 2), "%")) +
  ylab("Percent")
p3

p <- p1 + p2 + p3 & xlab(NULL)
pf <- wrap_elements(panel = p) + labs(tag = "Fraction of FTC hires that will get converted to OEC") +
  theme(
    text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )
pf
ggsave(pf, filename = here("Results", "Descriptive facts", "f_conv_cdd_siren_drop2.png"), width = 15, height = 8)


# # Distribution of CDD length ----------------------------------------------
# 
# p <- dat[cdd_length < 3*365] %>%
#   ggplot(aes(x = as.numeric(cdd_length))) +
#   stat_ecdf()+
#   theme_bw() +
#   geom_vline(aes(xintercept = 182), linetype = 'dashed')+ 
#   geom_vline(aes(xintercept = 365), linetype = 'dashed')+ 
#   geom_vline(aes(xintercept = 365 + 182), linetype = 'dashed')+ 
#   geom_vline(aes(xintercept = 2*365), linetype = 'dashed') +
#   geom_vline(aes(xintercept = 90), linetype = 'dashed') +
#   ylab("Fraction of individuals") + xlab("Mean length of CDD")
# p
# ggsave(p, filename = here("Results", "Descriptive facts", "cdd_length_distribution_0414.png"))
# 

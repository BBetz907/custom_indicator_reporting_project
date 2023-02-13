
col_order <- c("reportingperiod", "orgunit", "orgunituid",
               "mech_code", "partner", "ou", "orgunit_parent", "indicator",
               "sex", "age", "otherdisaggregate", "population","numdenom", "value")
mission2report <- c("Eswatini", "Cote d'Ivoire")

ci_fhi <- bind_rows(bdi, bwa, civ, cod, lso, mwi, swz, tnz, zaf, idn, kaz, kgz, tjk, lao, npl, mmr,  phl,  bfa, gha, lbr, mli, sen, tgo) %>%
  #exclude what country teams report
  filter(!(str_detect(tolower(indicator), "prep") & country == "Botswana"),
         !country %in% mission2report)

#prepare data for export by selecting reporting columns and summarizing by reported columns
ci_fhi360 <-  ci_fhi %>%    select(reportingperiod, orgunit, orgunituid,
         mech_code, partner, ou, orgunit_parent, indicator,
         sex, age, otherdisaggregate, population,numdenom, value) %>%
    group_by(across(-c(value))) %>% summarise(value = sum(value), .groups ="drop") %>%
  glimpse()

ci <- ci_fhi360[,col_order]

ous <- c(unique(ci$ou))
reportingperiod <- unique(c(ci$reportingperiod))

for (i in unique(ous)){
  ou_file <- subset(ci, ou %in% i)
  write.csv(    ou_file, paste0("Dataout/", reportingperiod, "/", i, ".csv"), na = "", row.names = FALSE)
}


# test <- bind_rows(idn, kaz, kgz, tjk, npl, mmr,  phl, bwa, swz, tnz, zaf, bfa, gha, lbr, mli, sen, tgo) %>% 
  # filter(is.na(orgunit_level)) %>% 
  # glimpse()
table(ci$ou, ci$indicator)

# rm(list = ls()[grepl("bwa", ls())])

country <- ci_fhi %>% filter(country == "Botswana")
table(country$partner)
table(country$orgunit_parent, country$indicator)


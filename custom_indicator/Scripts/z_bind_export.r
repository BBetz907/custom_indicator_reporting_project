
col_order <- c("reportingperiod", "orgunit", "orgunituid",
               "mech_code", "partner", "ou", "orgunit_parent", "indicator",
               "sex", "age", "otherdisaggregate", "population","numdenom", "value")

ci_fhi <- bind_rows(idn, kaz, kgz, tjk, npl, mmr,  phl, bwa, swz, tnz, zaf) %>%
  select(reportingperiod, orgunit, orgunituid,
         mech_code, partner, ou, orgunit_parent, indicator,
         sex, age, otherdisaggregate, population,numdenom, value) %>%
    group_by(across(-c(value))) %>% summarise(value = sum(value), .groups ="drop") %>%
  glimpse()

ci <- ci_fhi[,col_order]

ous <- c(unique(ci$ou))
reportingperiod <- unique(c(ci$reportingperiod))

for (i in unique(ous)){
  ou_file <- subset(ci, ou %in% i)
  write.csv(    ou_file, paste0("Dataout/", reportingperiod, "/", i, ".csv"), na = "", row.names = FALSE)
}




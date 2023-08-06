
col_order <- c("reportingperiod", "orgunit", "orgunituid",
               "mech_code", "partner", "ou", "psnu", "indicator",
               "sex", "age", "otherdisaggregate", "population","numdenom", "value")
mission2report <- c("Eswatini", "Cote d'Ivoire")

ci_fhi <- do.call(bind_rows, lapply(ls(pattern = "_merge_psnu"), get)) %>%
  #exclude what country teams report
  filter(!(str_detect(tolower(indicator), "prep") & country == "Botswana"),
         !country %in% mission2report)

#prepare data for export by selecting reporting columns and summarizing by reported columns
ci_fhi360 <-  ci_fhi %>%    select(reportingperiod, orgunit, orgunituid,
         mech_code, partner, ou, psnu, indicator,
         sex, age, otherdisaggregate, population,numdenom, value) %>%
    group_by(across(-c(value))) %>% summarise(value = sum(value), .groups ="drop") %>% 
  mutate(indicator = recode(indicator, "TX_PVLS_ELIGIBLE_VERIFY" = "TX_PVLS_ELIGIBLE")) %>%
  glimpse()

ci <- ci_fhi360[,col_order] 

ous <- c(unique(ci$ou))
reportingperiod <- unique(c(ci$reportingperiod))

for (i in ous){
  ou_file <- subset(ci, ou %in% i)
  write.csv(ou_file, paste0("Dataout/", reportingperiod, "/", i, ".csv"), na = "", row.names = FALSE)
}


ci |> count(ou, indicator) |> pivot_wider(names_from = indicator, values_from = n) |> relocate(TX_PVLS_ELIGIBLE, .before = PrEP_CT_VERIFY) |> 
  mutate_at(c(2:9), ~replace_na(.,0))

# rm(list = ls()[grepl("bwa", ls())])

country <- ci_fhi %>% filter(country == "Botswana")
table(country$partner)
table(country$psnu, country$indicator)


snapshot_indicators <- c("TX_CURR_VERIFY", "TX_PVLS_VERIFY", "TX_PVLS_ELIGIBLE")
site_type_indicators <- c("TX_NEW_VERIFY","TX_CURR_VERIFY","TX_PVLS_VERIFY")
last_month <- c("Dec","Mar","Jun","Sep")
indicators_less_than_20 <- c("TX_NEW_VERIFY","TX_RTT_VERIFY","TX_CURR_VERIFY","TX_PVLS_VERIFY", "PrEP_OFFER","PrEP_CT_VERIFY", "PrEP_NEW_VERIFY")
less_than_20 <- c("<01","01-04","05-09","10-14","15-19")
keypop <- c("FSW","MSM","PWID","TG","Prison")


# Select Current Quarter --------------------------------------------------
current_q <- "fy23_q3"

# identify files ----------------------------------------------------------
files <- list.files(path="data",pattern = current_q) %>% paste("data/",.) %>% str_replace(" ", "") 
kp_disaggs_counts <- files %>% keep(grepl("kp_disaggs_counts", files))
age_sex_counts <- files %>% keep(grepl("age_sex_counts", files))
age_sex_snapshots <- files %>% keep(grepl("snapshot", files))

#KP disaggs cleaning
kp_disaggs_counts <- read.csv(kp_disaggs_counts)

kp_disaggs_counts_clean <- kp_disaggs_counts %>% clean_names %>% 
  dplyr::rename(indicator = data_element_short_name, population = all_target_populations, otherdisaggregate = pepfar) %>% 
  unite(reportingperiod, c("fiscal_year_short_name","fiscal_quarter"), sep = " Q") %>% 
  separate(indicator, c("indicator", "numdenom"), sep = "[ ]") %>%
  mutate(numdenom = if_else(numdenom == "(D)", "Denominator", "Numerator"),
         population = recode(population,
                "Female Sex Workers" = "FSW",
                "Transgender People" = "TG"),
         population = case_when(!(population %in% keypop) ~ "Non-KP GP", TRUE ~ as.character(population)),
         otherdisaggregate = recode(otherdisaggregate,
                                 "NON-PEPFAR Supported Site" = "non-PEPFAR",
                                 "PEPFAR Supported Site" = "PEPFAR"),
         filter = case_when(indicator %in% snapshot_indicators & !(month_name %in% last_month) ~ "DEL", TRUE ~ "KEEP"), # try changing this using subset function
         otherdisaggregate = case_when(indicator %in% site_type_indicators ~ as.character(otherdisaggregate))) %>%
  filter(filter == "KEEP") %>%
  select(reportingperiod, country, contains("snu"), indicator, otherdisaggregate, population, numdenom, value) %>%
  dplyr::group_by(reportingperiod, country, snu_1, snu_2, snu_3, snu_4, snu_1_id, snu_2_id, snu_3_id, snu_4_id, indicator, otherdisaggregate, population, numdenom) %>%
  dplyr::summarise(value = sum(value), .groups = "drop")

#Age and Sex cleaning

age_sex_counts <- read.csv(age_sex_counts)

age_sex_counts_clean <- age_sex_counts %>% clean_names() %>%
  dplyr::rename(indicator = data_element_short_name, otherdisaggregate = pepfar, age = all_age_groups_for_datim_entry) %>%
  unite(reportingperiod, c("fiscal_year_short_name","fiscal_quarter"), sep = " Q") %>%
  separate(indicator, c("indicator", "numdenom"), sep = "[ ]") %>%
  mutate(numdenom = if_else(numdenom == "(D)", "Denominator", "Numerator"),
         otherdisaggregate = recode(otherdisaggregate,
                                    "NON-PEPFAR Supported Site" = "non-PEPFAR",
                                    "PEPFAR Supported Site" = "PEPFAR"),
         otherdisaggregate = case_when(indicator %in% site_type_indicators ~ as.character(otherdisaggregate)),
         age = recode(age,
                      "<1" = "<01",
                      "1-4" = "01-04",
                      "5-9" = "05-09",
                      "Age Unknown" = "Unknown Age"),
         age = if_else(indicator %in% indicators_less_than_20 & age %in% less_than_20, "<20", as.character(age))) %>%
  select(reportingperiod, country, contains("snu"), indicator, sex, age, otherdisaggregate, numdenom, value)%>%
  dplyr::group_by(reportingperiod, country,  snu_1, snu_2, snu_3, snu_4, snu_1_id, snu_2_id, snu_3_id, snu_4_id, indicator, sex, age, otherdisaggregate, numdenom) %>%
  dplyr::summarise(value = sum(value), .groups = "drop")

age_sex_snapshot1 <- read.csv(age_sex_snapshots[1])
age_sex_snapshot2 <- read.csv(age_sex_snapshots[2])
age_sex_snapshot <- bind_rows(age_sex_snapshot1, age_sex_snapshot2)

age_sex_snapshot_clean <- age_sex_snapshot %>% clean_names() %>%
  dplyr::rename(indicator = data_element_short_name, otherdisaggregate = pepfar, age = all_age_groups_for_datim_entry) %>%
  unite(reportingperiod, c("fiscal_year_short_name","fiscal_quarter"), sep = " Q") %>%
  separate(indicator, c("indicator", "numdenom"), sep = "[ ]") %>%
  mutate(numdenom = if_else(numdenom == "(D)", "Denominator", "Numerator"),
         otherdisaggregate = recode(otherdisaggregate,
                                    "NON-PEPFAR Supported Site" = "non-PEPFAR",
                                    "PEPFAR Supported Site" = "PEPFAR"), 
         filter = case_when(indicator %in% snapshot_indicators & !(month_name %in% last_month) ~ "DEL",
                            TRUE ~ "KEEP"), # try changing this using subset function, keep this here
         otherdisaggregate = case_when(indicator %in% site_type_indicators ~ as.character(otherdisaggregate)),
         age = recode(age,
                      "<1" = "<01",
                      "1-4" = "01-04",
                      "5-9" = "05-09",
                      "Age Unknown" = "Unknown Age"),
         age = if_else(indicator %in% indicators_less_than_20 & age %in% less_than_20, "<20", as.character(age))) %>%
  filter(filter == "KEEP") %>%
  select(reportingperiod, country, contains("snu"), indicator, sex, age, otherdisaggregate, numdenom, value)%>%
  dplyr::group_by(reportingperiod, country,  snu_1, snu_2, snu_3, snu_4, snu_1_id, snu_2_id, snu_3_id, snu_4_id, indicator, sex, age, otherdisaggregate, numdenom) %>%
  dplyr::summarise(value = sum(value), .groups = "drop")


#merge all three files
complete_clean_data_pre_mech <- bind_rows(age_sex_counts_clean, age_sex_snapshot_clean, kp_disaggs_counts_clean) %>% 
  mutate(country = recode(country, "Cote dIvoire" = "Cote d'Ivoire"),
         indicator = recode(indicator, "TX_PVLS_ELIGIBLE_VERIFY" = "TX_PVLS_ELIGIBLE")) %>%
  relocate(population, .after = "otherdisaggregate")

#merge with mech ref file


complete_clean_data_pre_mech %>%  select(country) %>% group_by_all() %>% summarise(.groups = "drop") %>% 
  left_join(mech_id_ref_table_fy23, by = "country") %>% glimpse() %>% print(n=23)

#overwrite df after merge
complete_clean_data <- left_join(complete_clean_data_pre_mech, mech_id_ref_table_fy23, by = "country") %>% 
  relocate(mech_code:mech_name, .after = "reportingperiod") %>%   
  mutate(
         # snu_4 = str_to_title(snu_4),
         # snu_3 = str_to_title(snu_3),
         # snu_2 = str_to_title(snu_2),
         snu_4  = str_replace(snu_4, "  ", " "),
         snu_3  = str_replace(snu_3, "  ", " "),
         snu_2  = str_replace(snu_2, "  ", " ")) %>%
  clean_names() %>% 
  group_by(across(-c(value))) %>% summarise(value = sum(value), .groups = "drop") %>% print()


#data validation with infolink
#kp data
country_kp_count <- complete_clean_data %>% filter(population %in% keypop & indicator %in% c("TX_NEW_VERIFY","TX_RTT_VERIFY","PrEP_OFFER","PrEP_NEW_VERIFY","PrEP_CT_VERIFY")) %>% 
  dplyr::group_by(reportingperiod, country,  indicator, ) %>% dplyr::summarize(value = sum(value), .groups = "drop")

country_kp_snapshot <- complete_clean_data %>% filter(population %in% keypop & indicator %in% c("TX_CURR_VERIFY", "TX_PVLS_VERIFY", "TX_PVLS_ELIGIBLE")) %>% 
  dplyr::group_by(reportingperiod, country, indicator, numdenom) %>% dplyr::summarize(value = sum(value), .groups = "drop")

#non-kp data
country_age_sex_count <- complete_clean_data %>% filter(!(population %in% keypop) & indicator == "TX_NEW_VERIFY") %>% 
  dplyr::group_by(reportingperiod, country, indicator) %>% dplyr::summarize(value = sum(value), .groups = "drop")

country_age_sex_snapshot <- complete_clean_data %>% filter(!(population %in% keypop) & indicator == "TX_CURR_VERIFY") %>% 
  dplyr::group_by(reportingperiod, country, indicator, numdenom) %>% dplyr::summarize(value = sum(value), .groups = "drop")

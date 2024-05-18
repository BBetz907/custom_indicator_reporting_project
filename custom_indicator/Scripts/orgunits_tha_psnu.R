#psnu level 4 (country level)
table(tha$orgunit_level)

psnu_level <- orgunit_levels %>% filter(country_iso == "THA")
lev <- (psnu_level$prioritization[1]:max(tha$orgunit_level))

tha_orgs_clean <- orgunit_clean(df_orgs$tha_orgs)
tha_org_levels <- map(.x = lev, .f = ~orgunit_level_sep(tha_orgs_clean, .x))

tha_orgunit_table <- reduce(tha_org_levels, full_join)

#merge with data
tha_7_clean <- tha %>% filter(orgunit_level == 7) %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)
tha_5_clean <- tha %>% filter(orgunit_level == 5) %>% rename(orgunit_4 = orgunit_parent, orgunit_4_uid = orgunit_parent_uid)

tha_7_merge_psnu <- left_join(tha_7_clean, tha_orgunit_table, by = join_by(orgunituid  == orgunit_7_uid, orgunit_6_uid  == orgunit_6_uid), multiple = "all", relationship = "many-to-one") %>% 
  select(-c(contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_4"), contains("orgunit_3"))) %>% 
  # distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)


tha_5_merge_psnu <- left_join(tha_5_clean, distinct(tha_orgunit_table[1:6]), by = join_by(orgunituid == orgunit_5_uid), multiple = "all", relationship = "many-to-one") %>% 
  select(-c(contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_5"), contains("orgunit_4"), contains("orgunit_3"))) %>% 
  # distinct() %>%
  mutate(psnu = orgunit, psnu_uid = orgunituid)

tha_merge_psnu <- bind_rows(tha_7_merge_psnu, tha_5_merge_psnu)


nrow(tha) - nrow(tha_merge_psnu)

#psnu level 5
table(mmr$orgunit_level)

psnu_level <- orgunit_levels %>% filter(country_iso == "MMR")
lev <- (psnu_level$prioritization[1]:max(mmr$orgunit_level))

mmr_orgs_clean <- orgunit_clean(df_orgs$mmr_orgs)
mmr_org_levels <- map(.x = lev, .f = ~orgunit_level_sep(mmr_orgs_clean, .x))

mmr_orgunit_table <- reduce(mmr_org_levels, full_join)

#merge with data
mmr_8_clean <- mmr %>% filter(orgunit_level == 8) %>% rename(orgunit_7 = orgunit_parent, orgunit_7_uid = orgunit_parent_uid)
mmr_7_clean <- mmr %>% filter(orgunit_level == 7) %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)

mmr_8_merge <- left_join(mmr_8_clean, mmr_orgunit_table, by = join_by(orgunituid == orgunit_8_uid, orgunit == orgunit_8), multiple = "all", relationship = "many-to-one") %>% 
  select(-c(contains("orgunit_8"), contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_4"))) %>% 
  # distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)
mmr_7_merge <- left_join(mmr_7_clean, distinct(mmr_orgunit_table[1:8]), by = join_by(orgunituid == orgunit_7_uid, orgunit == orgunit_7), multiple = "all", relationship = "many-to-many") %>% 
  select(-c(contains("orgunit_8"), contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_4"))) %>% 
  # distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)

mmr_merge_psnu <- bind_rows(mmr_8_merge, mmr_7_merge)

nrow(mmr) - nrow(mmr_merge_psnu)

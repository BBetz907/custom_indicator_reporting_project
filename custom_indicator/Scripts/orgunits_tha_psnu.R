#psnu level 4 (country level)
table(tha$orgunit_level)

psnu_level <- orgunit_levels %>% filter(country_iso == "THA")
lev <- (psnu_level$prioritization[1]:max(tha$orgunit_level))

tha_orgs_clean <- orgunit_clean(df_orgs$tha_orgs)
tha_org_levels <- map(.x = lev, .f = ~orgunit_level_sep(tha_orgs_clean, .x))

tha_orgunit_table <- reduce(tha_org_levels, full_join)

#merge with data
tha_clean <- tha %>% filter(orgunit_level == 7) %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)

tha_merge_psnu <- left_join(tha_clean, tha_orgunit_table, by = join_by(orgunituid == orgunit_7_uid, orgunit == orgunit_7), multiple = "all", relationship = "many-to-one") %>% 
  select(-c(contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_5"), contains("orgunit_3"))) %>% distinct() %>%
  rename(psnu = orgunit_4, psnu_uid = orgunit_4_uid)

nrow(tha_clean) - nrow(tha_merge_psnu)

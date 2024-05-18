#psnu level 5
table(kaz$orgunit_level)

psnu_level <- orgunit_levels %>% filter(country_iso == "KAZ")
lev <- (psnu_level$prioritization[1]:max(kaz$orgunit_level))

kaz_orgs_clean <- orgunit_clean(df_orgs$kaz_orgs)
kaz_org_levels <- map(.x = lev, .f = ~orgunit_level_sep(kaz_orgs_clean, .x))

kaz_orgunit_table <- reduce(kaz_org_levels, full_join)

#merge with data
kaz_clean <- kaz %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)

kaz_merge_psnu <- left_join(kaz_clean, kaz_orgunit_table, by = join_by(orgunituid == orgunit_6_uid, orgunit == orgunit_6), multiple = "all", relationship = "many-to-one") %>% 
  select(-c(contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_4"))) %>% 
  # distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)

nrow(kaz) - nrow(kaz_merge_psnu)

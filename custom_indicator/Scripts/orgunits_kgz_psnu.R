#psnu level 5
table(kgz$orgunit_level)

#transform and create table
psnu_level <- orgunit_levels %>% filter(country_iso == "KGZ")
lev <- (psnu_level$prioritization[1]:max(kgz$orgunit_level))

kgz_orgs_clean <- orgunit_clean(df_orgs$kgz_orgs)
kgz_org_levels <- map(.x = lev, .f = ~orgunit_level_sep(kgz_orgs_clean, .x))

kgz_orgunit_table <- reduce(kgz_org_levels, full_join)

#merge with data
kgz_clean <- kgz %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)

kgz_merge_psnu <- left_join(kgz_clean, kgz_orgunit_table, by = join_by(orgunituid == orgunit_6_uid, orgunit == orgunit_6), multiple = "all", relationship = "many-to-one") %>% 
  select(-c(contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_4"))) %>% 
  # distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)

nrow(kgz_clean) - nrow(kgz_merge_psnu)

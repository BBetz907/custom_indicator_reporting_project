#psnu level 5
table(gha$orgunit_level)

psnu_level <- orgunit_levels %>% filter(country_iso == "GHA")
lev <- (psnu_level$prioritization[1]:max(gha$orgunit_level))

gha_orgs_clean <- orgunit_clean(df_orgs$gha_orgs)
gha_org_levels <- map(.x = lev, .f = ~orgunit_level_sep(gha_orgs_clean, .x))

gha_orgunit_table <- reduce(gha_org_levels, full_join)

#merge with data
gha_clean <- gha %>% rename(orgunit_7 = orgunit_parent, orgunit_7_uid = orgunit_parent_uid)

gha_merge_psnu <- left_join(gha_clean, gha_orgunit_table, by = join_by(orgunituid == orgunit_8_uid, orgunit == orgunit_8), multiple = "all", relationship == "many-to-one") %>% 
  select(-c(contains("orgunit_8"), contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_4"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)

nrow(gha) - nrow(gha_merge_psnu)

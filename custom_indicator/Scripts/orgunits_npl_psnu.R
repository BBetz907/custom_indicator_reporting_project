#psnu level 5
table(npl$orgunit_level)

psnu_level <- orgunit_levels %>% filter(country_iso == "NPL")
lev <- (psnu_level$prioritization[1]:max(npl$orgunit_level))

npl_orgs_clean <- orgunit_clean(df_orgs$npl_orgs)
npl_org_levels <- map(.x = lev, .f = ~orgunit_level_sep(npl_orgs_clean, .x))

npl_orgunit_table <- reduce(npl_org_levels, full_join)

#merge with data
npl_7_clean <- npl %>% filter(orgunit_level == 7) %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)
npl_6_clean <- npl %>% filter(orgunit_level == 6) %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

npl_merge_psnu <- left_join(npl_7_clean, npl_orgunit_table, by = join_by(orgunituid == orgunit_7_uid, orgunit == orgunit_7), multiple = "all", relationship = "many-to-one") %>% 
  select(-c(contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_4"))) %>% 
  # distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid) %>%
  bind_rows(npl_6_clean)

nrow(npl) - nrow(npl_merge_psnu)

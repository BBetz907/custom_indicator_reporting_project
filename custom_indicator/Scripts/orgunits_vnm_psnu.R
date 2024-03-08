# psnu level 4
table(vnm$orgunit_level)

psnu_level <- orgunit_levels %>% filter(country_iso == "VNM")
lev <- (psnu_level$prioritization[1]:max(vnm$orgunit_level))

vnm_orgs_clean <- orgunit_clean(df_orgs$vnm_orgs)
vnm_org_levels <- map(.x = lev, .f = ~orgunit_level_sep(vnm_orgs_clean, .x))

vnm_orgunit_table <- reduce(vnm_org_levels, full_join)

#merge with data
vnm_6_clean <- vnm %>% filter(orgunit_level == 6) %>% rename(orgunit_5 = orgunit_parent, orgunit_5_uid = orgunit_parent_uid)
vnm_5_clean <- vnm %>% filter(orgunit_level == 5) %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

vnm_6_merge <- left_join(vnm_6_clean, vnm_orgunit_table, by = join_by(orgunit_5_uid, orgunit_5), multiple = "all", relationship = "many-to-many") %>% 
  select(-c(contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_5"), contains("orgunit_3"))) %>% distinct() %>%
  rename(psnu = orgunit_4, psnu_uid = orgunit_4_uid)

vnm_merge_psnu <- bind_rows(vnm_6_merge, vnm_5_clean)
nrow(vnm_merge_psnu) - nrow(vnm_info)

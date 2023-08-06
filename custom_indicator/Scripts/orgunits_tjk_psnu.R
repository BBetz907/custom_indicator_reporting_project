#psnu level 5
table(tjk$orgunit_level)

#transform and create table
psnu_level <- orgunit_levels %>% filter(country_iso == "TJK")
lev <- (psnu_level$prioritization[1]:max(tjk$orgunit_level))

tjk_orgs_clean <- orgunit_clean(df_orgs$tjk_orgs)
tjk_org_levels <- map(.x = lev, .f = ~orgunit_level_sep(tjk_orgs_clean, .x))

tjk_orgunit_table <- reduce(tjk_org_levels, full_join)


#merge with data
tjk_clean <- tjk %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)

tjk_merge_psnu <- left_join(tjk_clean, tjk_orgunit_table, by = join_by(orgunituid == orgunit_7_uid, orgunit == orgunit_7), multiple = "all", relationship = "many-to-one") %>% 
  select(-c(contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_4"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)

nrow(tjk_clean) - nrow(tjk_merge_psnu)

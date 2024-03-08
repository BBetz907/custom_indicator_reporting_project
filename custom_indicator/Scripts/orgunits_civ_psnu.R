#psnu level 4
table(civ$orgunit_level) #contains both 6 and 7
psnu_level <- orgunit_levels %>% filter(country_iso == "CIV")
lev <- (psnu_level$prioritization[1]:max(civ$orgunit_level))

civ_orgs_clean <- orgunit_clean(df_orgs$civ_orgs)
civ_org_levels <- map(.x = lev, .f = ~orgunit_level_sep(civ_orgs_clean, .x))

civ_orgunit_table <- reduce(civ_org_levels, full_join)

#merge with data
civ_7_clean <- civ %>% filter(orgunit_level == 7) %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)
civ_6_clean <- civ %>% filter(orgunit_level == 6) %>% rename(orgunit_5 = orgunit_parent, orgunit_5_uid = orgunit_parent_uid)

civ_7_merge <- left_join(civ_7_clean, civ_orgunit_table, by = join_by(orgunituid == orgunit_7_uid, orgunit == orgunit_7), multiple = "all", relationship = "many-to-one") %>% 
  select(-c(contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_5"), contains("orgunit_3"))) %>% distinct() %>%
  rename(psnu = orgunit_4, psnu_uid = orgunit_4_uid)
civ_6_merge <- left_join(civ_6_clean, civ_orgunit_table, by = join_by(orgunituid == orgunit_6_uid, orgunit == orgunit_6), multiple = "all", relationship = "many-to-many") %>% 
  select(-c(contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_5"), contains("orgunit_3"))) %>% distinct() %>%
  rename(psnu = orgunit_4, psnu_uid = orgunit_4_uid)

civ_merge_psnu <- bind_rows(civ_7_merge, civ_6_merge)

nrow(civ) - nrow(civ_merge_psnu)


# future: replacing lines 6-9
# civ_orgunit_table <- reduce(map(.x = lev, .f = ~orgunit_level_sep(orgunit_clean(df_orgs$civ_orgs), .x)), full_join)
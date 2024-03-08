#psnu level 5
#transform and create table
psnu_level <- orgunit_levels %>% filter(country_iso == "LBR")
lev <- (psnu_level$prioritization[1]:max(lbr$orgunit_level))

lbr_orgs_clean <- orgunit_clean(df_orgs$lbr_orgs)
lbr_org_levels <- map(.x = lev, .f = ~orgunit_level_sep(lbr_orgs_clean, .x))

lbr_orgunit_table <- reduce(lbr_org_levels, full_join)

#merge with data
lbr_7_clean <- lbr %>% filter(orgunit_level == 7) %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)
lbr_6_clean <- lbr %>% filter(orgunit_level == 6) %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

lbr_merge_psnu <- left_join(lbr_7_clean, lbr_orgunit_table, by = join_by(orgunituid == orgunit_7_uid, orgunit == orgunit_7), multiple = "all", relationship = "many-to-one") %>% 
  select(-c(contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_4"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid) %>%
  bind_rows(lbr_6_clean)

nrow(lbr) - nrow(lbr_merge_psnu)

#psnu level 5
table(mli$orgunit_level)

#transform and create table
psnu_level <- orgunit_levels %>% filter(country_iso == "MLI")
lev <- (psnu_level$prioritization[1]:max(mli$orgunit_level))

mli_orgs_clean <- orgunit_clean(df_orgs$mli_orgs)
mli_org_levels <- map(.x = lev, .f = ~orgunit_level_sep(mli_orgs_clean, .x))

mli_orgunit_table <- reduce(mli_org_levels, full_join)

#merge with data
mli_clean <- mli %>% rename(orgunit_6 = orgunit_parent, orgunit_6_uid = orgunit_parent_uid)

mli_merge_psnu <- left_join(mli_clean, mli_orgunit_table, by = join_by(orgunituid == orgunit_6_uid, orgunit == orgunit_6), multiple = "all", relationship = "many-to-one") %>% 
  select(-c(contains("orgunit_7"), contains("orgunit_6"), contains("orgunit_4"))) %>% distinct() %>%
  rename(psnu = orgunit_5, psnu_uid = orgunit_5_uid)

nrow(mli_clean) - nrow(mli_merge_psnu)
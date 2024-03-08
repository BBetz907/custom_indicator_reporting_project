#psnu level 4
psnu_level <- orgunit_levels %>% filter(country_iso == "LAO")
lev <- (psnu_level$prioritization[1]:max(lao$orgunit_level))

lao_orgs_clean <- orgunit_clean(df_orgs$lao_orgs)
lao_org_levels <- map(.x = lev, .f = ~orgunit_level_sep(lao_orgs_clean, .x))

lao_orgunit_table <- reduce(lao_org_levels, full_join)

lao_merge_psnu <- lao %>% mutate(psnu = first(lao_orgunit_table$orgunit_4), psnu_uid = first(lao_orgunit_table$orgunit_4_uid)) %>%
  select(-c(contains("parent")))

#data validation with infolink
lao_data_check <- lao_merge_psnu %>% group_by(psnu, psnu_uid, indicator, age, sex, population) %>% summarize(value = sum(value))

# read_csv("https://www.datim.org/api/sqlViews/W5RIpSXU1DP/data.csv")
# check psnu ref tables, sometimes wrong?

nrow(lao) - nrow(lao_merge_psnu)

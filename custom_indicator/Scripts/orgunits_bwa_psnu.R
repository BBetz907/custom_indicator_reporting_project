#psnu level 5
table(bwa$orgunit_level)
 
# bwa_orgs_clean <- orgunit_clean(df_orgs$bfa_orgs)
# 
# bwa_level_5 <- orgunit_level_sep(bwa_orgs_clean , 5, orgunit_4, orgunit_4_uid, orgunit_5, orgunit_5_uid)
# 
# bwa_level_6 <- orgunit_level_sep(bwa_orgs_clean , 6, orgunit_5, orgunit_5_uid, orgunit_6, orgunit_6_uid)
# 
# bwa_orgunit_table <- orgunit_table_join(bwa_level_5, bwa_level_6, orgunit_5_uid, orgunit_5)

bwa_merge_psnu <- bwa %>% rename(psnu = orgunit_parent, psun_uid = orgunit_parent_uid)

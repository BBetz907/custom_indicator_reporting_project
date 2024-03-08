#psnu level 5
table(bfa$orgunit_level)

bfa_merge_psnu <- bfa %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

#data validation
bfa_data_check <- data_check(bfa_merge_psnu)

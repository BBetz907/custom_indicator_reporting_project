#psnu level 5
table(tgo$orgunit_level)

tgo_merge_psnu <- tgo %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

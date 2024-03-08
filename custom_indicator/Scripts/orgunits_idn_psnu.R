#psnu level 6
table(idn$orgunit_level)

idn_merge_psnu <- idn %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

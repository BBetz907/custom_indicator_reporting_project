#psnu level 4
table(lso$orgunit_level)

lso_merge_psnu <- lso %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

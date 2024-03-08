#psnu level 5, need to double check the psnu level
table(cod$orgunit_level)

#psnu level 5 but has level 4 and 5 in the data
cod_merge_psnu <- cod %>% mutate(psnu = orgunit, psnu_uid = orgunituid) %>% select(-(contains("parent")))

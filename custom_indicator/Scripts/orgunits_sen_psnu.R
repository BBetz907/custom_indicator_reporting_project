#psnu level 5
table(sen$orgunit_level)

sen_merge_psnu <- sen %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

#data check with infolink
sen_data_check <- sen_merge_psnu %>% group_by(psnu, psnu_uid, indicator) %>% summarize(value = sum(value))

#duplication with prep_offer
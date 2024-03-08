#psnu level 5
table(zaf$orgunit_level)

zaf_merge_psnu <- zaf %>% mutate(psnu = orgunit, psnu_uid = orgunituid) %>% select(-c(contains("parent"))) %>% distinct()

#data check with infolink
zaf_data_check <- zaf_merge_psnu %>% group_by(psnu, psnu_uid, indicator, age, sex, population) %>% summarize(value = sum(value))
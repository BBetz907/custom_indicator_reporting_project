#only has level 6 and prioritization level is 6
table(mwi$orgunit_level)

mwi_merge_psnu <- mwi %>% mutate(psnu = orgunit_parent, psnu_uid = orgunituid) %>% select(-c(contains("orgunit_parent")))
# nrow(mwi_merge_psnu)

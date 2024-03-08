#psnu level 4
table(bdi$orgunit_level)

#merge with data

bdi_merge_psnu <- bdi %>% rename(psnu = orgunit_parent, psnu_uid = orgunit_parent_uid)

#data validation

#bdi_data_check <- data_check(bdi_merge_psnu)

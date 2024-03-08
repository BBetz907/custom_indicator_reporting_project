#psnu level 4
table(phl$orgunit_level)

phl_merge_psnu <- phl %>% mutate(psnu = "Philippines", psnu_uid = "p1E1K4MWGpa") %>% select(-c(contains("parent")))
#psnu level 4 is the country level so check that this is correct

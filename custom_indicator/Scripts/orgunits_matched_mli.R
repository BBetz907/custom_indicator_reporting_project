
# obtain metadata from infolink -------------------------------------------



# user purr to create DF for each country, named after each count --------
mli_info <- complete_clean_data %>% filter(country=="Mali") %>%
  # mutate(snu_2 = recode(snu_2,
  #                       "" = "")
  #   # snu_2 = str_to_title(snu_2),
  # ) %>%
glimpse()

# snu4 <- c(unique(mli_info$snu_4)) %>% print()
# snu3 <- c(unique(mli_info$snu_3)) %>% print()
snu2 <- c(unique(mli_info$snu_2)) %>% print() 
snu1 <- c(unique(mli_info$snu_1)) %>% print()

# get orgunit levels to match and join ------------------------------------
mli7op <- mli_6_7 %>% filter(orgunit_level  == "7") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name)
mli7uid <- c(mli7op$orgunit_uid)
unique(mli7op$orgunit_name)

mli6op <- mli_6_7 %>% filter(orgunit_level  == "6") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
mli6uid <- c(mli6op$orgunit_uid)
unique(mli6op$orgunit_name)


mli_6_7 %>% filter(orgunit_name %in% snu2) #snu1 should match datim level 7
mli_6_7 %>% filter(orgunit_name %in% snu1) #snu1 should match datim level 6
#test the other way in the future

################################################################################



# for most level 2 that match_level 7, use snu_2 id-----------------------
mli7 <- mli_info %>% filter(snu_2_id %in% mli7uid, snu_2!="") %>% select(-snu_1_id) %>% 
  rename(orgunit_uid = snu_2_id)  %>% inner_join(mli7op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(mli7)/nrow(mli_info))

# for level 2 that doesn't match level 7, match by snu_2 name --------
mli7m1 <- mli_info %>% filter(!snu_2_id %in% mli7uid, snu_2 != "") %>% rename(orgunit_name = snu_2)
scales::percent(nrow(mli7m1)/nrow(mli_info))
nrow(mli7m1)

#identify and resolve any failed matches
non_matched_snu_2 <- mli7m1 %>%
  anti_join(mli7op) %>% glimpse()
##resolve discrepancies
# non_matched_snu_2_name <- unique(non_matched_snu_2$orgunit_name) %>% print()
# test1 <- mli7op %>% select(orgunit_name) %>%
#   filter(str_detect(orgunit_name, "Leprosy")) %>% arrange((orgunit_name))
# unique(test1$orgunit_name)
#unable to find one facility (match level 1)


#now match
mli7m <- mli7m1 %>% inner_join(mli7op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_2_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(mli7m)
scales::percent(nrow(mli7m)/nrow(mli_info))

#check for 1:many matches
mli7m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)


#check for unmatched
mli7m %>% filter(is.na(orgunituid)) 


##############################################################################

# check for missing data at snu_level_2
mli6 <- mli_info %>% filter(snu_2 == "", snu_1 == "") %>% print()

##############################################################################

# # for snu2 2 that do not match_level 7, use snu_1 id-----------------------
# mli6 <- mli_info %>% filter(snu_1_id %in% mli6uid, snu_2 %in% non_matched_snu_2_name) %>% select(-snu_1_id) %>% 
#   rename(orgunit_uid = snu_2_id)  %>% inner_join(mli7op) %>%  
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>% 
# scales::percent(nrow(mli6)/nrow(mli_info))
# 
# # for snu2 that doesn't match level 7, match by snu_1 name --------
# mli6m1 <- mli_info %>% filter(!snu_2_id %in% mli7uid, snu_2 %in% non_matched_snu_2_name) %>% rename(orgunit_name = snu_1) %>% glimpse()
# scales::percent(nrow(mli6m1)/nrow(mli_info))
# nrow(mli6m1)
# 
# #identify and resolve any failed matches
# non_matched_snu_1 <- mli6m1 %>%
#   anti_join(mli6op) %>% glimpse()
# 
# 
# #now match
# mli6m <- mli6m1 %>% inner_join(mli6op) %>% # or inner if there are non-matches 
#   select(-snu_1_id) %>%
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
#   glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
# nrow(mli6m)
# scales::percent(nrow(mli6m)/nrow(mli_info))
# 
# #check for 1:many matches
# mli6m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
#   filter(n()>1)
# 
# 
# #check for unmatched
# mli6m %>% filter(is.na(orgunituid)) 

###############################################################################


mli <- bind_rows(mli7, mli7m) %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(mli) - nrow(mli_info)


#later bind country dfs together

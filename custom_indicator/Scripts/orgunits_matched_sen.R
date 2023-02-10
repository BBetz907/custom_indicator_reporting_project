
# obtain metadata from infolink -------------------------------------------



# user purr to create DF for each country, named after each count --------
sen_info <- complete_clean_data %>% filter(country=="Senegal") %>%
  # mutate(snu_2 = recode(snu_2,
  #                       "" = "")
  #   # snu_2 = str_to_title(snu_2),
  # ) %>%
glimpse()

# snu4 <- c(unique(sen_info$snu_4)) %>% print()
# snu3 <- c(unique(sen_info$snu_3)) %>% print()
snu2 <- c(unique(sen_info$snu_2)) %>% print() 
snu1 <- c(unique(sen_info$snu_1)) %>% print()

# get orgunit levels to match and join ------------------------------------
sen6op <- sen_6_6 %>% filter(orgunit_level  == "6") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name)
sen6uid <- c(sen6op$orgunit_uid)
unique(sen6op$orgunit_name)

sen6op <- sen_6_6 %>% filter(orgunit_level  == "6") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
sen6uid <- c(sen6op$orgunit_uid)
unique(sen6op$orgunit_name)


sen6op %>% filter(orgunit_name %in% snu2) #snu2 should match datim level 6
#test the other way in the future

################################################################################



# for most level 2 that match_level 6, use snu_2 id-----------------------
sen6 <- sen_info %>% filter(snu_2_id %in% sen6uid, snu_2!="") %>% select(-snu_1_id) %>% 
  rename(orgunit_uid = snu_2_id)  %>% inner_join(sen6op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(sen6)/nrow(sen_info))

# for level 2 that doesn't match level 6, match by snu_2 name --------
sen6m1 <- sen_info %>% filter(!snu_2_id %in% sen6uid, snu_2 != "") %>% rename(orgunit_name = snu_2)
scales::percent(nrow(sen6m1)/nrow(sen_info))
nrow(sen6m1)

#identify and resolve any failed matches
non_matched_snu_2 <- sen6m1 %>%
  anti_join(sen6op) %>% glimpse()
##resolve discrepancies
# non_matched_snu_2_name <- unique(non_matched_snu_2$orgunit_name) %>% print()
# test1 <- sen6op %>% select(orgunit_name) %>%
#   filter(str_detect(orgunit_name, "Leprosy")) %>% arrange((orgunit_name))
# unique(test1$orgunit_name)
#unable to find one facility (match level 1)


#now match
sen6m <- sen6m1 %>% inner_join(sen6op) %>%  # or inner if there are non-matches 
  select(-snu_1_id:-snu_2_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(sen6m)
scales::percent(nrow(sen6m)/nrow(sen_info))

#check for 1:many matches
sen6m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)


#check for unmatched
sen6m %>% filter(is.na(orgunituid)) 


##############################################################################

# check for missing data at snu_level_2
sen_info %>% filter(snu_2 == "", snu_1 == "") %>% print()

##############################################################################

# # for snu2 2 that do not match_level 7, use snu_1 id-----------------------
# sen6 <- sen_info %>% filter(snu_1_id %in% sen6uid, snu_2 %in% non_matched_snu_2_name) %>% select(-snu_1_id) %>% 
#   rename(orgunit_uid = snu_2_id)  %>% inner_join(sen7op) %>%  
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>% 
# scales::percent(nrow(sen6)/nrow(sen_info))
# 
# # for snu2 that doesn't match level 7, match by snu_1 name --------
# sen6m1 <- sen_info %>% filter(!snu_2_id %in% sen7uid, snu_2 %in% non_matched_snu_2_name) %>% rename(orgunit_name = snu_1) %>% glimpse()
# scales::percent(nrow(sen6m1)/nrow(sen_info))
# nrow(sen6m1)
# 
# #identify and resolve any failed matches
# non_matched_snu_1 <- sen6m1 %>%
#   anti_join(sen6op) %>% glimpse()
# 
# 
# #now match
# sen6m <- sen6m1 %>% inner_join(sen6op) %>% # or inner if there are non-matches 
#   select(-snu_1_id) %>%
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
#   glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
# nrow(sen6m)
# scales::percent(nrow(sen6m)/nrow(sen_info))
# 
# #check for 1:many matches
# sen6m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
#   filter(n()>1)
# 
# 
# #check for unmatched
# sen6m %>% filter(is.na(orgunituid)) 

###############################################################################


sen <- bind_rows(sen6, sen6m) %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(sen) - nrow(sen_info)


#later bind country dfs together

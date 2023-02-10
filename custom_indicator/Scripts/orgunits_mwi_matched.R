
# obtain metadata from infolink -------------------------------------------



# user purr to create DF for each country, named after each count --------
mwi_info <- complete_clean_data %>% filter(country=="Malawi") %>%
  mutate(snu_3 = str_c(snu_3, "District", sep = " ")
    # snu_3 = str_to_title(snu_3),
  ) %>%
glimpse()

# snu4 <- c(unique(mwi_info$snu_4)) %>% print()
snu3 <- c(unique(mwi_info$snu_3)) %>% print()
snu2 <- c(unique(mwi_info$snu_2)) %>% print()
snu1 <- c(unique(mwi_info$snu_1)) %>% print()


# get orgunit levels to match and join ------------------------------------

mwi7op <- mwi_5_7 %>% filter(orgunit_level  == "7") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
mwi7uid <- c(mwi7op$orgunit_uid)
mwi7name <- unique(c(mwi7op$orgunit_name)) %>% print()

mwi6op <- mwi_5_7 %>% filter(orgunit_level  == "6") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
mwi6uid <- c(mwi6op$orgunit_uid)
mwi6name <- unique(c(mwi6op$orgunit_name)) %>% print()

mwi5op <- mwi_5_7 %>% filter(orgunit_level  == "5") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
mwi5uid <- c(mwi5op$orgunit_uid)
mwi5name <- unique(c(mwi5op$orgunit_name)) %>% print()

mwi7op %>% filter(orgunit_name %in% snu4) #snu4 should match datim level 7 for most
mwi6op %>% filter(orgunit_name %in% snu4) #snu3 should match datim level 6 for some (H...)


################################################################################
nrow(mwi_info)
mwi <- mwi_info %>% filter(snu_3 %in% mwi6name) %>% 
  rename(orgunit_name = snu_3) %>%
  inner_join(mwi6op) %>% 
  rename(orgunit = orgunit_name, orgunituid = orgunit_uid) %>% 
  select(-contains("snu"), -orgunit_parent) %>%
  mutate(orgunit_parent = orgunit) %>%
  glimpse()


#check for 1:many matches
mwi %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>%
  filter(n()>1)

################################################################################
# 
# 
# 
# # for most level 3 that match_level 6, use snu_3 id-----------------------
# mwi6 <- mwi_info %>% filter(snu_3_id %in% mwi6uid, snu_3!="") %>% select(-snu_1_id) %>% 
#   rename(orgunit_uid = snu_3_id)  %>% inner_join(mwi6op) %>%  
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
# scales::percent(nrow(mwi6)/nrow(mwi_info))
# 
# # for level 3 that doesn't match level 6, match by snu_3 name --------
# mwi6m1 <- mwi_info %>% filter(!snu_3_id %in% mwi6uid, snu_3 != "") %>% rename(orgunit_name = snu_3)
# scales::percent(nrow(mwi6m1)/nrow(mwi_info))
# nrow(mwi6m1)
# 
# #identify and resolve any failed matches
# non_matched_snu_3 <- mwi6m1 %>%
#   anti_join(mwi6op) %>% glimpse()
# ##resolve discrepancies
# # non_matched_snu_3_name <- unique(non_matched_snu_3$orgunit_name) %>% print()
# # test2 <- mwi6op %>% select(orgunit_name) %>%
# #   filter(str_detect(orgunit_name, "Leprosy")) %>% arrange((orgunit_name))
# # unique(test2$orgunit_name)
# #unable to find one facility (match level 2)
# 
# 
# #now match
# mwi6m <- mwi6m1 %>% inner_join(mwi6op) %>% # or inner if there are non-matches 
#   select(-snu_1_id:-snu_3_id) %>%
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
#   glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
# nrow(mwi6m)
# scales::percent(nrow(mwi6m)/nrow(mwi_info))
# 
# #check for 1:many matches
# mwi6m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
#   filter(n()>1)
# 
# 
# #check for unmatched
# mwi6m %>% filter(is.na(orgunituid)) 
# 
# 
# # ##############################################################################
# # 
# # # check for missing data at snu_level_3
# # mwi_info %>% filter(snu_3 == "", snu_2 == "") %>% print()
# # 
# # ##############################################################################
# # 
# # # for snu3 3 that do not match_level 6, use snu_2 id-----------------------
# # mwi6 <- mwi_info %>% filter(snu_2_id %in% mwi6uid, snu_3 %in% non_matched_snu_3_name) %>% select(-snu_1_id) %>% 
# #   rename(orgunit_uid = snu_3_id)  %>% inner_join(mwi6op) %>%  
# #   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>% 
# # scales::percent(nrow(mwi6)/nrow(mwi_info))
# # 
# # # for snu3 that doesn't match level 6, match by snu_2 name --------
# # mwi6m1 <- mwi_info %>% filter(!snu_3_id %in% mwi6uid, snu_3 %in% non_matched_snu_3_name) %>% rename(orgunit_name = snu_2) %>% glimpse()
# # scales::percent(nrow(mwi6m1)/nrow(mwi_info))
# # nrow(mwi6m1)
# # 
# # #identify and resolve any failed matches
# # non_matched_snu_2 <- mwi6m1 %>%
# #   anti_join(mwi6op) %>% glimpse()
# # 
# # 
# # #now match
# # mwi6m <- mwi6m1 %>% inner_join(mwi6op) %>% # or inner if there are non-matches 
# #   select(-snu_1_id) %>%
# #   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
# #   glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
# # nrow(mwi6m)
# # scales::percent(nrow(mwi6m)/nrow(mwi_info))
# # 
# # #check for 1:many matches
# # mwi6m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
# #   filter(n()>1)
# # 
# # 
# # #check for unmatched
# # mwi6m %>% filter(is.na(orgunituid)) 
# 
# ###############################################################################
# 
# 
# mwi <- bind_rows(mwi6, mwi6m) %>% select(-contains("snu")) %>% 
#   glimpse() 
# #check to see if number of rows matches source
# nrow(mwi) - nrow(mwi_info)
# 
# 
# #later bind country dfs together

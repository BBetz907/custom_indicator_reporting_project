
# obtain metadata from infolink -------------------------------------------



# user purr to create DF for each country, named after each count --------
lso_info <- complete_clean_data %>% filter(country=="Lesotho") %>%
  # mutate(snu_3 = recode(snu_3,
  #                       "" = "")
  #   # snu_3 = str_to_title(snu_3),
  # ) %>%
glimpse()

# snu3 <- c(unique(lso_info$snu_3)) %>% print()
snu2 <- c(unique(lso_info$snu_2)) %>% print()
snu1 <- c(unique(lso_info$snu_1)) %>% print()

# get orgunit levels to match and join ------------------------------------
lso5op <- lso_5_6 %>% filter(orgunit_level  == "5") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
lso5uid <- c(lso5op$orgunit_uid)
unique(lso5op$orgunit_name)


lso5op %>% filter(orgunit_name %in% snu2) #snu2 should match datim level 5


################################################################################



# for most level 3 that match_level 5, use snu_3 id-----------------------
lso5 <- lso_info %>% filter(snu_2_id %in% lso5uid, snu_2!="") %>% select(-snu_1_id) %>% 
  rename(orgunit_uid = snu_2_id)  %>% inner_join(lso5op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(lso5)/nrow(lso_info))

# for level 2 that doesn't match level 5, match by snu_2 name --------
lso5m1 <- lso_info %>% filter(!snu_2_id %in% lso5uid, snu_2 != "") %>% rename(orgunit_name = snu_2)
scales::percent(nrow(lso5m1)/nrow(lso_info))
nrow(lso5m1)

#identify and resolve any failed matches
non_matched_snu_2 <- lso5m1 %>%
  anti_join(lso5op) %>% glimpse()
##resolve discrepancies
# non_matched_snu_2_name <- unique(non_matched_snu_2$orgunit_name) %>% print()
# test2 <- lso5op %>% select(orgunit_name) %>%
#   filter(str_detect(orgunit_name, "Leprosy")) %>% arrange((orgunit_name))
# unique(test2$orgunit_name)
#unable to find one facility (match level 2)


#now match
lso5m <- lso5m1 %>% inner_join(lso5op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_2_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(lso5m)
scales::percent(nrow(lso5m)/nrow(lso_info))

#check for 1:many matches
lso5m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)


#check for unmatched
lso5m %>% filter(is.na(orgunituid)) 


##############################################################################

# check for missing data at snu_level_2
# lso_info %>% filter(snu_2 == "", snu_2 == "") %>% print()

##############################################################################
# 
# # for snu2 2 that do not match_level 5, use snu_2 id-----------------------
# lso5 <- lso_info %>% filter(snu_2_id %in% lso5uid, snu_2 %in% non_matched_snu_2_name) %>% select(-snu_1_id) %>% 
#   rename(orgunit_uid = snu_2_id)  %>% inner_join(lso5op) %>%  
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>% 
# scales::percent(nrow(lso5)/nrow(lso_info))
# 
# # for snu2 that doesn't match level 5, match by snu_2 name --------
# lso5m1 <- lso_info %>% filter(!snu_2_id %in% lso5uid, snu_2 %in% non_matched_snu_2_name) %>% rename(orgunit_name = snu_2) %>% glimpse()
# scales::percent(nrow(lso5m1)/nrow(lso_info))
# nrow(lso5m1)
# 
# #identify and resolve any failed matches
# non_matched_snu_2 <- lso5m1 %>%
#   anti_join(lso5op) %>% glimpse()
# 
# 
# #now match
# lso5m <- lso5m1 %>% inner_join(lso5op) %>% # or inner if there are non-matches 
#   select(-snu_1_id) %>%
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
#   glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
# nrow(lso5m)
# scales::percent(nrow(lso5m)/nrow(lso_info))
# 
# #check for 1:many matches
# lso5m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
#   filter(n()>1)
# 
# 
# #check for unmatched
# lso5m %>% filter(is.na(orgunituid)) 

###############################################################################


lso <- bind_rows(lso5, lso5m) %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(lso) - nrow(lso_info)


#later bind country dfs together

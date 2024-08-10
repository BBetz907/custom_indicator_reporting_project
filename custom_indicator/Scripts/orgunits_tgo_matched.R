
# obtain metadata from infolink -------------------------------------------



# user purr to create DF for each country, named after each count --------
tgo_info <- complete_clean_data %>% filter(country=="Togo") %>%
  # mutate(snu_3 = recode(snu_3,
  #                       "" = "")
  #   # snu_3 = str_to_title(snu_3),
  # ) %>%
glimpse()

# snu3 <- c(unique(tgo_info$snu_3)) %>% print()
snu2 <- c(unique(tgo_info$snu_2)) %>% print()
snu1 <- c(unique(tgo_info$snu_1)) %>% print()

# get orgunit levels to match and join ------------------------------------
tgo6op <- df_orgs$tgo_orgs %>% filter(orgunit_level  == "6") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name)
tgo6uid <- c(tgo6op$orgunit_uid)
unique(tgo6op$orgunit_name)

tgo5op <- df_orgs$tgo_orgs %>% filter(orgunit_level  == "5") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
tgo5uid <- c(tgo5op$orgunit_uid)
unique(tgo5op$orgunit_name)


df_orgs$tgo_orgs %>% filter(orgunit_name %in% snu2) #snu2 should match datim level 6
df_orgs$tgo_orgs %>% filter(orgunit_name %in% snu1) #snu1 should match datim level 5


################################################################################



# for most level 3 that match_level 6, use snu_3 id-----------------------
tgo6 <- tgo_info %>% filter(snu_2_id %in% tgo6uid, snu_2!="") %>% select(-snu_1_id) %>% 
  rename(orgunit_uid = snu_2_id)  %>% inner_join(tgo6op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(tgo6)/nrow(tgo_info))

# for level 2 that doesn't match level 6, match by snu_2 name --------
tgo6m1 <- tgo_info %>% filter(!snu_2_id %in% tgo6uid) %>% rename(orgunit_name = snu_2)
scales::percent(nrow(tgo6m1)/nrow(tgo_info))
nrow(tgo6m1)

tgo_info


#identify and resolve any failed matches
tgo5m1 <- tgo6m1 %>%
  anti_join(tgo6op) %>% glimpse()
##resolve discrepancies
# non_matched_snu_2_name <- unique(non_matched_snu_2$orgunit_name) %>% print()
# test2 <- tgo6op %>% select(orgunit_name) %>%
#   filter(str_detect(orgunit_name, "Leprosy")) %>% arrange((orgunit_name))
# unique(test2$orgunit_name)
#unable to find one facility (match level 2)


#now match
tgo6m <- tgo6m1 %>% inner_join(tgo6op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_2_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(tgo6m)
scales::percent(nrow(tgo6m)/nrow(tgo_info))

#check for 1:many matches
tgo6m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)



tgo5 <- tgo5m1 %>% mutate(orgunit_name = str_to_upper(snu_1)) %>% 
  select(-contains("snu")) %>% 
  inner_join(tgo5op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 

##############################################################################

# check for missing data at snu_level_2
# tgo_info %>% filter(snu_2 == "", snu_2 == "") %>% print()

##############################################################################
# 
# # for snu2 2 that do not match_level 6, use snu_2 id-----------------------
# tgo5 <- tgo_info %>% filter(snu_2_id %in% tgo5uid, snu_2 %in% non_matched_snu_2_name) %>% select(-snu_1_id) %>% 
#   rename(orgunit_uid = snu_2_id)  %>% inner_join(tgo6op) %>%  
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>% 
# scales::percent(nrow(tgo5)/nrow(tgo_info))
# 
# # for snu2 that doesn't match level 6, match by snu_2 name --------
# tgo5m1 <- tgo_info %>% filter(!snu_2_id %in% tgo6uid, snu_2 %in% non_matched_snu_2_name) %>% rename(orgunit_name = snu_2) %>% glimpse()
# scales::percent(nrow(tgo5m1)/nrow(tgo_info))
# nrow(tgo5m1)
# 
# #identify and resolve any failed matches
# non_matched_snu_2 <- tgo5m1 %>%
#   anti_join(tgo5op) %>% glimpse()
# 
# 
# #now match
# tgo5m <- tgo5m1 %>% inner_join(tgo5op) %>% # or inner if there are non-matches 
#   select(-snu_1_id) %>%
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
#   glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
# nrow(tgo5m)
# scales::percent(nrow(tgo5m)/nrow(tgo_info))
# 
# #check for 1:many matches
# tgo5m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
#   filter(n()>1)
# 
# 
# #check for unmatched
# tgo5m %>% filter(is.na(orgunituid)) 

###############################################################################


tgo <- bind_rows(tgo6, tgo6m, tgo5) %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(tgo) - nrow(tgo_info)


#later bind country dfs together

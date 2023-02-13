
# obtain metadata from infolink -------------------------------------------



# user purr to create DF for each country, named after each count --------
bfa_info <- complete_clean_data %>% filter(country=="Burkina Faso") %>%
  # mutate(snu_3 = recode(snu_3,
  #                       "" = "")
  #   # snu_3 = str_to_title(snu_3),
  # ) %>%
glimpse()

snu1 <- unique(bfa_info$snu_1) %>% print()
snu2 <- c(unique(bfa_info$snu_2)) %>% print()

# get orgunit levels to match and join ------------------------------------
bfa6op <- bfa_5_6 %>% filter(orgunit_level  == "6") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name)
bfa6uid <- c(bfa6op$orgunit_uid)
unique(bfa6op$orgunit_name)

bfa5op <- bfa_5_6 %>% filter(orgunit_level  == "5") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
bfa5uid <- c(bfa5op$orgunit_uid)
unique(bfa5op$orgunit_name)


bfa_5_6 %>% filter(orgunit_name %in% snu2) #snu2 should match datim level 6
bfa_5_6 %>% filter(orgunit_name %in% snu1) #snu2 should match datim level 5


################################################################################



# for most level 3 that match_level 6, use snu_2 id-----------------------
bfa6 <- bfa_info %>% filter(snu_2_id %in% bfa6uid, snu_2!="") %>% select(-snu_1_id) %>% 
  rename(orgunit_uid = snu_2_id)  %>% inner_join(bfa6op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(bfa6)/nrow(bfa_info))

# for level 3 that doesn't match level 6, match by snu_2 name --------
bfa6m1 <- bfa_info %>% filter(!snu_2_id %in% bfa6uid, snu_2 != "") %>% rename(orgunit_name = snu_2)
scales::percent(nrow(bfa6m1)/nrow(bfa_info))
nrow(bfa6m1)

#identify and resolve any failed matches
test <- bfa61 <- bfa6m1 %>%
  anti_join(bfa6op) %>% print()
#resolve discrepancies
# unique(test$orgunit_name)
# test2 <- bfa6op %>% select(orgunit_name) %>%
#   filter(str_detect(orgunit_name, "Ramotswa")) %>% arrange((orgunit_name))
# unique(test2$orgunit_name)


#now match
bfa6m <- bfa6m1 %>% inner_join(bfa6op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_2_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(bfa6m)
scales::percent(nrow(bfa6m)/nrow(bfa_info))

#check for 1:many matches
bfa6m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)


#check for unmatched
bfa6m %>% filter(is.na(orgunituid)) 


##############################################################################

# check for missing data at snu_level_3
bfa5 <- bfa_info %>% filter(snu_2 == "", snu_2 == "") %>% print()

##############################################################################




###############################################################################


bfa <- bind_rows(bfa6, bfa6m) %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(bfa) - nrow(bfa_info)


#later bind country dfs together


# obtain metadata from infolink -------------------------------------------


#ideally we'd run this after mech ref table merge and before data checking. First the 
#issue with the data source must be solved for mech_ref...R
# tnz_info <- complete_clean_data %>% filter(country=="Tanzania") %>% 
#   select(snu_1:snu_4_id, value) %>% group_by(across(c(-value))) %>% summarise(value = sum(value), .groups = "drop") %>% 
#   clean_names() %>% 
#   glimpse()

# user purr to create DF for each country, named after each count --------
tnz_info <- complete_clean_data %>% filter(country=="Tanzania") %>% 
  clean_names() %>% print()
#currently commented out because we are merging at a simpler level for now as we test the process




# get orgunit levels to match and join ------------------------------------
tnz7op <- tnz_6_7 %>% filter(orgunit_level  == "7") %>% select(orgunit_level:orgunit_name) 
tnz7uid <- c(tnz7op$orgunit_uid)

tnz6op <- tnz_6_7 %>% filter(orgunit_level  == "6") %>% select(orgunit_level:orgunit_name)
tnz6uid <- c(tnz6op$orgunit_uid)



################################################################################

level7 <- tnz_info %>% filter(snu_4!="")
# for level 4 that match_level 7, use snu_4_id -----------------------
tnz7<- tnz_info %>% filter(snu_4_id %in% tnz7uid, snu_4!="") %>% select(-snu_1_id:-snu_3_id) %>% 
  rename(orgunit_uid  = snu_4_id) %>% inner_join(tnz7op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>% 
glimpse()
nrow(tnz7)

# for level 4 that doesn't match level 7, try match by snu_4 name --------
tnz7m1 <- tnz_info %>% filter(!snu_4_id %in% tnz7uid, snu_4 != "") %>% 
  rename(orgunit_name = snu_4) %>%
        glimpse()
nrow(tnz7m1)

#identify and resolve any failed matches
tnz7m1 %>%
  anti_join(tnz7op) 
#resolve discrepancies

#now match
tnz7m <- tnz7m1 %>% inner_join(tnz7op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_4_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching


#check for 1:many matches
tnz7m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)

#check for unmatched
tnz7m %>% filter(is.na(orgunituid)) 

count(level7) - count(tnz7) - count(tnz7m)
##############################################################################

# check for missing data at snu_level_3
tnz6 <- tnz_info %>% filter(snu_3 == "", snu_4 == "") %>% print()

##############################################################################


#excluding the above

# at snu_level_match to metadata level, tz 6 ------------------------------
tnz6<- tnz_info %>% filter(snu_3_id %in% tnz6uid, snu_4 == "") %>% 
  rename(orgunit_uid = snu_3_id) %>% inner_join(tnz6op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  select(-snu_1_id:-snu_2_id, -snu_4, -snu_4_id)
nrow(tnz6)

# for level 3 that doesn't match level 6, match by snu_3 to snu_3_id from metadata ----------
tnz6m1 <- tnz_info %>% filter(!snu_3_id %in% tnz6uid, snu_4 == "")
nrow(tnz6m1) 

#identify any failed matches, but do not save --> resolve in later stages
tnz6m1 %>%
  rename(orgunit_name = snu_3, orgunit_parent = snu_2) %>%
  #add in second join criteria for parent id to statement below
  anti_join(tnz6op, by = c("orgunit_name","orgunit_parent")) %>%
  #resolve
  mutate(orgunit_name = recode(orgunit_name, "Mbinga mjini" = "Mbinga Mjini")) %>%
  print()

tnz6op %>% filter(orgunit_parent == "Dodoma MC", orgunit_name == "Mpunguzi")
#one orgunit has tow orgunit_uids, arbitrarily remove one below
tnz6op <- tnz6op %>% filter(orgunit_uid != "gxidqz4F8pv")
#check if resolved
tnz6op %>% filter(orgunit_parent == "Dodoma MC", orgunit_name == "Mpunguzi")


#run inner join
tnz6m <- tnz6m1 %>% 
  rename(orgunit_name = snu_3, orgunit_parent = snu_2) %>% 
  #fix the issue noted above for TZ
  mutate(orgunit_name = recode(orgunit_name, "Mbinga mjini" = "Mbinga Mjini")) %>% 
  #add in second join criteria for parent id to statement below
  inner_join(tnz6op, by = c("orgunit_name", "orgunit_parent"), na_matches = "never") %>% # or inner if there are non-matches
  select(-snu_1_id:-snu_4_id) %>%
  rename(orgunit = orgunit_name, orgunituid = orgunit_uid) %>% 
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching

#1 row double matched initially but no longer --> resolved above
test <- tnz6m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit, orgunit_parent) %>% group_by_all() %>% 
  filter(n()>1)
test



tnz <- bind_rows(tnz7, tnz7m, tnz6, tnz6m) %>% 
select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(tnz) - nrow(tnz_info)
glimpse(tnz6m)

#later bind country dfs together
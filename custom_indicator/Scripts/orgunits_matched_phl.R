
# obtain metadata from infolink -------------------------------------------


#ideally we'd run this after mech ref table merge and before data checking. First the 
#issue with the data source must be solved for mech_ref...R
# npl_info <- complete_clean_data %>% filter(country=="Nepal") %>% 
#   select(snu_1:snu_3_id, value) %>% group_by(across(c(-value))) %>% summarise(value = sum(value), .groups = "drop") %>% 
#   clean_names() %>% 
#   glimpse()

# user purr to create DF for each country, named after each count --------
phll_info <- complete_clean_data %>% filter(country=="Philippines") %>%
  clean_names() %>% print()

table(npl_info$snu_3) #snu3 level should match to level 7 in datim

# get orgunit levels to match and join ------------------------------------
npl7op <- npl_6_7 %>% filter(orgunit_level  == "7") %>% select(orgunit_level:orgunit_name) %>% print()
npl7uid <- c(npl7op$orgunit_uid)

npl6op <- npl_6_7 %>% filter(orgunit_level  == "6") %>% select(orgunit_level:orgunit_name)
npl6uid <- c(npl6op$orgunit_uid)

npl6uid

################################################################################



# for most level 3 that match_level 7, use snu_3_id -----------------------
npl7<- npl_info %>% filter(snu_3_id %in% npl7uid, snu_3!="") %>% select(-snu_1_id:-snu_2_id) %>% 
  rename(orgunit_uid = snu_3_id) %>% inner_join(npl7op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name)
scales::percent(nrow(npl7)/nrow(npl_info))

# for level 3 that doesn't match level 7, match by snu_3 id --------
npl7m1 <- npl_info %>% filter(!snu_3_id %in% npl7uid, snu_3 != "") %>% rename(orgunit_name = snu_3)
scales::percent(nrow(npl7m1)/nrow(npl_info))
nrow(npl7m1)

#identify and resolve any failed matches
npl61 <- npl7m1 %>%
  anti_join(npl7op) %>% print()
#resolve discrepancies

# npl7m1 %>% filter(str_detect(orgunit_name, "Lalitpur"))
npl7op %>% filter(str_detect(orgunit_name, "Bpkihs"))

#now match
npl7m <- npl7m1 %>% inner_join(npl7op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_3_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching


#check for 1:many matches
npl7m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)


#check for unmatched
npl7m %>% filter(is.na(orgunituid)) 


##############################################################################

# check for missing data at snu_level_3
npl_info %>% filter(snu_2 == "") %>% print()

##############################################################################


# at snu_level_match to metadata level, npl 6 ------------------------------
npl6 <- npl61 %>% filter(snu_2_id %in% npl6uid) %>% 
  rename(orgunit = snu_2, orgunituid = snu_2_id) %>% 
  select(-snu_1_id,-snu_3_id) 
nrow(npl6)

# for level 3 that doesn't match level 6, match by snu_2 to snu_2_id from metadata ----------
npl6m1 <- npl61 %>% filter(!snu_2_id %in% npl6uid)
nrow(npl6m1) 

#identify any failed matches, but do not save --> resolve in later stages
npl6m1 %>%
  rename(orgunit_name = snu_2, orgunit_parent = snu_2) %>%
  #add in second join criteria for parent id to statement below
  anti_join(npl6op, by = c("orgunit_name","orgunit_parent")) %>%
  #resolve
  # mutate(orgunit_name = recode(orgunit_name, "Mbinga mjini" = "Mbinga Mjini")) %>%
  print()

#in the case of Nepal some snu_level 3 are wrong but level 2 are ok, no parents needed


# npl6op %>% filter(orgunit_parent == "Dodoma MC", orgunit_name == "Mpunguzi")
# #one orgunit has tow orgunit_uids, arbitrarily remove one below
# npl6op <- npl6op %>% filter(orgunit_uid != "gxidqz4F8pv")
# #check if resolved
# npl6op %>% filter(orgunit_parent == "Dodoma MC", orgunit_name == "Mpunguzi")


#run inner join
npl6m <- npl6m1 %>% select(-orgunit_name) %>%
  rename(orgunit_name = snu_2) %>%
  # #fix the issue noted above for npl
  # mutate(orgunit_name = recode(orgunit_name, "Mbinga mjini" = "Mbinga Mjini")) %>%
  # #add in second join criteria for parent id to statement below
  # inner_join(npl6op, by = c("orgunit_name","orgunit_parent"), na_matches = "never") %>% # or inner if there are non-matches
  inner_join(npl6op) %>%
  select(-snu_1_id:-snu_3_id) %>%
  rename(orgunit = orgunit_name, orgunituid = orgunit_uid) %>% 
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching

#1 row double matched initially but no longer --> resolved above
test <- npl6m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit, orgunit_parent) %>% group_by_all() %>% 
  filter(n()>1)
test



npl <- bind_rows(npl7, npl7m, npl6m) %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(npl) - nrow(npl_info)


#later bind country dfs together

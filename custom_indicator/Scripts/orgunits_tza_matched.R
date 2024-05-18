
# user purr to create DF for each country, named after each count --------
tza_info <- complete_clean_data %>% filter(country=="Tanzania") %>% 
  mutate(snu_2 = recode(snu_2, "Wangingombe DC" = "Wanging'ombe DC"),
         snu_3 = recode(snu_3, "Ilkidinga" = "Ilkiding'a"),
         snu_3 = recode(snu_3, "Mbinga mjini" = "Mbinga Mjini"),
         ) %>%
  clean_names() %>% 
  print()
#currently commented out because we are merging at a simpler level for now as we test the process

# tza_orgs <- df_orgs %>% filter(regionorcountry_code=="TZA")
# get orgunit levels to match and join ------------------------------------
tza7op <- df_orgs$tza_orgs %>% filter(orgunit_level  == "7") %>% select(orgunit_level:orgunit_name) 
tza7uid <- c(tza7op$orgunit_uid)
tza7org <- c(tza7op$orgunit_name)

tza6op <- df_orgs$tza_orgs %>% filter(orgunit_level  == "6") %>% select(orgunit_level:orgunit_name) %>% group_by_all() %>% summarise(.groups = "drop")
tza6uid <- c(tza6op$orgunit_uid)
tza6org <- c(tza6op$orgunit_name)

tza5op <- df_orgs$tza_orgs %>% filter(orgunit_level  == "5") %>% select(orgunit_level:orgunit_name)
tza5uid <- c(tza5op$orgunit_uid)
tza5org <- c(tza5op$orgunit_name)
################################################################################

# for level 4 that match_level 7, use snu_4_id -----------------------
tza7<- tza_info %>% filter(snu_4_id %in% tza7uid, snu_4 != "") %>% select(-snu_1_id:-snu_3_id) %>% 
  rename(orgunit_uid  = snu_4_id) %>% inner_join(tza7op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>% 
glimpse()
nrow(tza7)
scales::percent(nrow(tza7)/nrow(tza_info))

# for level 4 that doesn't match level 7, try match by snu_4 name --------
tza7m1 <- tza_info %>% filter(!snu_4_id %in% tza7uid, snu_4 != "") %>% 
  rename(orgunit_name = snu_4) %>%
        glimpse()
nrow(tza7m1)

#identify and resolve any failed matches
tza7m1 %>%
  anti_join(tza7op) %>% select(snu_2, snu_3, orgunit_name)
#resolve discrepancies
tza7op %>% filter(str_detect(orgunit_parent, "Makum")) #this will be matched one level lower


#now match
tza7m <- tza7m1 %>% inner_join(tza7op, by = c("orgunit_name" = "orgunit_name", "snu_3" ="orgunit_parent")) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_4_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(tza7m)
scales::percent(nrow(tza7m)/nrow(tza_info))

#check for 1:many matches
tza7m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)

#check for unmatched
tza7m %>% filter(is.na(orgunituid)) 

#left to match below level 7
nrow(tza_info) - nrow(tza7) - nrow(tza7m)
# tza_7_completed <- tza7 |> count(snu_4) |> print()
# tza_7_completed2 <- tza7m |> count(orgunit) |> rename(snu_4=orgunit) |> print()
# tza7_anti_list <- rbind(tza_7_completed, tza_7_completed2)
##############################################################################

# check for nonmatched, use going forward
tza6_unmatched_at_7 <- tza_info %>% 
  filter(!snu_4 %in% tza7org, !snu_4_id %in% tza7uid) %>% 
  anti_join(tza_7_completed2 ) |> print()

##############################################################################
nrow(tza6_unmatched_at_7)
# nrow(tza6) + nrow(tza6m)

#excluding the above

# at snu_level 3 match to metadata level, tz 6 ------------------------------
tza6 <- tza6_unmatched_at_7 %>%  
  rename(orgunit_uid = snu_3_id) %>% inner_join(tza6op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  select(-snu_1_id:-snu_2_id, -snu_4, -snu_4_id) %>% print()
nrow(tza6)
#1 check for dup
tza6 %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit, orgunit_parent) %>% group_by_all() %>% 
  filter(n()>1)

# for level 3 that doesn't match level 6, match by snu_3 to snu_3_id from metadata ----------
tza6m1 <- tza6_unmatched_at_7 %>% 
  rename(orgunit_uid = snu_3_id) %>% anti_join(tza6op) %>%  print()


#identify any failed matches, but do not save --> resolve in later stages
tza6m1 %>%
  rename(orgunit_name = snu_3, orgunit_parent = snu_2) %>%
  #add in second join criteria for parent id to statement below
  anti_join(tza6op, by = c("orgunit_name","orgunit_parent")) %>%
  #resolve
  mutate(orgunit_name = recode(orgunit_name, "Mbinga mjini" = "Mbinga Mjini")) %>%
  print()

tza6op %>% filter(orgunit_parent == "Dodoma MC", orgunit_name == "Mpunguzi")
#one orgunit has tow orgunit_uids, arbitrarily remove one below
tza6op <- tza6op %>% filter(orgunit_uid != "gxidqz4F8pv")
#check if resolved
tza6op %>% filter(orgunit_parent == "Dodoma MC", orgunit_name == "Mpunguzi")


tza6m <- tza6m1 %>%   rename(orgunit_name = snu_3, orgunit_parent = snu_2) %>% 
  rename(snu_3_id = orgunit_uid) %>%
  inner_join(tza6op, by = c("orgunit_name", "orgunit_parent"), na_matches = "never") %>% # or inner if there are non-matches
  rename(orgunit = orgunit_name, orgunituid = orgunit_uid) %>% 
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(tza6m) 

#1 row double matched initially but no longer --> resolved above
tza6m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit, orgunit_parent) %>% group_by_all() %>% 
  filter(n()>1)

nrow(tza6_unmatched_at_7) 
- nrow(tza6) - nrow(tza6m1) 

#records unmatched from snu3 to level 6, try against level 5
tza5 <- tza6m1 %>% 
  rename(orgunit_name = snu_3, orgunit_parent = snu_2, snu_3_id = orgunit_uid) %>% 
  anti_join(tza6op, by = c("orgunit_name", "orgunit_parent"), na_matches = "never") %>% 
  rename(snu_3 = orgunit_name, orgunit_name = orgunit_parent) %>% 
  inner_join(tza5op, by = c("orgunit_name"), na_matches = "never") %>% 
  rename(orgunit = orgunit_name, orgunituid = orgunit_uid) 
  
nrow(tza5)

unmatched_at_5m67 <- tza6m1 %>% rename(orgunit_name = snu_3, orgunit_parent = snu_2) %>% 
  anti_join(tza6op, by = c("orgunit_name", "orgunit_parent"), na_matches = "never") %>% 
  select(-orgunit_name) %>% 
  rename(orgunit_name = orgunit_parent) %>%
  anti_join(tza5op, by = c("orgunit_name"), na_matches = "never") %>% print()

unmatched_at_5m67 %>% count(orgunit_name)
# tza6op %>% filter(str_detect(orgunit_parent, "Arusha DC"))

tza <- bind_rows(tza7, tza7m, tza6, tza6m, tza5) %>% 
select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(tza) - nrow(tza_info)


#later bind country dfs together
tza %>% count(orgunit_level)

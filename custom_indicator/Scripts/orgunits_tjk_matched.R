
# obtain metadata from infolink -------------------------------------------


#ideally we'd run this after mech ref table merge and before data checking. First the 
#issue with the data source must be solved for mech_ref...R
# tjk_info <- complete_clean_data %>% filter(country=="Nepal") %>% 
#   select(snu_1:snu_3_id, value) %>% group_by(across(c(-value))) %>% summarise(value = sum(value), .groups = "drop") %>% 
#   clean_names() %>% 
#   glimpse()

# user purr to create DF for each country, named after each count --------
tjk_info <- complete_clean_data %>% filter(country=="Tajikistan") %>%
  mutate(
    snu_4 = str_to_title(snu_4),
    snu_3 = str_to_title(snu_3),
    snu_2 = str_to_title(snu_2),
  )

table(tjk_info$snu_3) #snu3 level should match to level 7 in datim


# get orgunit levels to match and join ------------------------------------
tjk7op <- df_orgs$tjk_orgs %>% 
  mutate(orgunit_parent  = str_to_title(orgunit_parent),
         orgunit_parent  = str_replace(orgunit_parent, "\\s\\s", "\\s"), 
         orgunit_name  = str_to_title(orgunit_name),
         orgunit_name  = str_replace(orgunit_name, "\\s\\s", "\\s")) %>% print() %>%
  filter(orgunit_level  == "7") %>% select(orgunit_level:orgunit_name) 
tjk7uid <- c(tjk7op$orgunit_uid)

tjk6op <- df_orgs$tjk_orgs %>% 
  mutate(orgunit_parent  = str_to_title(orgunit_parent),
         orgunit_parent  = str_replace(orgunit_parent, "\\s\\s", "\\s"), 
         orgunit_name  = str_to_title(orgunit_name),
         orgunit_name  = str_replace(orgunit_name, "\\s\\s", "\\s")) %>% print() %>% 
  filter(orgunit_level  == "6") %>% select(orgunit_level:orgunit_name)
tjk6uid <- c(tjk6op$orgunit_uid)


################################################################################



# for most snu 3 that match_level 7, use snu_3_id -----------------------
tjk7<- tjk_info %>% filter(snu_3_id %in% tjk7uid, snu_3!="") %>% select(-snu_1_id:-snu_2_id) %>% rename(orgunit = snu_3, orgunituid = snu_3_id) 
nrow(tjk7)
scales::percent(nrow(tjk7)/nrow(tjk_info))

# for snu 3 that doesn't match level 7, match by snu_3 id --------
tjk7m1 <- tjk_info %>% filter(!snu_3_id %in% tjk7uid, snu_3 != "") %>% rename(orgunit_name = snu_3)
scales::percent(nrow(tjk7m1)/nrow(tjk_info))
nrow(tjk7m1) 

#identify and resolve any failed matches
tjk7m1 %>%
  anti_join(tjk7op) %>% print()
#resolve discrepancies


#now match
tjk7m <- tjk7m1 %>% inner_join(tjk7op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_3_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  print() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
scales::percent(nrow(tjk7m)/nrow(tjk_info))
nrow(tjk7m) 

#check for 1:many matches
tjk7m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)


#check for unmatched
tjk7m %>% filter(is.na(orgunituid)) 


# for null snu3s, match snu2 to level6 ------------------------------------

tjk6 <- tjk_info %>% filter(snu_3 == "") %>% rename(orgunit_uid = snu_2_id) %>% inner_join(tjk6op)
nrow(tjk6)

#keep non matches
tjk6m1 <- tjk_info %>% filter(!snu_2_id %in% tjk6uid, snu_3 == "") 

tjk6m <- tjk6m1 %>% filter(snu_3 == "") %>% rename(orgunit_name = snu_2) %>% inner_join(tjk6op)
nrow(tjk6m)


tjk <- bind_rows(tjk7, tjk7m, tjk6, tjk6m) %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(tjk) - nrow(tjk_info)


#later bind country dfs together

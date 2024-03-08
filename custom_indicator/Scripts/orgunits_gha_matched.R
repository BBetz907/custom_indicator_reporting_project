
# obtain metadata from infolink -------------------------------------------



# user purr to create DF for each country, named after each count --------
gha_info <- complete_clean_data %>% filter(country=="Ghana") %>%
  # mutate(snu_3 = recode(snu_3,
  #                       "" = "")
  #   # snu_3 = str_to_title(snu_3),
  # ) %>%
glimpse()

snu4 <- c(unique(gha_info$snu_4)) %>% print()
snu3 <- c(unique(gha_info$snu_3)) %>% print()
snu2 <- c(unique(gha_info$snu_2)) %>% print()
snu1 <- c(unique(gha_info$snu_1)) %>% print()

# get orgunit levels to match and join ------------------------------------
gha8op <- df_orgs$gha_orgs %>% filter(orgunit_level  == "8") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name)
gha8uid <- c(gha8op$orgunit_uid)
unique(gha8op$orgunit_name)

gha7op <- df_orgs$gha_orgs %>% filter(orgunit_level  == "7") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
gha7uid <- c(gha7op$orgunit_uid)
unique(gha7op$orgunit_name)


df_orgs$gha_orgs %>% filter(orgunit_name %in% snu4) #snu4 should match datim level 8
df_orgs$gha_orgs %>% filter(orgunit_name %in% snu3) #snu3 should match datim level 7


################################################################################



# for most level 3 that match_level 8, use snu_2 id-----------------------
gha8 <- gha_info %>% filter(snu_4_id %in% gha8uid, snu_4!="") %>% select(-snu_1_id) %>% 
  rename(orgunit_uid = snu_4_id)  %>% inner_join(gha8op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(gha8)/nrow(gha_info))

# for level 3 that doesn't match level 8, match by snu_2 name --------
gha8m1 <- gha_info %>% filter(!snu_4_id %in% gha8uid, snu_4 != "") %>% rename(orgunit_name = snu_4)
scales::percent(nrow(gha8m1)/nrow(gha_info))
nrow(gha8m1)

#identify and resolve any failed matches
test <- gha81 <- gha8m1 %>%
  anti_join(gha8op) %>% print()
#resolve discrepancies
# unique(test$orgunit_name)
# test2 <- gha8op %>% select(orgunit_name) %>%
#   filter(str_detect(orgunit_name, "Ramotswa")) %>% arrange((orgunit_name))
# unique(test2$orgunit_name)


#now match
gha8m <- gha8m1 %>% inner_join(gha8op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_3_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(gha8m)
scales::percent(nrow(gha8m)/nrow(gha_info))

#check for 1:many matches
gha8m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)


#check for unmatched
gha8m %>% filter(is.na(orgunituid)) 


##############################################################################

# check for missing data at snu_level_3
gha7 <- gha_info %>% filter(snu_2 == "", snu_2 == "") %>% print()

##############################################################################




###############################################################################


gha <- bind_rows(gha8, gha8m) %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(gha) - nrow(gha_info)


#later bind country dfs together

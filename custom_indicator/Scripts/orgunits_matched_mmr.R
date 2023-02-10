
# obtain metadata from infolink -------------------------------------------


#ideally we'd run this after mech ref table merge and before data checking. First the 
#issue with the data source must be solved for mech_ref...R
# mmr_info <- complete_clean_data %>% filter(country=="Nepal") %>% 
#   select(snu_1:snu_4_id, value) %>% group_by(across(c(-value))) %>% summarise(value = sum(value), .groups = "drop") %>% 
#   clean_names() %>% 
#   glimpse()

# user purr to create DF for each country, named after each count --------
mmr_info <- complete_clean_data %>% filter(country=="Burma") %>%
  mutate(snu_4 = recode(snu_4,
                        "Daisy TG Clinic" = "Daisy Transgender Clinic")
    # snu_4 = str_to_title(snu_4),
    # snu_4 = str_to_title(snu_4),
    # snu_3 = str_to_title(snu_3),
  ) %>% glimpse()

table(mmr_info$snu_4) #snu4 level should match to level 7 in datim


# get orgunit levels to match and join ------------------------------------
mmr7op <- mmr_7_8 %>% filter(orgunit_level  == "7") %>% select(orgunit_level:orgunit_name) 
mmr7uid <- c(mmr7op$orgunit_uid)

mmr8op <- mmr_7_8 %>% filter(orgunit_level  == "8") %>% select(orgunit_level:orgunit_name)
mmr8uid <- c(mmr8op$orgunit_uid)
mmr8op

################################################################################
unique(mmr_info$snu_4)
unique(mmr_7_8$orgunit_name)


# for most level 4 that match_level 7, use snu_4 id-----------------------
mmr8 <- mmr_info %>% filter(snu_4_id %in% mmr8uid, snu_4!="") %>% select(-snu_1_id:-snu_3_id) %>% 
  rename(orgunit_uid = snu_4_id)  %>% inner_join(mmr8op) %>%  glimpse()
  rename(orgunituid = orgunit_uid, orgunit = snu_4) 
scales::percent(nrow(mmr8)/nrow(mmr_info))

# for level 4 that doesn't match level 8, match by snu_4 id --------
mmr8m1 <- mmr_info %>% filter(!snu_4_id %in% mmr8uid, snu_4 != "") %>% rename(orgunit_name = snu_4)
scales::percent(nrow(mmr8m1)/nrow(mmr_info))
nrow(mmr8m1)

#identify and resolve any failed matches
test <- mmr81 <- mmr8m1 %>%
  anti_join(mmr8op) %>% print()
#resolve discrepancies
unique(test$orgunit_name)
test2 <- mmr8op %>% select(orgunit_name) %>%
  filter(str_detect(orgunit_name, "Clinic")) %>% arrange((orgunit_name))
unique(test2$orgunit_name)


#now match
mmr8m <- mmr8m1 %>% inner_join(mmr8op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_4_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(mmr8m)
scales::percent(nrow(mmr8m)/nrow(mmr_info))

#check for 1:many matches
mmr8m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)


#check for unmatched
mmr8m %>% filter(is.na(orgunituid)) 


##############################################################################

# check for missing data at snu_level_3
mmr7 <- mmr_info %>% filter(snu_3 == "", snu_4 == "") %>% print()

##############################################################################


#excluding the above

# at snu_level_match to metadata level, mmr 7 using uid------------------------------
mmr7<- mmr_info %>% filter(snu_3_id %in% mmr7uid, snu_4 == "") %>% 
  rename(orgunit_uid = snu_3_id) %>% inner_join(mmr7op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%  
select(-snu_1_id:-snu_2_id, -snu_4)
nrow(mmr7)
scales::percent(nrow(mmr7)/nrow(mmr_info))

# for level 3 that doesn't match level 7 id, match by snu_3 name from metadata ----------
mmr7m1 <- mmr_info %>% filter(!snu_3_id %in% mmr7uid, snu_4 == "")
nrow(mmr7m1) 
#none that doesn't match by snu3id



###############################################################################


mmr <- bind_rows(mmr8, mmr8m, mmr7) %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(mmr) - nrow(mmr_info)


#later bind country dfs together


# obtain metadata from infolink -------------------------------------------


#ideally we'd run this after mech ref table merge and before data checking. First the 
#issue with the data source must be solved for mech_ref...R
# phl_info <- complete_clean_data %>% filter(country=="philippines") %>% 
#   select(snu_1:snu_4_id, value) %>% group_by(across(c(-value))) %>% summarise(value = sum(value), .groups = "drop") %>% 
#   clean_names() %>% 
#   glimpse()

# user purr to create DF for each country, named after each count --------
phl_info <- complete_clean_data %>% filter(country=="Philippines") %>%
  mutate(snu_4 = recode(snu_4, 
         "Usbong ng Bulakenyo Inc." = "Usbong ng Bulakenyo",
         "Gen. Trias SHC" = "General Trias SHC" 
         )) %>%
  clean_names() %>% print()

unique(phl_info$snu_4) #snu4 level should match to level 8 in datim
unique(phl_8$orgunit_name)

# get orgunit levels to match and join ------------------------------------
phl8op <- phl_8 %>% select(orgunit_level:orgunit_name) %>% print()
phl8uid <- c(phl8op$orgunit_uid)



################################################################################



# for most level 4 that match_level 8, use snu_4_id -----------------------
phl8<- phl_info %>% filter(snu_4_id %in% phl8uid, snu_4!="") %>% select(-snu_1_id:-snu_3_id) %>% 
  rename(orgunit_uid = snu_4_id) %>% inner_join(phl8op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name)
scales::percent(nrow(phl8)/nrow(phl_info))

# for level 4 that doesn't match level 8, match by snu_4 name --------
phl8m1 <- phl_info %>% filter(!snu_4_id %in% phl8uid, snu_4 != "") %>% rename(orgunit_name = snu_4)
scales::percent(nrow(phl8m1)/nrow(phl_info))
nrow(phl8m1)

#identify and resolve any failed matches
phl8m1 %>%
  anti_join(phl8op) %>% select(orgunit_name) 
#resolve discrepancies



#now match
phl8m <- phl8m1 %>% inner_join(phl8op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_4_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(phl8m)
scales::percent(nrow(phl8m)/nrow(phl_info))

#check for 1:many matches
phl8m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)


#check for unmatched
phl8m %>% filter(is.na(orgunituid)) 

##############################################################################

# check for missing data at snu_level_4
phl_info %>% filter(snu_2 == "") %>% print()

##############################################################################




phl <- bind_rows(phl8, phl8m) %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(phl) - nrow(phl_info)


#later bind country dfs together

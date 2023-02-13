
# obtain metadata from infolink -------------------------------------------


#ideally we'd run this after mech ref table merge and before data checking. First the 
#issue with the data source must be solved for mech_ref...R
# swz_info <- complete_clean_data %>% filter(country=="Nepal") %>% 
#   select(snu_1:snu_3_id, value) %>% group_by(across(c(-value))) %>% summarise(value = sum(value), .groups = "drop") %>% 
#   clean_names() %>% 
#   glimpse()

# user purr to create DF for each country, named after each count --------
swz_info <- complete_clean_data %>% filter(country=="Eswatini") %>%
  clean_names() %>% glimpse()

table(swz_info$snu_2) #snu2 level should match to level 5 in datim
table(swz6op$orgunit_parent)

# get orgunit levels to match and join ------------------------------------
swz5op <- swz_5_6 %>% filter(orgunit_level  == "5") %>% select(orgunit_level:orgunit_name)
swz5uid <- c(swz5op$orgunit_uid)
swz5uid

################################################################################



# for most level 2 that match_level 5, use snu_2_id -----------------------
swz5<- swz_info %>% filter(snu_2_id %in% swz5uid, snu_2!="") %>% 
  select(-snu_4_id, -snu_3_id) %>% 
  rename(orgunit_uid  = snu_2_id) %>% inner_join(swz5op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(swz5)/nrow(swz_info))
nrow(swz5)

# for level 3 that doesn't match level 2, match by snu_2 id --------
swz5m1 <- swz_info %>% filter(!snu_2_id %in% swz5uid, snu_2 != "") %>% rename(orgunit_name = snu_2)
scales::percent(nrow(swz5m1)/nrow(swz_info), accuracy = 3)
nrow(swz5m1)

#identify and resolve any failed matches
swz5m1 %>% 
  anti_join(swz5op) %>% print()
#resolve discrepancies

#now match
swz5m <- swz5m1 %>% inner_join(swz5op) %>% # or inner if there are non-matches
  select(-snu_3_id:-snu_4_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching


#check for 1:many matches
swz5m_dup <- swz5m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>%
  filter(n()>1) %>% ungroup() %>% print(n=22)

(unique(swz5m_dup$indicator))

swz5m_dups <- c(unique(swz5m_dup$orgunit))
swz5m_dups

swz5op %>% filter(orgunit_name %in% swz5m_dups)  #test against ref file
swz5m1 %>% filter(orgunit_name %in% swz5m_dups, indicator == "PrEP_OFFER") %>% 
  print(n=202) #test against data file 

#potential duplicates were non-unique rows in the data file, not in the merge file


#check for unmatched
swz5m %>% filter(is.na(orgunituid))


swz <- bind_rows(swz5, swz5m) %>% select(-contains("snu"))
#check to see if number of rows matches source
nrow(swz) - nrow(swz_info)


#later bind country dfs together

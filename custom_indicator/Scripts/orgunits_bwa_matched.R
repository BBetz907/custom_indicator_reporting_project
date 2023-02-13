
# obtain metadata from infolink -------------------------------------------



# user purr to create DF for each country, named after each count --------
bwa_info <- complete_clean_data %>% filter(country=="Botswana") %>%
  mutate(snu_3 = recode(snu_3,
                        "Tebelopele VCTC Ramotswa" = "Ramotswa Tebelopele")
    # snu_3 = str_to_title(snu_3),
  ) %>%
glimpse()

is.na(bwa_info$snu_3)  #snu3 level should match to level 6 in datim
unique(bwa_info$snu_3)

# get orgunit levels to match and join ------------------------------------
bwa5op <- bwa_5_6 %>% filter(orgunit_level  == "5") %>% select(orgunit_level:orgunit_name) 
bwa5uid <- c(bwa5op$orgunit_uid)

bwa6op <- bwa_5_6 %>% filter(orgunit_level  == "6") %>% select(orgunit_level:orgunit_name)
bwa6uid <- c(bwa6op$orgunit_uid)
unique(bwa6op$orgunit_name)

################################################################################



# for most level 3 that match_level 6, use snu_3 id-----------------------
bwa6 <- bwa_info %>% filter(snu_3_id %in% bwa6uid, snu_3!="") %>% select(-snu_1_id:-snu_2_id) %>% 
  rename(orgunit_uid = snu_3_id)  %>% inner_join(bwa6op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(bwa6)/nrow(bwa_info))

# for level 3 that doesn't match level 6, match by snu_3 name --------
bwa6m1 <- bwa_info %>% filter(!snu_3_id %in% bwa6uid, snu_3 != "") %>% rename(orgunit_name = snu_3)
scales::percent(nrow(bwa6m1)/nrow(bwa_info))
nrow(bwa6m1)

#identify and resolve any failed matches
test <- bwa61 <- bwa6m1 %>%
  anti_join(bwa6op) %>% print()
#resolve discrepancies
unique(test$orgunit_name)
test2 <- bwa6op %>% select(orgunit_name) %>%
  filter(str_detect(orgunit_name, "Ramotswa")) %>% arrange((orgunit_name))
unique(test2$orgunit_name)


#now match
bwa6m <- bwa6m1 %>% inner_join(bwa6op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_3_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(bwa6m)
scales::percent(nrow(bwa6m)/nrow(bwa_info))

#check for 1:many matches
bwa6m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)


#check for unmatched
bwa6m %>% filter(is.na(orgunituid)) 


##############################################################################

# check for missing data at snu_level_3
bwa5 <- bwa_info %>% filter(snu_3 == "", snu_3 == "") %>% print()

##############################################################################




###############################################################################


bwa <- bind_rows(bwa6, bwa6m) %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(bwa) - nrow(bwa_info)


#later bind country dfs together

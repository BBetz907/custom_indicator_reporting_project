
# obtain metadata from infolink -------------------------------------------



# user purr to create DF for each country, named after each count --------
cod_info <- complete_clean_data %>% filter(country=="DRC") %>% 
  group_by(across(-c(value))) %>% summarise(value = sum(value),.groups = "drop") %>%
  # mutate(snu_3 = recode(snu_3,
  #                       "" = "")
  #   # snu_3 = str_to_title(snu_3),
  # ) %>%
glimpse()

# snu3 <- c(unique(cod_info$snu_3)) %>% print()
snu2 <- c(unique(cod_info$snu_2)) %>% print()
snu1 <- c(unique(cod_info$snu_1)) %>% print()

# get orgunit levels to match and join ------------------------------------
cod5op <- cod_4_5 %>% filter(orgunit_level  == "5") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
cod5uid <- c(cod5op$orgunit_uid)
unique(cod5op$orgunit_name)

cod4op <- cod_4_5 %>% filter(orgunit_level  == "4") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
cod4uid <- c(cod4op$orgunit_uid)
unique(cod4op$orgunit_name)


cod5op %>% filter(orgunit_name %in% snu2) #snu2 should match datim level 5
cod4op %>% filter(orgunit_name %in% snu1) #snu2 should match datim level 5


################################################################################



# for most level 3 that match_level 5, use snu_3 id-----------------------
cod5 <- cod_info %>% filter(snu_2_id %in% cod5uid, snu_2!="") %>% select(-snu_1_id) %>% 
  rename(orgunit_uid = snu_2_id)  %>% inner_join(cod5op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(cod5)/nrow(cod_info))

# for level 2 that doesn't match level 5, match by snu_2 name --------
cod5m1 <- cod_info %>% filter(!snu_2_id %in% cod5uid, snu_2 != "") %>% rename(orgunit_name = snu_2)
scales::percent(nrow(cod5m1)/nrow(cod_info))
nrow(cod5m1)

#identify and resolve any failed matches
non_matched_snu_2 <- cod5m1 %>%
  anti_join(cod5op) %>% glimpse()
##resolve discrepancies
# non_matched_snu_2_name <- unique(non_matched_snu_2$orgunit_name) %>% print()
# test2 <- cod5op %>% select(orgunit_name) %>%
#   filter(str_detect(orgunit_name, "Leprosy")) %>% arrange((orgunit_name))
# unique(test2$orgunit_name)
#unable to find one facility (match level 2)


#now match
cod5m <- cod5m1 %>% filter(orgunit_name != "Binza Meteo") %>%
  inner_join(cod5op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_2_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(cod5m)
scales::percent(nrow(cod5m)/nrow(cod_info))

#check for 1:many matches
dup <- cod5m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  arrange(orgunit, indicator, age, sex, otherdisaggregate, population) %>%
  filter(n()>1)
dup

level5_dup <- unique(c(dup$orgunit)) %>% print()
cod5op %>% filter(orgunit_name %in% level5_dup)


#check for unmatched
cod5m %>% filter(is.na(orgunituid)) 

#duplicates were not actually issues



#now recide "Binza Meteo" at level 4, Kinsasha hIPJyWupajj. 2 orgunit levels make PSNU level analysis a challenge    
cod4 <- cod_info %>% filter(snu_2 == "Binza Meteo") %>% 
  mutate(orgunit_level = "4",
         orgunit = snu_1,
         orgunituid = "hIPJyWupajj",
         orgunit_parent = "Democratic Republic of the Congo",
         orgunit_parent_uid  = "ANN4YCOufcP") %>% 
  select(-contains("snu")) %>% glimpse()
scales::percent(nrow(cod4)/nrow(cod_info))

##############################################################################

# check for missing data at snu_level_2
# cod_info %>% filter(snu_2 == "", snu_2 == "") %>% print()

##############################################################################
# 
# # for snu2 2 that do not match_level 5, use snu_2 id-----------------------
# cod5 <- cod_info %>% filter(snu_2_id %in% cod5uid, snu_2 %in% non_matched_snu_2_name) %>% select(-snu_1_id) %>% 
#   rename(orgunit_uid = snu_2_id)  %>% inner_join(cod5op) %>%  
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>% 
# scales::percent(nrow(cod5)/nrow(cod_info))
# 
# # for snu2 that doesn't match level 5, match by snu_2 name --------
# cod5m1 <- cod_info %>% filter(!snu_2_id %in% cod5uid, snu_2 %in% non_matched_snu_2_name) %>% rename(orgunit_name = snu_2) %>% glimpse()
# scales::percent(nrow(cod5m1)/nrow(cod_info))
# nrow(cod5m1)
# 
# #identify and resolve any failed matches
# non_matched_snu_2 <- cod5m1 %>%
#   anti_join(cod5op) %>% glimpse()
# 
# 
# #now match
# cod5m <- cod5m1 %>% inner_join(cod5op) %>% # or inner if there are non-matches 
#   select(-snu_1_id) %>%
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
#   glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
# nrow(cod5m)
# scales::percent(nrow(cod5m)/nrow(cod_info))
# 
# #check for 1:many matches
# cod5m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
#   filter(n()>1)
# 
# 
# #check for unmatched
# cod5m %>% filter(is.na(orgunituid)) 

###############################################################################


cod <- bind_rows(cod5, cod5m, cod4) %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(cod) - nrow(cod_info)


#later bind country dfs together

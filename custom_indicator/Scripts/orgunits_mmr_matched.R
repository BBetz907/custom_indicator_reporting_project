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
mmr7op <- df_orgs$mmr_orgs %>% filter(orgunit_level  == "7") %>% select(orgunit_level:orgunit_name) 
mmr7uid <- c(mmr7op$orgunit_uid)

mmr8op <- df_orgs$mmr_orgs %>% filter(orgunit_level  == "8") %>% select(orgunit_level:orgunit_name)
mmr8uid <- c(mmr8op$orgunit_uid)
mmr8org <- c(mmr8op$orgunit_name)


################################################################################
unique(mmr_info$snu_4)
unique(df_orgs$mmr_orgs$orgunit_name)

nrow(mmr_info)
# for most level 4 that match_level 7, use snu_4 id-----------------------
mmr8 <- mmr_info %>% filter(snu_4_id %in% mmr8uid, snu_4!="") %>% select(-snu_1_id:-snu_3_id) %>% 
  rename(orgunit_uid = snu_4_id)  %>% inner_join(mmr8op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(mmr8)/nrow(mmr_info))
nrow(mmr8)


# for non-missing level 4 that doesn't match level 8 by id, match by snu_4 name --------
mmr8m1 <- mmr_info %>% filter(!snu_4_id %in% mmr8uid, snu_4 != "") %>% rename(orgunit_name = snu_4)
scales::percent(nrow(mmr8m1)/nrow(mmr_info))
nrow(mmr8m1)


#now match level 4 to level 8 by ID
mmr8m <- mmr8m1 %>%   inner_join(mmr8op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_4_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(mmr8m)
scales::percent(nrow(mmr8m)/nrow(mmr_info))

#check for 1:many matches
mmr8m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)




##############################################################################

# check for missing data at snu_level_3
mmr_info %>% filter(snu_3 == "", snu_4 == "") %>% print()

##############################################################################
mmr_info |> rename(orgunit_name = snu_4) |> count(orgunit_name) |> 
  inner_join(mmr8op)

#excluding the above that match at level 8

# at snu_level_match to metadata level, mmr 7 using uid------------------------------
mmr7<- mmr_info %>% 
  rename(orgunit_name = snu_4) |> anti_join(mmr8op) |> select(-orgunit_name) |> #excluding those that matched by snu4 name to level 8 name
  filter(snu_3_id %in% mmr7uid) %>% #302 appear to match by snu_3_id to level 7 id
  rename(orgunit_uid = snu_3_id) %>% inner_join(mmr7op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>% glimpse()
nrow(mmr7)
scales::percent(nrow(mmr7)/nrow(mmr_info))


# for level 3 that doesn't match level 7 id, try to match by snu_3 name from metadata ----------
mmr7m1 <-mmr_info %>% 
  rename(orgunit_name = snu_4) |> anti_join(mmr8op) |> select(-orgunit_name) |> #excluding those that matched by snu4 name to level 8 name
  filter(!snu_3_id %in% mmr7uid) |> 
  rename(orgunit_name = snu_3) %>% glimpse()
nrow(mmr7m1) 

mmr7m <- mmr7m1 %>%  inner_join(mmr7op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_4_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(mmr7m)
scales::percent(nrow(mmr8m)/nrow(mmr_info))

#check for 1:many matches
mmr7m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)


#check for unmatched
mmr7m %>% filter(is.na(orgunituid)) 


##############################################################################

# check for missing data at snu_level_3
mmr_info %>% filter(snu_3 == "", snu_4 == "") %>% print()

##############################################################################
mmr6<- mmr_info %>% filter(!snu_4 %in% mmr8org) |> print()
###############################################################################


mmr <- bind_rows(mmr8m, mmr7, mmr7m) %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(mmr) - nrow(mmr_info)


#later bind country dfs together

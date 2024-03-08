
# obtain metadata from infolink -------------------------------------------



# user purr to create DF for each country, named after each count --------
lbr_info <- complete_clean_data %>% filter(country=="Liberia") %>%
  # mutate(snu_3 = recode(snu_3,
  #                       "" = "")
  #   # snu_3 = str_to_title(snu_3),
  # ) %>%
glimpse()

# snu3 <- c(unique(lbr_info$snu_3)) %>% print()
snu3 <- c(unique(lbr_info$snu_3)) %>% print()
snu2 <- c(unique(lbr_info$snu_2)) %>% print()
snu1 <- c(unique(lbr_info$snu_1)) %>% print()

# get orgunit levels to match and join ------------------------------------
lbr7op <- df_orgs$lbr_orgs %>% filter(orgunit_level  == "7") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name)
lbr7uid <- c(lbr7op$orgunit_uid)
lbr7org <- c(lbr7op$orgunit_name)
# unique(lbr7op$orgunit_name)

lbr6op <- df_orgs$lbr_orgs %>% filter(orgunit_level  == "6") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
lbr6uid <- c(lbr6op$orgunit_uid)
unique(lbr6op$orgunit_name)


df_orgs$lbr_orgs %>% filter(orgunit_name %in% snu3) #snu3 should match datim level 7
df_orgs$lbr_orgs %>% filter(orgunit_name %in% snu2) #snu2 should match datim level 6


################################################################################

nrow(lbr_info)

# for snu 3 that match_level 7 by id, use snu_3 id-----------------------
lbr7 <- lbr_info %>% filter(snu_3_id %in% lbr7uid, snu_3!="") %>% select(-snu_1_id) %>% 
  rename(orgunit_uid = snu_3_id)  %>% inner_join(lbr7op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
nrow(lbr7)
scales::percent(nrow(lbr7)/nrow(lbr_info))

# for snu 3 that doesn't match level 7 by id, create new df to match by name or snu2_id to level 6 --------
lbr7m1 <- lbr_info %>% filter(!snu_3_id %in% lbr7uid |  snu_3=="") %>% rename(orgunit_name = snu_3)
scales::percent(nrow(lbr7m1)/nrow(lbr_info))
nrow(lbr7m1)

#identify and resolve any failed matches
non_matched_snu_3 <- lbr7m1 %>%
  anti_join(lbr7op) %>% glimpse()

##resolve discrepancies
non_matched_snu_3_name <- unique(non_matched_snu_3$orgunit_name) %>% print()
test2 <- lbr7op %>% select(orgunit_name) %>%
  filter(str_detect(orgunit_name, "eprosy")) %>% arrange((orgunit_name))
unique(test2$orgunit_name)
#unable to find one facility (match level 2)

glimpse(lbr7m1)
#now match
lbr7m <- lbr7m1 %>% filter(orgunit_name %in% lbr7org, orgunit_name != "") %>%
  inner_join(lbr7op, by=join_by("orgunit_name")) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_3_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(lbr7m)
scales::percent(nrow(lbr7m)/nrow(lbr_info))

nrow(lbr_info) - nrow(lbr7) - nrow(lbr7m)
scales::percent((nrow(lbr_info) - nrow(lbr7) - nrow(lbr7m))/nrow(lbr_info))

#check for 1:many matches
#lbr7m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
 # filter(n()>1)


#check for unmatched
lbr7m %>% filter(is.na(orgunit)) 

# lbr_info %>% anti_join(lbr7op, by = c("snu_3_id" = "")) %>% anti_join(lbr_info, by = c("" = ))
#############################################################################

# check for missing data at snu_level_3
lbr_no3 <- lbr_info %>% filter(snu_3 == "" ) %>% print()
nrow(lbr_info) - (nrow(lbr_no3) + nrow(lbr7m) + nrow(lbr7))

##############################################################################

# for snu3 3 that do not match_level 7, use snu_2  and match to level 6 names (uid does not work)-----------------------
lbr6m <- lbr_info %>% filter(!snu_3_id %in% lbr7uid, !snu_3 %in% lbr7org) %>% 
  rename(orgunit_name = snu_2)  %>% 
  inner_join(lbr6op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>% glimpse()
scales::percent(nrow(lbr6m)/nrow(lbr_info))




lbr <- bind_rows(lbr7, lbr7m, lbr6m) %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(lbr) - nrow(lbr_info)


#later bind country dfs together


# obtain metadata from infolink -------------------------------------------



# user purr to create DF for each country, named after each count --------
civ_info <- complete_clean_data %>% mutate(country = recode(country, "Cote dIvoire" = "Cote d'Ivoire")) %>%
  filter(country=="Cote d'Ivoire") %>%
  mutate(snu_4 = recode(snu_4,
                        "CEEDeB Guiglo" = "Centre d’Eveil et d’Encadrement pour le Developpement a la Base de Guiglo (CEEDeB)",
                        "CEEDeB Duekoue" = "Centre d’Eveil et d’Encadrement pour le Developpement a la Base de Duekoue (CEEDeB)",
                        "CMS PRIVE-ONG ASAPSU de Yamoussoukro" = "Centre Medico-Social PRIVE-ONG ASAPSU de YAMOUSSOUKRO")
    # snu_3 = str_to_title(snu_3),
  ) %>%
glimpse()

snu4 <- c(unique(civ_info$snu_4)) %>% print()
snu3 <- c(unique(civ_info$snu_3)) %>% print()
snu2 <- c(unique(civ_info$snu_2)) %>% print()
snu1 <- c(unique(civ_info$snu_1)) %>% print()

# get orgunit levels to match and join ------------------------------------

civ7op <- df_orgs$civ_orgs %>% filter(orgunit_level  == "7") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
civ7uid <- c(civ7op$orgunit_uid)
unique(civ7op$orgunit_name)

civ6op <- df_orgs$civ_orgs%>% filter(orgunit_level  == "6") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
civ6uid <- c(civ6op$orgunit_uid)
unique(civ6op$orgunit_name)

civ7op %>% filter(orgunit_name %in% snu4) #snu4 should match datim level 7
civ6op %>% filter(orgunit_name %in% snu3) |> print(n=29
                                                   
                                                   )#snu3 should match datim level 6


################################################################################



# for most level 3 that match_level 6, use snu_3 id-----------------------
civ7 <- civ_info %>% filter(snu_4_id %in% civ7uid, snu_4!="") %>% select(-snu_1_id) %>% 
  rename(orgunit_uid = snu_4_id)  %>% inner_join(civ7op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(civ7)/nrow(civ_info))

# for level 4 that doesn't match level 7, match by snu_4 name --------
civ7m1 <- civ_info %>% filter(!snu_4_id %in% civ7uid, snu_4 != "") %>% rename(orgunit_name = snu_4)
scales::percent(nrow(civ7m1)/nrow(civ_info))
nrow(civ7m1)

#identify and resolve any failed matches
non_matched_snu_4 <- civ7m1 %>%
  anti_join(civ7op) %>% glimpse()
#resolve discrepancies
non_matched_snu_4_name <- unique(non_matched_snu_4$orgunit_name) %>% print()
test2 <- civ7op %>% select(orgunit_name) %>%
  filter(str_detect(orgunit_name, "ASAPSU")) %>% arrange((orgunit_name))
unique(test2$orgunit_name)
#unable to find one facility (match level 2)

#now match
civ7m <- civ7m1 %>% inner_join(civ7op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_4_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(civ7m)
scales::percent(nrow(civ7m)/nrow(civ_info))

#check for 1:many matches
civ7m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)


#check for unmatched
civ7m %>% filter(is.na(orgunituid)) 


# ##############################################################################
# 
# check for missing data at snu_level_3
civ_info %>% filter(snu_3 == "", snu_4 == "") %>% print()
# 
# ##############################################################################


civ6 <- civ_info %>% anti_join(civ7m, by = c("snu_4" = "orgunit")) |> inner_join(civ6op, by = c("snu_3" = "orgunit_name")) |> 
  rename(orgunituid = orgunit_uid, orgunit = snu_3) |>   select(-ends_with("_id"), -starts_with("snu")) %>% glimpse()

scales::percent(nrow(civ6)/nrow(civ_info))
nrow(civ6)
# 
# # for snu3 that doesn't match level 6, match by snu_2 name --------
# civ6m1 <- civ_info %>% filter(!snu_3_id %in% civ6uid, snu_4 == "") %>% rename(orgunit_name = snu_3) %>% glimpse()
# scales::percent(nrow(civ6m1)/nrow(civ_info))
# nrow(civ6m1)
# 
# #identify and resolve any failed matches
# non_matched_snu_3 <- civ6m1 %>%
#   anti_join(civ6op) %>% glimpse()
# 
# 
# #now match
# civ6m <- civ6m1 %>% inner_join(civ6op) %>% # or inner if there are non-matches
#   select(-snu_1_id:-snu_2_id) %>%
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
#   glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
# nrow(civ6m)
# scales::percent(nrow(civ6m)/nrow(civ_info))
# 
# #check for 1:many matches
# civ6m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>%
#   filter(n()>1)
# 
# 
# 
# 
# #check for unmatched
# civ6m %>% filter(is.na(orgunituid))

###############################################################################


civ <- bind_rows(civ7, civ7m, civ6) %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(civ) - nrow(civ_info)

#later bind country dfs together

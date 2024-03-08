
# obtain metadata from infolink -------------------------------------------


#ideally we'd run this after mech ref table merge and before data checking. First the 
#issue with the data source must be solved for mech_ref...R
# kaz_info <- complete_clean_data %>% filter(country=="Nepal") %>% 
#   select(snu_1:snu_3_id, value) %>% group_by(across(c(-value))) %>% summarise(value = sum(value), .groups = "drop") %>% 
#   clean_names() %>% 
#   glimpse()

# user purr to create DF for each country, named after each count --------
kaz_info <- complete_clean_data %>% filter(country=="Kazakhstan") %>%
  mutate(
    snu_4 = str_to_title(snu_4),
    snu_3 = str_to_title(snu_3),
    snu_2 = str_to_title(snu_2),
  )

table(kaz_info$snu_3) #snu3 level should match to level 7 in datim


# get orgunit levels to match and join ------------------------------------
kaz7op <- df_orgs$kaz_orgs %>% mutate(orgunit_parent  = str_to_title(orgunit_parent),
                                      orgunit_parent  = str_replace(orgunit_parent, "\\s\\s", "\\s"), 
                                      orgunit_name  = str_to_title(orgunit_name),
                                      orgunit_name  = str_replace(orgunit_name, "\\s\\s", "\\s")) %>% print() %>%
  filter(orgunit_level  == "7") %>% select(orgunit_level:orgunit_name) 
kaz7uid <- c(kaz7op$orgunit_uid)

kaz6op <- df_orgs$kaz_orgs %>% mutate(orgunit_parent  = str_to_title(orgunit_parent),
                                      orgunit_parent  = str_replace(orgunit_parent, "\\s\\s", "\\s"), 
                                      orgunit_name  = str_to_title(orgunit_name),
                                      orgunit_name  = str_replace(orgunit_name, "\\s\\s", "\\s")) %>% print() %>%
  filter(orgunit_level  == "6") %>% select(orgunit_level:orgunit_name)


################################################################################



# for most level 3 that match_level 7, use snu_3 -----------------------
kaz7<- kaz_info %>% filter(snu_3_id %in% kaz7uid, snu_3!="") %>% select(-snu_1_id:-snu_2_id) %>% rename(orgunit = snu_3, orgunituid = snu_3_id) 
scales::percent(nrow(kaz7)/nrow(kaz_info))

# for level 3 that doesn't match level 7, separate match by snu_3 id --------
kaz7m1 <- kaz_info %>% filter(!snu_3_id %in% kaz7uid, snu_3 != "") %>% rename(orgunit_name = snu_3)
scales::percent(nrow(kaz7m1)/nrow(kaz_info))
nrow(kaz7m1)

#identify and resolve any failed matches
kaz71 <- kaz7m1 %>%
  anti_join(kaz7op) %>% print()
#resolve discrepancies


#now match
kaz7m <- kaz7m1 %>% inner_join(kaz7op) %>% # or inner if there are non-matches 
  select(-snu_1_id:-snu_3_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching


#check for 1:many matches
kaz7m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)


#check for unmatched
kaz7m %>% filter(is.na(orgunituid)) 

#######
kaz6 <- kaz_info %>% filter(snu_3 == "") %>% rename(orgunit_name = snu_2) |> 
  inner_join(kaz6op) |>  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse()

compare_df_cols(kaz7m, kaz6)


kaz <- bind_rows(kaz7, kaz7m, kaz6) %>% select(-contains("snu")) %>% 
  print() 
#check to see if number of rows matches source
nrow(kaz) - nrow(kaz_info)


#later bind country dfs together

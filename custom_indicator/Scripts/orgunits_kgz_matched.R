
# obtain metadata from infolink -------------------------------------------


#ideally we'd run this after mech ref table merge and before data checking. First the 
#issue with the data source must be solved for mech_ref...R
# kgz_info <- complete_clean_data %>% filter(country=="Nepal") %>% 
#   select(snu_1:snu_3_id, value) %>% group_by(across(c(-value))) %>% summarise(value = sum(value), .groups = "drop") %>% 
#   clean_names() %>% 
#   glimpse()

# user purr to create DF for each country, named after each count --------
kgz_info <- complete_clean_data %>% filter(country=="Kyrgyzstan") %>%
  mutate(
    snu_4 = str_to_title(snu_4),
    snu_3 = str_to_title(snu_3),
    snu_2 = str_to_title(snu_2),
  )

kgz_info |> filter(str_detect(snu_1, "Osh Oblast")) |> select(starts_with("SNU"))

table(kgz_info$snu_3) #snu3 level should match to level 7 in datim


# get orgunit levels to match and join ------------------------------------
kgz7op <- df_orgs$kgz_orgs %>% 
  mutate(orgunit_parent  = str_to_title(orgunit_parent),
         orgunit_parent  = str_replace(orgunit_parent, "\\s\\s", "\\s"), 
         orgunit_name  = str_to_title(orgunit_name),
         orgunit_name  = str_replace(orgunit_name, "\\s\\s", "\\s")) %>% print() %>%
  filter(orgunit_level  == "7") %>% select(orgunit_level:orgunit_name) 
kgz7uid <- c(kgz7op$orgunit_uid)

kgz6op <- df_orgs$kgz_orgs %>% 
  mutate(orgunit_parent  = str_to_title(orgunit_parent),
         orgunit_parent  = str_replace(orgunit_parent, "\\s\\s", "\\s"), 
         orgunit_name  = str_to_title(orgunit_name),
         orgunit_name  = str_replace(orgunit_name, "\\s\\s", "\\s")) %>% print() %>%
  filter(orgunit_level  == "6") %>% select(orgunit_level:orgunit_name) 
kgz6uid <- c(kgz6op$orgunit_uid)
kgz6op |> filter(str_detect(orgunit_name, "Aravan") )
################################################################################



# for most level 2 that match_level 7, use snu_3 -----------------------
kgz7<- kgz_info %>% filter(snu_2_id %in% kgz7uid, snu_2!="") %>% select(-snu_1_id, -snu_3_id) %>% rename(orgunit = snu_2, orgunituid = snu_2_id) 
scales::percent(nrow(kgz7)/nrow(kgz_info))

# for level 2 that doesn't match level 7, match by snu_3 id --------
kgz7m1 <- kgz_info %>% filter(!snu_2_id %in% kgz7uid, snu_2 != "") %>% rename(orgunit_name = snu_3)
scales::percent(nrow(kgz7m1)/nrow(kgz_info))
nrow(kgz7m1)

#identify and resolve any failed matches
kgz71 <- kgz7m1 %>%
  anti_join(kgz7op) %>% print()
#resolve discrepancies
kgz71 |> count(snu_1, snu_2)

#now match on level 2 names
kgz7m <- kgz7m1 %>% inner_join(kgz7op) %>% # or inner if there are non-matches 
  select(-snu_1_id, -snu_3_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  print() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching


#check for 1:many matches
kgz7m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)


#check for unmatched
kgz7m %>% filter(is.na(orgunituid)) 




# match 2 to 7 ------------------------------------------------------------
kgz6 <- kgz71 |> select(-starts_with("org")) |> 
  rename(orgunit_name = snu_2) |> 
  inner_join(kgz6op) |> glimpse()


kgz <- bind_rows(kgz7, kgz7m, kgz6) %>% select(-contains("snu")) 
#check to see if number of rows matches source
nrow(kgz) - nrow(kgz_info)


#later bind country dfs together

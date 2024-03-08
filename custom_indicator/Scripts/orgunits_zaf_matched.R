
# obtain metadata from infolink -------------------------------------------


#ideally we'd run this after mech ref table merge and before data checking. First the 
#issue with the data source must be solved for mech_ref...R
# zaf_info <- complete_clean_data %>% filter(country=="Nepal") %>% 
#   select(snu_1:snu_3_id, value) %>% group_by(across(c(-value))) %>% summarise(value = sum(value), .groups = "drop") %>% 
#   clean_names() %>% 
#   glimpse()

# user purr to create DF for each country, named after each count --------
zaf_info <- complete_clean_data %>% filter(country=="South Africa") %>%
  select(-snu_3:-snu_4, -snu_3_id:-snu_4_id) %>%
  group_by(across(-c(value))) %>% summarise(value = sum(value), .groups = "drop") %>% 
  mutate(
    snu_2 = str_to_title(snu_2),
    snu_2init = str_extract(snu_1, "^[A-Z]"), #pull first initial
    snu_2init2 = if_else(snu_2init == "G", "p", tolower(str_extract(snu_1, "(?<=\\s)[A-Z]"))), #pull second initial else p for Guateng (province)
    snu_2post = if_else(str_detect(snu_2, "Cape\\sTown|Johannesburg"), " Metropolitan Municipality", ""), #add postscript for cape town and johannesburg
    snu_2 = str_c(snu_2init, snu_2init2, " ", snu_2, snu_2post)
                                ) %>%
  select(-snu_2init, -snu_2init2, -snu_2post) %>%
  clean_names() %>% print()

unique(zaf_info$snu_2) #snu2 level should match to level 5 in datim


# get orgunit levels to match and join ------------------------------------
zaf5op <- df_orgs$zaf_orgs %>%
  mutate(orgunit_parent  = str_to_title(orgunit_parent),
         orgunit_parent  = str_replace(orgunit_parent, "\\s\\s", "\\s"), 
         orgunit_name  = str_to_title(orgunit_name),
         orgunit_name  = str_replace(orgunit_name, "\\s\\s", "\\s")) %>% print() %>%
  filter(orgunit_level  == "5") %>% select(orgunit_level:orgunit_name) %>% print()
zaf5uid <- c(zaf5op$orgunit_uid)
unique(zaf5op$orgunit_name)

zaf4op <- df_orgs$zaf_orgs %>%
  mutate(orgunit_parent  = str_to_title(orgunit_parent),
         orgunit_parent  = str_replace(orgunit_parent, "\\s\\s", "\\s"), 
         orgunit_name  = str_to_title(orgunit_name),
         orgunit_name  = str_replace(orgunit_name, "\\s\\s", "\\s")) %>% print() %>%
  filter(orgunit_level  == "4") %>% select(orgunit_level:orgunit_name) %>% print()
zaf4uid <- c(zaf4op$orgunit_uid)
unique(zaf4op$orgunit_name)
################################################################################



# for most level 3 that match_level 5, use snu_2_id -----------------------
zaf5<- zaf_info %>% filter(snu_2_id %in% zaf5uid, snu_2!="") %>% select(-snu_1_id) %>% 
  rename(orgunit = snu_2, orgunituid = snu_2_id)
scales::percent(nrow(zaf5)/nrow(zaf_info))

# for level 3 that doesn't match level 5, match by snu_2 id --------
zaf5m1 <- zaf_info %>% filter(!snu_2_id %in% zaf5uid, snu_2 != "") %>% 
  rename(orgunit_name = snu_2) %>% glimpse()
scales::percent(nrow(zaf5m1)/nrow(zaf_info))
nrow(zaf5m1)

unique(zaf5m1$orgunit_name)

#identify and resolve any failed matches
zaf41 <- zaf5m1 %>%
  anti_join(zaf5op) %>% print()
#resolve discrepancies

#now match
zaf5m <- zaf5m1 %>% inner_join(zaf5op) %>% # or inner if there are non-matches 
  select(-snu_1_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching


#check for 1:many matches
zaf5m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
  filter(n()>1)


#check for unmatched
zaf5m %>% filter(is.na(orgunituid)) 




zaf <- bind_rows(zaf5, zaf5m) %>% select(-contains("snu")) 
#check to see if number of rows matches source
nrow(zaf) - nrow(zaf_info)


#later bind country dfs together

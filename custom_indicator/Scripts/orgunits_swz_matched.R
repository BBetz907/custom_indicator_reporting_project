
# obtain metadata from infolink -------------------------------------------


#ideally we'd run this after mech ref table merge and before data checking. First the 
#issue with the data source must be solved for mech_ref...R
# swz_info <- complete_clean_data %>% filter(country=="Nepal") %>% 
#   select(snu_1:snu_3_id, value) %>% group_by(across(c(-value))) %>% summarise(value = sum(value), .groups = "drop") %>% 
#   clean_names() %>% 
#   glimpse()

# user purr to create DF for each country, named after each count --------
swz_info <- complete_clean_data %>% filter(country=="Eswatini") %>%
  clean_names() %>% 
  mutate(snu_2 = recode(snu_2, "Siphocosini" = "Siphocosini Inhkuhndla")) %>%
  glimpse()

table(swz_info$snu_2) #snu2 level should match to level 5 in datim
#table(swz6op$orgunit_parent)

# get orgunit levels to match and join ------------------------------------
swz5op <- df_orgs$swz_orgs %>%
  mutate(orgunit_parent  = str_to_title(orgunit_parent),
         orgunit_parent  = str_replace(orgunit_parent, "\\s\\s", "\\s"), 
         orgunit_name  = str_to_title(orgunit_name),
         orgunit_name  = str_replace(orgunit_name, "\\s\\s", "\\s")) %>% print(n=641) %>%
  filter(orgunit_level  == "5") %>% select(orgunit_level:orgunit_name)
swz5uid <- c(swz5op$orgunit_uid)
swz5org <- c(swz5op$orgunit_name)

swz4op <- df_orgs$swz_orgs %>%
  mutate(orgunit_parent  = str_to_title(orgunit_parent),
         orgunit_parent  = str_replace(orgunit_parent, "\\s\\s", "\\s"), 
         orgunit_name  = str_to_title(orgunit_name),
         orgunit_name  = str_replace(orgunit_name, "\\s\\s", "\\s")) %>% print(n=641) %>%
  filter(orgunit_level  == "4") %>% select(orgunit_level:orgunit_name)
swz4org <- c(swz4op$orgunit_name)
swz4uid <- c(swz4op$orgunit_uid)

################################################################################
nrow(swz_info)


# for most snu 2 that match_level 5, use snu_2_id -----------------------
swz5<- swz_info %>% filter(snu_2_id %in% swz5uid, snu_2!="") %>% 
  select(-snu_4_id, -snu_3_id) %>% 
  rename(orgunit_uid  = snu_2_id) %>% inner_join(swz5op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(swz5)/nrow(swz_info))
nrow(swz5)

# for snu 2 that doesn't match level 5, match by snu_2 name --------
swz5m1 <- swz_info %>% filter(!snu_2_id %in% swz5uid, snu_2 != "") %>% rename(orgunit_name = snu_2)
scales::percent(nrow(swz5m1)/nrow(swz_info), accuracy = 3)
nrow(swz5m1)

#identify and resolve any failed matches
swz5m1 %>% 
  anti_join(swz5op) %>% select(contains("snu"), contains("org"), indicator, value) %>% print()
#resolve discrepancies
#Kumethula


#now match
swz5m <- swz5m1 %>% inner_join(swz5op) %>% # or inner if there are non-matches
  select(-snu_3_id:-snu_4_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
nrow(swz5m)

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


# level 4
swz4 <- swz5m1 %>% 
  anti_join(swz5op) %>%  filter(snu_1 %in% swz4org)  |> 
  select(-snu_4_id, -snu_3_id, -snu_2_id, -orgunit_name, -snu_3, -snu_4, - snu_1_id) %>% 
  mutate(orgunit_uid = case_when(snu_1 == "Shiselweni" ~ "qRppsyyTP4A")) %>% 
  rename(orgunit_name = snu_1) |> 
  inner_join(swz4op) |> 
  glimpse()
nrow(swz4)
  


swz5 |> glimpse()
swz <- bind_rows(swz5, swz5m, swz4) %>% select(-contains("snu"))
#check to see if number of rows matches source
nrow(swz) - nrow(swz_info)

# swz
# swz5
# swz5m
# swz4
#later bind country dfs together

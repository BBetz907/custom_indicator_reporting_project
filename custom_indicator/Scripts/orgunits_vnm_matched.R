vnm_info <- complete_clean_data %>% filter(country=="Vietnam") %>%
  # mutate(snu_3 = recode(snu_3,
  #                       "" = "")
  #   # snu_3 = str_to_title(snu_3),
  # ) %>%
  glimpse()

snu1 <- unique(vnm_info$snu_1) %>% print()
snu2 <- c(unique(vnm_info$snu_2)) %>% print()
snu3 <- c(unique(vnm_info$snu_3)) %>% print()

# get orgunit levels to match and join ------------------------------------
vnm6op <- df_orgs$vnm_orgs %>% filter(orgunit_level  == "6") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name)
vnm6uid <- c(vnm6op$orgunit_uid)
unique(vnm6op$orgunit_name) 

vnm5op <- df_orgs$vnm_orgs %>% filter(orgunit_level  == "5") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name)
vnm5uid <- c(vnm5op$orgunit_uid)
unique(vnm5op$orgunit_name)

df_orgs$vnm_orgs %>% filter(orgunit_name %in% snu3) #snu3 should match datim level 6
df_orgs$vnm_orgs %>% filter(orgunit_name %in% snu2) #snu2 should match datim level 5

# for most level 3 vnmt match_level 6, use snu_3 id-----------------------
vnm6 <- vnm_info %>% filter(snu_3_id %in% vnm6uid, snu_3!="") %>% select(-c(snu_1_id, snu_2_id)) %>% 
  rename(orgunit_uid = snu_3_id)  %>% inner_join(vnm6op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(vnm6)/nrow(vnm_info))

# for level 3 vnmt doesn't match level 6, match by snu_2 name --------
vnm6m1 <- vnm_info %>% filter(!snu_3_id %in% vnm6uid, snu_3 == "")
scales::percent(nrow(vnm6m1)/nrow(vnm_info))
nrow(vnm6m1)

# for most level 2 vnmt match_level 5, use snu_2 id-----------------------
vnm5 <- vnm6m1 %>% filter(snu_2_id %in% vnm5uid, snu_2!="") %>% select(-c(snu_1_id)) %>% 
  rename(orgunit_uid = snu_2_id)  %>% inner_join(vnm5op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(vnm5)/nrow(vnm_info))
nrow(vnm5)

#non-matching
vnm5m1 <- vnm6m1 %>% filter(!snu_2_id %in% vnm5uid, snu_2!="") %>% select(-c(snu_1_id)) %>% 
  rename(orgunit_uid = snu_2_id)  %>% anti_join(vnm5op) %>%  
  rename(snu_2_id = orgunit_uid) |> glimpse()

vnm51 <- vnm5m1 |> rename(orgunit_name = snu_2, orgunit_parent = snu_1) |>  inner_join(vnm5op) |> 
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) |> 
glimpse()

vnm <- bind_rows(vnm5, vnm6, vnm51) %>% select(-contains("snu")) %>% 
  glimpse() 
#check to see if number of rows matches source
nrow(vnm) - nrow(vnm_info)

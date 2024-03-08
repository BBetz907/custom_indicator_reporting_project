
# obtain metadata from infolink -------------------------------------------


# user purr to create DF for each country, named after each count --------
bdi_info <- complete_clean_data %>% filter(country=="Burundi") %>%
  mutate(snu_2 = recode(snu_2, 
                        "DS Nyanza-LAC" = "DS Nyanza-Lac"),
         snu_4 = recode(snu_4,
                        "CDS BubanzaI" = "CDS Bubanza I",
                        "CDS Mere de la misercorde" = "CDS Mère de la misercorde",
                        "CDS Don Beni" = "CDS Don Béni",
                        "CDS ABCMAV (Rohero)" = "CDS ABCMAV",
                        "CDS Abcmav (Gabiro)" = "CDS Abcmav",
                        "CDS Musenyi agree" = "CDS Musenyi Agrée",
                        "CDS ABUBEF Rumonge" = "CDS ABUBEF RUMONGE",
                        "Nouvelle Esperance" = "Nouvelle Espérance",
                        "Nouvelle Esperance (Buyenzi)" = "Nouvelle Espérance"
  #                       "CEEDeB Duekoue" = "Centre d’Eveil et d’Encadrement pour le Developpement a la Base de Duekoue (CEEDeB)",
  #                       "CMS PRIVE-ONG ASAPSU de Yamoussoukro" = "Centre Medico-Social PRIVE-ONG ASAPSU de YAMOUSSOUKRO")
  #   # snu_3 = str_to_title(snu_3),
  )) %>%
glimpse()

snu4 <- c(unique(bdi_info$snu_4)) %>% print()
snu3 <- c(unique(bdi_info$snu_3)) %>% print()



# get orgunit levels to match and join ------------------------------------

bdi7op <- df_orgs$bdi_orgs %>% filter(orgunit_level  == "7") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
bdi7uid <- c(bdi7op$orgunit_uid)
bdi7name <- unique(c(bdi7op$orgunit_name))

bdi6op <- df_orgs$bdi_orgs %>% filter(orgunit_level  == "6") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
bdi6uid <- c(bdi6op$orgunit_uid)
bdi6name <- unique(c(bdi6op$orgunit_name))

bdi5op <- df_orgs$bdi_orgs %>% filter(orgunit_level  == "5") %>% arrange(orgunit_name) %>% select(orgunit_level:orgunit_name) 
bdi5uid <- c(bdi5op$orgunit_uid)
bdi5name <- unique(c(bdi5op$orgunit_name))

bdi7op %>% filter(orgunit_name %in% snu4) #snu4 should match datim level 7 for most
bdi6op %>% filter(orgunit_name %in% snu4) #snu3 should match datim level 6 for some (H...)


################################################################################

bdi <- bdi_info %>% filter(snu_2 %in% bdi5name) %>% 
  rename(orgunit_name = snu_2) %>%
  inner_join(bdi5op) %>% 
  rename(orgunit = orgunit_name, orgunituid = orgunit_uid) %>% 
  select(-contains("snu"), -orgunit_parent) %>%
  mutate(orgunit_parent = orgunit) %>%
  glimpse()
# 
# # for most level 3 that match_level 6, use snu_4 id-----------------------
# bdi7 <- bdi_info %>% filter(snu_4_id %in% bdi7uid, snu_4!="") %>% select(-snu_1_id) %>%
#   rename(orgunit_uid = snu_4_id)  %>% inner_join(bdi7op) %>%  
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
# scales::percent(nrow(bdi7)/nrow(bdi_info))
# nrow(bdi7)
# 
# #check for 1:many matches
# duplicates_7id <- bdi7 %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
#   filter(n()>1)
# nrow(duplicates_7id)
# 
# # for level 4 that doesn't match level 7, match by snu_4 name --------
# bdi7m1 <- bdi_info %>% filter(!snu_4_id %in% bdi7uid) %>% rename(orgunit_name = snu_4)
# scales::percent(nrow(bdi7m1)/nrow(bdi_info))
# nrow(bdi7m1)
# 
# #identify and resolve any failed matches
# non_matched_snu_4 <- bdi7m1 %>% rename(orgunit_parent = snu_3) %>%
#   anti_join(bdi7op, by = c("orgunit_name",  "orgunit_parent")) %>% glimpse()
# #resolve discrepancies
# non_matched_snu_4_name <- unique(c(non_matched_snu_4$orgunit_name)) %>% print()
# non_matched_snu_4 %>% filter(orgunit_name %in% non_matched_snu_4_name,
#                              !str_detect(orgunit_name, "H "),
#                              !orgunit_name == "") %>% select(orgunit_name, orgunit_parent) %>% group_by_all() %>% summarise(.groups = "drop") %>% print(n=36)
# 
# test2 <- bdi7op %>% select(orgunit_name, orgunit_parent) %>%
#   filter(str_detect(orgunit_name, "Kayanza")) %>% arrange((orgunit_name))
# test2
# 
# 
# # match - test run - exclude duplicates later
# bdi7m_predup <- bdi7m1 %>% 
#   rename(orgunit_parent = snu_3) %>%
#   inner_join(bdi7op, by = c("orgunit_name", "orgunit_parent")) %>% # or inner if there are non-matches 
#   select(-snu_1_id:-snu_4_id) %>%
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
#   glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
# nrow(bdi7m_predup)
# scales::percent(nrow(bdi7m_predup)/nrow(bdi_info))
# 
# #check for 1:many matches
# duplicates_7name <- bdi7m_predup %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>% 
#   filter(n()>1)
# dup7n <- unique(c(duplicates_7name$orgunit)) %>% print()
# 
# #########deal with duplicates
# bdi7op %>% filter(orgunit_name %in% dup7n) %>% arrange(orgunit_name, orgunit_parent) %>% print(n=86)
# bdi7m1 %>% filter(orgunit_name %in% dup7n) %>% select(contains("snu"), contains("org")) %>% group_by_all() %>% summarise(.groups = "drop") %>%
#   arrange(orgunit_name) %>% print(n=86)
# 
# #exclude duplicated rows from completed output
# bdi7m <- bdi7m_predup %>% filter(!orgunit %in% dup7n)
# nrow(bdi7m)
# scales::percent(nrow(bdi7m)/nrow(bdi_info))
# scales::percent((nrow(bdi7m)+nrow(bdi7))/nrow(bdi_info))
# nrow(bdi_info) - (nrow(bdi7m)+nrow(bdi7))
# 
# 
# #save combination of orgunit and parents that were not included and merge at snu_2, datim_level 5?
# bdi7m_nunq <- bdi7m_predup %>% filter(orgunit %in% dup7n) %>% 
#   rename(snu_4 = orgunit, snu_3 = orgunit_parent) %>% select(-contains("org")) %>% 
#   group_by(across(-c(value))) %>% summarise(value = mean(value), .groups = "drop") %>% glimpse() 
# nrow(bdi7m_nunq)
# 
# bdi7m_nunq_combos <- bdi7m_nunq %>%  select(snu_4, snu_3) %>% group_by_all() %>% summarise(.groups = "drop")
# nunq_combo4 <- unique(c(bdi7m_nunq_combos$snu_4))
# nunq_combo3 <- unique(c(bdi7m_nunq_combos$snu_3))
# 
# 
# 
# 
# bdi_5_prematch <- bdi_info %>% filter(snu_4 %in% nunq_combo4, snu_3 %in% nunq_combo3)  %>% glimpse()
# non_matched_snu_4_pre <- non_matched_snu_4 %>% rename(snu_3 = orgunit_parent, snu_4 = orgunit_name) %>% glimpse()
# bdi_unmerged_at_snu_4 <- bind_rows(bdi_5_prematch, non_matched_snu_4_pre) %>% glimpse()
# 
# 
# 
# # ##############################################################################
# glimpse(bdi7m1)
# # for snu3 3 that do not match_level 6, use snu_2 id-----------------------
# bdi6 <- bdi_unmerged_at_snu_4 %>% filter(snu_3_id %in% bdi6uid, snu_3 %in% bdi6name) %>% select(-snu_1_id:-snu_2_id ) %>% 
#   rename(orgunit_uid = snu_3_id)  %>% inner_join(bdi6op) %>%
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name)
# scales::percent(nrow(bdi6)/nrow(bdi_info))
# nrow(bdi6)
# #remaining
# nrow(bdi_info) - (nrow(bdi7m)+nrow(bdi7)+nrow(bdi6))
# 
# # for snu3 that doesn't match level 6, match by snu_2 name --------
# bdi6m1 <- bdi_unmerged_at_snu_4 %>% filter(!snu_3_id %in% bdi6uid) %>% rename(orgunit_name = snu_3) %>% glimpse()
# scales::percent(nrow(bdi6m1)/nrow(bdi_info))
# nrow(bdi6m1)
# 
# #identify and resolve any failed matches
# non_matched_snu_2 <- bdi5m1 %>%
#   anti_join(bdi5op) %>% 
#   arrange(orgunit_name) %>% glimpse()
# unique(non_matched_snu_2$orgunit_name)
# unique(bdi5op$orgunit_name)
# 
# #now match
# bdi6m <- bdi6m1 %>% inner_join(bdi6op) %>% # or inner if there are non-matches
#   select(-snu_1_id:-snu_3_id) %>%
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
#   glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
# nrow(bdi6m)
# scales::percent(nrow(bdi6m)/nrow(bdi_info))
# 
# #check for 1:many matches
# bdi6m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>%
#   filter(n()>1)
# 
# 
# #check for unmatched
# bdi6m %>% filter(is.na(orgunituid))
# 
# ###############################################################################
# # for rows with multiple matches at snu3 or 4, match snu_2 to level 5 by id-----------------------
# bdi_5_prematch <- bdi_info %>% filter(snu_4 %in% nunq_combo4, snu_3 %in% nunq_combo3)  %>% glimpse()
# 
# bdi_info %>% filter(snu_4 == "" | snu_3 == "")
# 
# bdi5 <- bdi_5_prematch %>% filter(snu_2_id %in% bdi5uid) %>% select(-snu_1_id) %>% 
#   rename(orgunit_uid = snu_2_id)  %>% inner_join(bdi5op) %>%
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name)
# scales::percent(nrow(bdi5)/nrow(bdi_5_prematch))
# 
# # for rows with multiple matches at snu3 or 4, match snu_2 to level 5 by name
# bdi5m1 <- bdi_5_prematch %>% filter(!snu_2_id %in% bdi5uid) %>% rename(orgunit_name = snu_2) %>% arrange(orgunit_name) %>% glimpse()
# scales::percent(nrow(bdi5m1)/nrow(bdi_info))
# nrow(bdi5m1)
# 
# unique(bdi5m1$orgunit_name)
# unique(bdi5op$orgunit_name)
# 
# # #identify and resolve any failed matches
# non_matched_snu_4_6 <- bdi6m1 %>%
#   anti_join(bdi6op) %>%
#   arrange(orgunit_name) %>% glimpse()
# unique(non_matched_snu_4_6$orgunit_name)
# 
# #now match
# bdi5m <- bdi5m1 %>% inner_join(bdi5op, by = "orgunit_name") %>%  # or inner if there are non-matches
#   select(-snu_1_id:-snu_4_id, -snu_4, -snu_3) %>%
#   rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
#   glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching
# nrow(bdi5m)
# scales::percent(nrow(bdi5m)/nrow(bdi_info))
# 
# #check for 1:many matches
# bdi6m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>%
#   filter(n()>1)
# 
# 
# #check for unmatched
# bdi6m %>% filter(is.na(orgunituid))
# 
# ###############################################################################
# 
# 
# bdi <- bind_rows(bdi7, bdi7m, bdi6, bdi6m, bdi5, bdi5m) %>% select(-contains("snu")) %>% 
#   glimpse() 
# #check to see if number of rows matches source
# nrow(bdi) - nrow(bdi_info)


#later bind country dfs together

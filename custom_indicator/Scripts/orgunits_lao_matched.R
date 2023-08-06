
# get orgunit levels to match and join ------------------------------------
lao7op <- df_orgs$lao_orgs %>% filter(orgunit_level == 7) %>% select(orgunit_level:orgunit_name) %>% arrange(orgunit_name)
lao7uid <- c(lao7op$orgunit_uid)
lao7name <- unique(c(lao7op$orgunit_name))

lao5op <- df_orgs$lao_orgs %>% filter(orgunit_level == 5) %>% select(orgunit_level:orgunit_name) %>% arrange(orgunit_name)
lao5uid <- c(lao5op$orgunit_uid)
lao5name <- unique(c(lao5op$orgunit_name))
################################################################################
lao5name

lao_info <- complete_clean_data %>% filter(country=="Laos") %>%
  # mutate(snu_3 = recode(snu_3,
  #                       "RSUD Kota Depok" = "Rumah Sakit Umum Daerah Depok"),
  #        snu_3 = if_else(!snu_3 %in% lao7name, str_replace(snu_3, "RSUK\\s", "Rumah Sakit Umum Kabupaten "), snu_3),

  #         ) %>%
  clean_names() %>% glimpse()


unique(lao_info$snu_3)
unique(lao7name)

#snu3 should match level 7, usually or snu1 to level 5


# try to match with snu_3_id -----------------------
lao7 <- lao_info %>% filter(snu_3 %in% lao7name, snu_3!="") %>% 
  rename(orgunit_name  = snu_3) %>% inner_join(lao7op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>% 
  select(-contains("snu")) %>% glimpse()
scales::percent(nrow(lao7)/nrow(lao_info))
nrow(lao7)


#check for 1:many matches
lao7m_dup <- lao7 %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>%
  filter(n()>1) %>% ungroup() %>% print(n=33)

  
# for snu 3 that doesn't match level 7, match by snu_1/id to level 6 since the other levels are missing --------
lao5 <- lao_info %>% filter(!(snu_3 %in% lao7name & snu_3!=""), snu_1 %in% lao5name) %>%  
  inner_join(lao5op, by = c("snu_1" = "orgunit_name")) %>% 
  rename(orgunituid = orgunit_uid, orgunit = snu_1) %>%
  select(-contains("snu")) %>% glimpse()
nrow(lao5)
scales::percent(nrow(lao5)/nrow(lao_info), accuracy = 1)
nrow(lao_info)

lao5_dup <- lao5 %>%  select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>%
  filter(n()>1) %>% ungroup() %>% print(n=33)



lao <- bind_rows(lao7, lao5) %>% select(-contains("snu")) %>% glimpse()
#check to see if number of rows matches source
nrow(lao) - nrow(lao_info) 
#number of rows

lao_info |> print(n=130)

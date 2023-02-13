
# obtain metadata from infolink -------------------------------------------


#ideally we'd run this after mech ref table merge and before data checking. First the 
#issue with the data source must be solved for mech_ref...R
# lao_info <- complete_clean_data %>% filter(country=="Nepal") %>% 
#   select(snu_1:snu_3_id, value) %>% group_by(across(c(-value))) %>% summarise(value = sum(value), .groups = "drop") %>% 
#   clean_names() %>% 
#   glimpse()

# user purr to create DF for each country, named after each count --------

# get orgunit levels to match and join ------------------------------------
lao7op <- lao_6_7 %>% filter(orgunit_level == 7) %>% select(orgunit_level:orgunit_name) %>% arrange(orgunit_name)
lao7uid <- c(lao7op$orgunit_uid)
lao7name <- unique(c(lao7op$orgunit_name))

lao6op <- lao_6_7 %>% filter(orgunit_level == 6) %>% select(orgunit_level:orgunit_name) %>% arrange(orgunit_name)
lao6uid <- c(lao6op$orgunit_uid)
lao6name <- unique(c(lao6op$orgunit_name))
################################################################################


lao_info <- complete_clean_data %>% filter(country=="Laos") %>%
  # mutate(snu_3 = recode(snu_3,
  #                       "Puskesmas Tambora" = "Puskesmas Tambora (Klinik CINTTA)",
  #                       "RSUD Leuwiliang Bogor" = "Rumah Sakit Umum Daerah Leuwiliang Bogor",
  #                       "RSJ Dr. H. Marzoeki Mahdi" = "Rumah Sakit Jiwa Dr. H. Marzoeki Mahdi",
  #                       "RS Gatot Subroto" = "RS Umum PAD Gatot Soebroto",
  #                       "RSUD Kota Bekasi" = "Rumah Sakit Umum Kota Bekasi",
  #                       "RSUD Kota Depok" = "Rumah Sakit Umum Daerah Depok"),
  #        snu_3 = if_else(!snu_3 %in% lao7name, str_replace(snu_3, "RSUK\\s", "Rumah Sakit Umum Kabupaten "), snu_3),
  #        snu_3 = if_else(!snu_3 %in% lao7name, str_replace(snu_3, "RSUD\\s", "Rumah Sakit Umum Daerah "), snu_3),
  #        snu_3 = if_else(!snu_3 %in% lao7name, str_replace(snu_3, "RSU\\s", "Rumah Sakit Umum "), snu_3),
  #        snu_3 = if_else(!snu_3 %in% lao7name, str_replace(snu_3, "RS\\s", "Rumah Sakit "), snu_3),
  #         ) %>%
  clean_names() %>% glimpse()


unique(lao_info$snu_3)
unique(lao7name)

#snu3 should match level 7, usually


# try to match with snu_3_id -----------------------
lao7 <- lao_info %>% filter(snu_3_id %in% lao7uid, snu_3!="") %>% 
  rename(orgunit_uid  = snu_3_id) %>% inner_join(lao7op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(lao7)/nrow(lao_info))
nrow(lao7)

# for level 3 that doesn't match level 3, match by snu_3 id --------
lao7m1 <- lao_info %>% filter(!snu_3_id %in% lao7uid, snu_3 != "") %>% rename(orgunit_name = snu_3)
scales::percent(nrow(lao7m1)/nrow(lao_info), accuracy = 1)
nrow(lao7m1)

#identify and resolve any failed matches
test <- lao7m1 %>% 
  anti_join(lao7op) %>% arrange(orgunit_name) %>% glimpse()


#now match
lao7m <- lao7m1 %>% inner_join(lao7op) %>% # or inner if there are non-matches
  select(-snu_4_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching

nrow(lao7m)
scales::percent(nrow(lao7m)/nrow(lao_info), accuracy = 1)
nrow(lao_info)

#check for 1:many matches
lao7m_dup <- lao7m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>%
  filter(n()>1) %>% ungroup() %>% print(n=33)



lao <- bind_rows(lao7, lao7m) %>% select(-contains("snu")) %>% glimpse()
#check to see if number of rows matches source
nrow(lao) - nrow(lao_info)

# lao %>% write_csv("Data/laos_custom_data.csv")
#later bind country dfs together

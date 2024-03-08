
# obtain metadata from infolink -------------------------------------------


#ideally we'd run this after mech ref table merge and before data checking. First the 
#issue with the data source must be solved for mech_ref...R
# idn_info <- complete_clean_data %>% filter(country=="Nepal") %>% 
#   select(snu_1:snu_3_id, value) %>% group_by(across(c(-value))) %>% summarise(value = sum(value), .groups = "drop") %>% 
#   clean_names() %>% 
#   glimpse()

# user purr to create DF for each country, named after each count --------

# get orgunit levels to match and join ------------------------------------
idn7op <- df_orgs$idn_orgs %>% select(orgunit_level:orgunit_name)
idn7uid <- c(idn7op$orgunit_uid)
idn7name <- unique(c(idn7op$orgunit_name))


################################################################################


idn_info <- complete_clean_data %>% filter(country=="Indonesia") %>%
  mutate(snu_3 = recode(snu_3,
                        "Puskesmas Tambora" = "Puskesmas Tambora (Klinik CINTTA)",
                        "RSUD Leuwiliang Bogor" = "Rumah Sakit Umum Daerah Leuwiliang Bogor",
                        "RSJ Dr. H. Marzoeki Mahdi" = "Rumah Sakit Jiwa Dr. H. Marzoeki Mahdi",
                        "RS Gatot Subroto" = "RS Umum PAD Gatot Soebroto",
                        "RSUD Kota Bekasi" = "Rumah Sakit Umum Kota Bekasi",
                        "RSUD Kota Depok" = "Rumah Sakit Umum Daerah Depok"),
         snu_3 = if_else(!snu_3 %in% idn7name, str_replace(snu_3, "RSUK\\s", "Rumah Sakit Umum Kabupaten "), snu_3),
         snu_3 = if_else(!snu_3 %in% idn7name, str_replace(snu_3, "RSUD\\s", "Rumah Sakit Umum Daerah "), snu_3),
         snu_3 = if_else(!snu_3 %in% idn7name, str_replace(snu_3, "RSU\\s", "Rumah Sakit Umum "), snu_3),
         snu_3 = if_else(!snu_3 %in% idn7name, str_replace(snu_3, "RS\\s", "Rumah Sakit "), snu_3),
          ) %>%
  clean_names() %>% glimpse()


unique(idn_info$snu_3)
#unique(idn_6_7$orgunit_name)

#snu3 should match level 7, usually





# try to match with snu_3_id -----------------------
idn7 <- idn_info %>% filter(snu_3_id %in% idn7uid, snu_3!="") %>% 
  rename(orgunit_uid  = snu_3_id) %>% inner_join(idn7op) %>%  
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) 
scales::percent(nrow(idn7)/nrow(idn_info))
nrow(idn7)

# for level 3 that doesn't match level 3, match by snu_3 id --------
idn7m1 <- idn_info %>% filter(!snu_3_id %in% idn7uid, snu_3 != "") %>% rename(orgunit_name = snu_3)
scales::percent(nrow(idn7m1)/nrow(idn_info), accuracy = 1)
nrow(idn7m1)

#identify and resolve any failed matches
test <- idn7m1 %>% 
  anti_join(idn7op) %>% arrange(orgunit_name) %>% glimpse()
#resolve discrepancies
unique(test$orgunit_name)
# test2 <- idn_7 %>% select(orgunit_name) %>%
#   filter(str_detect(orgunit_name, "Subroto")) %>% arrange((orgunit_name))
# unique(test2$orgunit_name)


#now match
idn7m <- idn7m1 %>% inner_join(idn7op) %>% # or inner if there are non-matches
  select(-snu_4_id) %>%
  rename(orgunituid = orgunit_uid, orgunit = orgunit_name) %>%
  glimpse() #check if the tibble nrow matches the previous count. if it exceeds there is some double matching

nrow(idn7m)
scales::percent(nrow(idn7m)/nrow(idn_info), accuracy = 1)
nrow(idn_info)

#check for 1:many matches
idn7m_dup <- idn7m %>% select(value, indicator, age, sex, otherdisaggregate, numdenom, population, orgunit) %>% group_by_all() %>%
  filter(n()>1) %>% ungroup() %>% print(n=33)

(unique(idn7m_dup$indicator))

idn7m_dups <- c(unique(idn7m_dup$orgunit))
idn7m_dups

idn7op %>% filter(orgunit_name %in% idn7m_dups)  #test against ref file
idn7m1 %>% filter(orgunit_name %in% idn7m_dups, indicator == "PrEP_OFFER") %>% 
  print(n=303) #test against data file 

#potential duplicates were non-unique rows in the data file, not in the merge file


#check for unmatched
idn7m %>% filter(is.na(orgunituid))


idn <- bind_rows(idn7, idn7m) %>% select(-contains("snu")) 
#check to see if number of rows matches source
nrow(idn) - nrow(idn_info)


#later bind country dfs together

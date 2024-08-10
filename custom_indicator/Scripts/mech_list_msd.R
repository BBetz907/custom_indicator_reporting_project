# generate list of FHI mechanisms for FY24, by country --------------------------------
mech_id_ref_datim_fy24 <- read_csv("https://www.datim.org/api/sqlViews/fgUtV6e9YIX/data.csv") %>% 
  filter(str_detect(partner, "^FHI|^Family\\sHealth\\sInt"),
         agency == "USAID",
         startdate < "2024-10-01",
         enddate >= "2023-10-01",
         # !str_detect(mechanism, "(?<=\\s\\-\\s)GHSC-QA|(?<=\\s\\-\\s)Quality\\sAssurance|"),
         str_detect(mechanism, "EpiC|Epic|EAWA")) %>% 
  rename(mech_code = code) %>%
  left_join(rp_mechs) %>% 
  mutate(country = if_else(is.na(country), ou, country),
         mech_name = str_after_nth(mechanism, "\\s\\-\\s", 2)) %>%
  select(-uid, -primeid,-agency, -enddate, -mechanism) %>% print(n=42)


# generate list of FHI mechanisms for FY23 that list OU but not country --------------------------------
nocountry <- mech_id_ref_datim_fy24 %>%   
  filter(is.na(country)) %>% select(-country) %>%
  print(n=24)

# to merge with list of mechanisms with no country, prepare list from the MSD --------------------------------
country_from_msd <- msd %>% 
  filter(operatingunit %in% rp,
         str_detect(partner, "^FHI|^Family\\sHealth\\sInt")) %>% 
  select(country, mech_code, fy) %>% 
  group_by_all() %>% summarise(.groups = "drop") %>%
  arrange(country, mech_code, fy) %>% print()

# country_from_msd_2023 <- country_from_msd %>% filter(fy == max(fy)) %>% select(-fy) %>% group_by_all() %>% summarise(.groups="drop")
##note some still show multiple IMs per country

# #generate 2 splits, matched (4bind) and unmatched
# rp_2023 <- nocountry %>% left_join(country_from_msd_2023, by = "mech_code")
# 
# rp_2023_4bind <- rp_2023 %>%
#   filter(!is.na(country)) %>% print()
# 
# rp_2023_2fix <- rp_2023 %>%
#   filter(is.na(country)) %>% 
#   select(-country) %>% print()
# 
# #join FY22 summary data with unmatched entries
# country_from_msd_2022 <- country_from_msd %>% filter(fy == 2022) %>% select(-fy)
# 
# #generate 2 splits, matched (4bind) and unmatched
# rp_2022_4bind <- rp_2023_2fix %>% left_join(country_from_msd_2022, by = "mech_code") %>% print()
# 
# #bind both
# rp_4bind <- bind_rows(rp_2023_4bind, rp_2022_4bind)

# bind table to list all mech names by country ----------------------------

mech_id_ref_table_fy24 <- mech_id_ref_datim_fy24 %>%   
  filter(!is.na(country)) %>% #remove blanks to be added back in next step
  bind_rows(rp_4bind) %>%
  arrange(ou, country, mech_code) %>% 
  select(-startdate)

# adding/cleaning countries & mech codes

mech_id_ref_table_fy24 <- mech_id_ref_table_fy24 %>% 
  mutate(country = str_replace_all(country, fixed("Democratic Republic of the Congo"), "DRC")) %>%
  filter(mech_code != 84522) %>%
  filter(!(str_detect(mech_name, "EAWA") & country %in% c("Burkina Faso", "Togo"))) %>% #Benin's only mechanism is EAWA, change this to only filter out for Togo and Burkina
  add_row(mech_code = "83017", partner = "Family Health International", ou = "Pacific Region",
                                         country = "Philippines", mech_name = "FHI 360 Epic") %>% print(n=50)
  
#note some countries may have more than 1 mechanism per country. To avoid duplicating when merging by country (to obtain the mech information)
#You will need to generate a list of countries with multiple mechanisms and ascertain from the data or, more likely, from FHI360 (Amy's team)
#which mechanisms are reporting for each country/geographic unit. Then filter out the mechanisms which exist but are note reporting the data. If you do not do this you will multiply the values 
#by the number of merges by country and overreport. 

#please filter below before using the final reference table

# export -------------------------------------------------------------------


# write_csv(mech_id_ref_table_fy24, "./Dataout/mech_id_ref_table_fy24.csv")


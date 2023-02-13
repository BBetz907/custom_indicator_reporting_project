countries <- c(unique(complete_clean_data$country))


orgunit_levels <- grabr::get_levels(
  username = datim_user(),
  password = datim_pwd(),
  baseurl = "https://final.datim.org/") %>% 
  filter(countryname %in% countries) %>% 
  arrange(operatingunit, countryname) %>% print(n=23)



# write_csv(orgunit_levels, "Data/orgunit_levels_by_ou.csv")

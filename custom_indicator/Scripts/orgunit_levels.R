countries <- c(unique(complete_clean_data$country)) %>% print()
#append with full name or mutate and renam



## OKTA VERIFY NOW PROHIBITS THESE APIS. Do not run/overwrite. OBTAIN MANUALLY. ------------------------------ 
# orgunit_levels <- grabr::get_levels(
#   username = datim_user(),
#   password = datim_pwd()) %>% 
#   mutate(countryname = recode(countryname, "Democratic Republic of the Congo" = "DRC")) %>%
#   filter(countryname %in% countries) %>% 
#   arrange(operatingunit, countryname) %>% print(n=23)

# write_csv(orgunit_levels, "Data/orgunit_levels_by_ou.csv")
# orgunit_levels <- read.csv("Data/orgunit_levels_by_ou.csv")

#df orgs using purr package
# obtain list of countries from cleaned infolink data ---------------------
countries <- complete_clean_data %>% group_by(country) %>% 
  mutate(country = recode(country, "Cote dIvoire" = "Cote d'Ivoire", "DRC" = "Democratic Republic of the Congo")) %>%
  summarise(.groups = "drop")
countries <- c(countries$country)

# get ou tableD from DATIM -------------------------------------------------

#USE PURR TO ITERATE AND PULL LIST FOR ALL COUNTRIES
df_orgs <- map(countries, orgs_func)
df_orgs <- df_orgs %>% set_names(., nm = map(df_orgs, ~glue("{tolower({first({.x$regionorcountry_code})})}_orgs")))

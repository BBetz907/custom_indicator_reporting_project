
# obtain list of countries from cleaned infolink data ---------------------
countries <- complete_clean_data %>% group_by(country) %>% summarise(.groups = "drop")
countries <- c(countries$country)


# get ou tableD from DATIM -------------------------------------------------

org_url <- "https://www.datim.org/api/sqlViews/DataExchOUs/data?format=json"

load_secrets()


df_ous <- grabr::get_outable(
  username = glamr::datim_user(), 
  password = glamr::datim_pwd()
)
df_ous

#USE PURR TO ITERATE AND PULL LIST FOR ALL COUNTRIES

list_orgs <- df_ous %>% 
  filter(country %in% c("Philippines")) %>% 
  pull(country_iso) %>% 
  paste0(org_url, "&var=OU:", ., "&paging=false") %>% 
  httr::GET(httr::authenticate(user = glamr::datim_user(), 
                               password = glamr::datim_pwd())) %>% 
  httr::content("text") %>% 
  jsonlite::fromJSON(flatten=TRUE) 


df_orgs <- tibble::as_tibble(list_orgs$listGrid$rows, .name_repair = "unique") %>% 
  setNames(list_orgs$listGrid$headers$name) %>% 
  rename_with(.cols = contains("internal_id"),
              .fn = ~str_replace(., "internal_id", "uid")) 

max(df_orgs$orgunit_level)
phl_8 <- df_orgs %>% filter(orgunit_level %in% c(8)) %>% print()


# ultimately bind rows so that we can merge by country, orgunit_pa --------


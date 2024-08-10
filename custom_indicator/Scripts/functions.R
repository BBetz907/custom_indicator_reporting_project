## OKTA VERIFY NOW PROHIBITS THESE APIS. OBTAIN MANUALLY. ------------------------------ 

# #org unit list function
# org_url <- "https://www.datim.org/api/sqlViews/DataExchOUs/data?format=json"
# glamr::load_secrets()
# df_ous <- grabr::get_outable(
#   username = glamr::datim_user(), 
#   password = glamr::datim_pwd())



#org unit table functions
orgunit_clean <- function(df) {df %>% 
    select(-c(regionorcountry_code, regionorcountry_name, moh_id, orgunit_code))}

orgunit_level_sep <- function(df, lev) {
  (df %>% filter(orgunit_level == lev) %>%
     rename(!!paste("orgunit", as.character(lev), sep = "_") := orgunit_name,
            !!paste("orgunit", as.character(lev), "uid", sep = "_") := orgunit_uid,
            !!paste("orgunit", as.character(lev-1), sep = "_") := orgunit_parent,
            !!paste("orgunit", as.character(lev-1), "uid", sep = "_") := orgunit_parent_uid) %>%
     select(-orgunit_level))}


## OKTA VERIFY NOW PROHIBITS THESE APIS. OBTAIN MANUALLY. ------------------------------ 

# orgs_func <- function(ou) {
#   list_orgs <- df_ous %>% 
#     filter(country %in% c(ou)) %>% 
#     pull(country_iso) %>% 
#     paste0(org_url, "&var=OU:", ., "&paging=false") %>% 
#     httr::GET(httr::authenticate(user = glamr::datim_user(), 
#                                  password = glamr::datim_pwd())) %>% 
#     httr::content("text") %>% 
#     jsonlite::fromJSON(flatten=TRUE)
#   
#   df_orgs <- tibble::as_tibble(list_orgs$listGrid$rows, .name_repair = "unique") %>% 
#     setNames(list_orgs$listGrid$headers$name) %>% 
#     rename_with(.colsa = contains("internal_id"),
#                 .fn = ~str_replace(., "internal_id", "uid"))
# }

orgunit_level_list <- function(df, levels) {
  df %>% filter(orgunit_level %in% levels)
}

#data validation
data_check <- function(merge_psnu) {
  merge_psnu %>% group_by(psnu, psnu_uid, indicator, age, sex, population) %>% summarize(value = sum(value))
}


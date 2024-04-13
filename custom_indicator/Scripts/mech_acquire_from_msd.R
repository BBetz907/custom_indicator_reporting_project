
set_pano("bbetz@usaid.gov")

load_secrets()

# set_paths(folderpath_msd = "Data",
#                     folderpath_datim =  "Data",
#                     folderpath_downloads =  "Data")
## comment in/out the above after setting initially


#create active session

sess <- grabr::pano_session(username = pano_user(), password = pano_pwd())

# Extract data items details
url <- "https://pepfar-panorama.org/forms/downloads/"

dir_items <- pano_items(page_url = url, 
                        username = pano_user(),
                        password = pano_pwd()) 

# Extract data items details, and url
dir_mer_path <- dir_items %>%
  filter(str_detect(item, "^MER")) %>%
  pull(path)

# pull latest pOUXim MSD URL ---------------------------------------------------------
url_ou_im <- pano_items(page_url = dir_mer_path) |> filter(str_detect(item, "OU_IM_FY2")) |> pull(path)

# Download most recent PSNUxIM MSD ------------------------------------------------
grabr::pano_download(item_url = url_ou_im, session = sess)




# read OUxIM MSD, filter and condense---------------------------------------------
file <- glamr::return_latest("Data/", "OU_IM_FY2") %>% print()
msd <- read_psd(file, save_rds = TRUE, remove_txt = FALSE) %>%  filter(
  str_detect(standardizeddisaggregate, "KeyPop|Total") == TRUE,
  funding_agency == "USAID") %>% 
  mutate(fy = fiscal_year,
         partner = prime_partner_name) %>% 
  filter(fy>=max(fy)-1) %>%
  select(operatingunit, country, partner, mech_code, mech_name, award_number, fy) %>% 
  group_by_all() %>% summarise(.groups = "drop") %>% glimpse()




# establish parameters ----------------------------------------------------


col_order <- c("reportingperiod", "orgunit", "orgunituid",
               "mech_code", "partner", "ou", "psnu", "indicator",
               "sex", "age", "otherdisaggregate", "population","numdenom", "value")
mission2report <- c("Eswatini", "Cote d'Ivoire")


# bind and process files for submission -----------------------------------------


ci_fhi <- do.call(bind_rows, lapply(ls(pattern = "_merge_psnu"), get)) %>%
  #exclude what country teams report
  filter(!(str_detect(tolower(indicator), "prep") & country == "Botswana"),
         !country %in% mission2report,
         str_to_lower(reportingperiod) == str_replace(current_q, "_", " "))

ci_fhi |> count(str_to_lower(reportingperiod))
# str_replace(current_q, "_", " ")

#prepare data for export by selecting reporting columns and summarizing by reported columns
ci_fhi360 <-  ci_fhi %>%  select(reportingperiod, orgunit, orgunituid,
         mech_code, partner, ou, psnu, indicator,
         sex, age, otherdisaggregate, population,numdenom, value) %>% 
        # mutate() |> 
    group_by(across(-c(value))) %>% summarise(value = sum(value), .groups ="drop") %>% 
  mutate(indicator = recode(indicator, "TX_PVLS_ELIGIBLE_VERIFY" = "TX_PVLS_ELIGIBLE"),
         ou = if_else(psnu == "Greater Accra Region" & orgunituid == "shNTvGHKhCu", "West Africa Region", ou) #unsure why this is not coded but nonetheless, #2023_11_03
         ) %>%
  glimpse()

ci_fhi360 |> count(ou)

ci <- ci_fhi360[,col_order] 

# view reporting content
ci |> mutate(indicator_topic = str_extract(indicator, "PrEP|TX(?!_PVLS_ELIGIBLE)"),
             indicator_topic = case_when(indicator == "TX_PVLS_ELIGIBLE" ~ "Lab",
                                         indicator_topic == "TX" ~ "KP",
                                         .default = indicator_topic)
             ) |> count(ou, indicator_topic) |> 
  mutate(n = case_when(n>0 ~1)) |> 
  pivot_wider(names_from = "indicator_topic", values_from = "n")

# back to code
operating_units <- c(unique(ci$ou))
reportingperiod <- unique(c(ci$reportingperiod))



library(openxlsx)
# define functions for exporting each sheet together in the output -------------------------------
write_excel_output_by_ou <- function(operating_units) {
  
## CIRG Tab
  ### Recode integer as character and rename ou to simplify union of header and data
  ci_str <- ci |> rename(operatingunit=ou) 
  ### Create OU-specific frames to write to separate files
  subset_ci <- subset(ci_str, operatingunit %in% operating_units)
  ### Extract CIRG worksheet headers from template to union with data for each OU
  head3r_cirg <- readxl::read_excel(paste0("Dataout/", "/Template.xlsx"), sheet = "CIRG") |> 
    select(-value) |> mutate(value = 0)
  ### Union header and data values
  cirg_ws <- head3r_cirg |> bind_rows(subset_ci)
  
## meta Tab
  ### Read and group OU and reporting period data to input values into fields
  ci_meta_str <- ci |> group_by(ou, reportingperiod) |> summarise(.groups = "drop")
  ### Create OU-specific frames to write to separate files
  subset_ci_meta <- subset(ci_meta_str, ou %in% operating_units)
  ### Extract meta worksheet header and row1 from template to union with data for each OU
  meta_ws <- readxl::read_excel(paste0("Dataout/", "/Template.xlsx"), sheet = "meta", range = "B1:E2") |> 
    mutate(`Operating Unit/Country` = subset_ci_meta$ou, 
           `CIRG Reporting Period` = subset_ci_meta$reportingperiod) 

## create filename
  filename2 <- paste0("Dataout/", reportingperiod, "/Submissions/", "CIRG_", str_to_upper(current_q), "_", 
                      str_replace_all(operating_units, " ", "_"), "_FHI360_EpiC_", 
                      str_replace_all(as.character(today()), "-", "_"), ".xlsx")

## output steps
  wb <- createWorkbook("cirg_submission")
  addWorksheet(wb, "meta")
  addWorksheet(wb, "CIRG")
  writeData(wb, sheet = "meta", meta_ws, startCol = 2)
  writeData(wb, sheet = "CIRG", cirg_ws, borders = "columns")
  writeData(wb, "CIRG", c("CIRG RESULT VALUE", "value"), startCol = 14, startRow = 2:3) #restore header for quantitative output field
  saveWorkbook(wb, filename2, overwrite = TRUE)

}

# write_excel_output_by_ou(operating_units = "Tanzania")

# run direct output for each OU
map(operating_units, write_excel_output_by_ou)


# output global data -------------------------------
write_csv(ci, paste0("Dataout/", reportingperiod, "/Global_.csv"), na = "")




# explore data after clearing environment  -------------------------------
reportingperiod_selected <- str_replace(current_q, "_", " ")
ci_read <- read_csv(paste0("Dataout/", reportingperiod_selected, "/Global_.csv")) 
  

# assess indicators reported
ci_read  |> 
  count(ou, indicator) |> 
  pivot_wider(names_from = indicator, values_from = n) |>
  relocate(TX_PVLS_ELIGIBLE, .before = PrEP_CT_VERIFY) |> 
  mutate_at(c(2:9), ~replace_na(.,0))

# assess technical areas covered by indicators reported
ci_read |> mutate(technical_area = case_when(str_detect(indicator, "PrEP") ~ "PrEP",
                                             indicator == "TX_PVLS_ELIGIBLE" ~ "Lab",
                                             str_detect(indicator, "TX") ~ "Key_Populations",
                                             str_detect(str_to_lower(indicator), "gen") ~ "Gender",
                                                                                              )) |> 
  count(ou, technical_area) |> 
  pivot_wider(names_from = technical_area, values_from = n) |> print()
  # relocate(TX_PVLS_ELIGIBLE, .before = PrEP_CT_VERIFY) |> 
  # mutate_at(c(2:9), ~replace_na(.,0))

# data check
ci_fhi|> filter(str_detect(country, "Liberia"), !is.na(population)) |> group_by(country, indicator) |> summarise(value = sum(value))


#Data check
ci_read |> filter(str_detect(orgunit, "Semey"), reportingperiod == "FY24 Q1", indicator=="TX_NEW_VERIFY") |> glimpse()



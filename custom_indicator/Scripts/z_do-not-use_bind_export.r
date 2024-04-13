
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
str_replace(current_q, "_", " ")

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


# define function
write_csv_by_ou <- function(operating_units) {
  subset_ci <- subset(ci, ou %in% operating_units)
  filename <- paste0("Dataout/", reportingperiod, "/", "CIRG_", str_to_upper(current_q), "_", 
                     str_replace_all(operating_units, " ", "_"), "_FHI360_EpiC_", 
                     str_replace_all(as.character(today()), "-", "_"), ".csv")
  write_csv(subset_ci, filename, na = "")
}

#apply function
map(operating_units, write_csv_by_ou)


#REPEAT process above to create empty files to paste into
#### eventually use unlocked sheets and paste without column names/headers into row 4 of CIRG sheet
#### likewise to META page OU / FYQ
dummy <- as.data.frame("test")

write_empty_filename_by_ou <- function(operating_units) {
  subset_ci <- subset(ci, ou %in% operating_units)
  filename2 <- paste0("Dataout/", reportingperiod, "/Submissions/", "CIRG_", str_to_upper(current_q), "_", 
                     str_replace_all(operating_units, " ", "_"), "_FHI360_EpiC_", 
                     str_replace_all(as.character(today()), "-", "_"), ".xlsx")
  write_csv(dummy, filename2, na = "")
}

map(operating_units, write_empty_filename_by_ou)


write_csv(ci, paste0("Dataout/", reportingperiod, "/Global_.csv"), na = "")

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


# rename files ------------------------------------------------------------


# $ for file in *02_14.xlsx; do mv "$file" "${file/02_14.xlsx/03_12.xlsx}"; done

# data check
ci_fhi |> filter(str_detect(country, "Vietnam"), !is.na(population)) |> group_by(country, indicator) |> summarise(value = sum(value))


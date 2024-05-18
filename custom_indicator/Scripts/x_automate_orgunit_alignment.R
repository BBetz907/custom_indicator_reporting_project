
# run orgunit recode files by country -------------------------------------
# alternatively run manually

orgunit_recode_filenames <- list.files(path="Scripts", pattern = "_matched.R|_psnu.R") |>   
  #no Burundi or Ghana data in FY24
  keep(~!str_detect(., "gha|bdi|vnm")) |> 
  print()


orgunit_recode_filepaths <- str_c("Scripts/", orgunit_recode_filenames
                                  # orgunit_recode_filenames[!grepl("gha|bdi", orgunit_recode_filenames)]
) 

map(orgunit_recode_filepaths, source)



# test output to ensure no missing data, between orgunit alignment matching ----------------------------------
#establish country codes for test
country_codes <- orgunit_recode_filenames |>
  keep(~str_detect(., "psnu")) |> #keep only half
  str_extract("(?<=\\_)[a-z]{3}(?=\\_)") #extract code



# test output again to ensure no missing data between orgunit alignment to PSNU level (step 2) ----------------------------------
country_code_psnu <- country_codes |> str_c("_merge_psnu")

difference <- map2(country_codes, country_code_psnu, ~ nrow(get(.x)) - nrow(get(.y)))
names(difference) <- country_codes
imap(difference, ~ cat(.y, "difference:", .x, "\n"))


# test output again to ensure no missing data between orgunit matching most granular level (step 1) ----------------------------------
###### MOST ERRORS WILL OCCUR HERE ################### 
country_code_original <- country_codes |> str_c("_info")

difference <- map2(country_codes, country_code_original, ~ nrow(get(.x)) - nrow(get(.y)))
names(difference) <- country_codes
imap(difference, ~ cat(.y, "difference:", .x, "\n"))

#distinct is only necessary where multiple levels must be merged. In that case use it to prevent multiple matches at the PSNU stage when you merge and not after (see MMR)

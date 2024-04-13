# define functions for exporting each sheet -------------------------------


# define function for CSV output, then paste and add meta content
write_csv_by_ou <- function(operating_units) {
  subset_ci <- subset(ci, ou %in% operating_units)
  filename <- paste0("Dataout/", reportingperiod, "/", "CIRG_", str_to_upper(current_q), "_", 
                     str_replace_all(operating_units, " ", "_"), "_FHI360_EpiC_", 
                     str_replace_all(as.character(today()), "-", "_"), ".csv")
  write_csv(subset_ci, filename, na = "")
}

#apply function
map(operating_units, write_csv_by_ou)


# superceded
# Create function to create OU excel outputs for CIRG tab
write_excel_cirg_tab_by_ou <- function(operating_units) {
  # Recode integer as character and rename ou to simplify union of header and data
  ci_str <- ci |> mutate(value = as.character(value)) |> rename(operatingunit=ou)
  # Create OU-specific frames to write to separate files
  subset_ci <- subset(ci_str, operatingunit %in% operating_units)
  # Extract CIRG worksheet headers from template to union with data for each OU
  head3r_cirg <- readxl::read_excel(paste0("Dataout/", "/Template.xlsx"), sheet = "CIRG") 
  # Union header and data values
  subset_cirg <- head3r_cirg |> bind_rows(subset_ci)
  # create filenames
  filename2 <- paste0("Dataout/", reportingperiod, "/Submissions/", "CIRG_", str_to_upper(current_q), "_", 
                      str_replace_all(operating_units, " ", "_"), "_FHI360_EpiC_", 
                      str_replace_all(as.character(today()), "-", "_"), ".xlsx")
  #  Output OU frames with headers to new worksheets for submission
  write.xlsx(subset_cirg, filename2, sheetName = "CIRG", colnames = TRUE, borders = "columns")
}

map(operating_units, write_excel_cirg_tab_by_ou)

# superceded
# Create function to create OU excel outputs for meta tab
write_excel_cirg_meta_by_ou <- function(operating_units) {
  # Read and group OU and reporting period data to input values into fields
  ci_meta_str <- ci |> group_by(ou, reportingperiod) |> summarise(.groups = "drop")
  # Create OU-specific frames to write to separate files
  subset_ci_meta <- subset(ci_meta_str, ou %in% operating_units)
  # Extract meta worksheet header and row1 from template to union with data for each OU
  head3r_meta <- readxl::read_excel(paste0("Dataout/", "/Template.xlsx"), sheet = "meta", range = "B1:E2") |> 
    mutate(`Operating Unit/Country` = subset_ci_meta$ou, 
           `CIRG Reporting Period` = subset_ci_meta$reportingperiod) 
  filename3 <- paste0("Dataout/", reportingperiod, "/Submissions/", "CIRG_", str_to_upper(current_q), "_", 
                      str_replace_all(operating_units, " ", "_"), "_FHI360_EpiC_", 
                      str_replace_all(as.character(today()), "-", "_"), ".xlsx")
  write.xlsx(head3r_meta, filename3, sheetName = "meta", colnames = TRUE, startCol = 2)
}

map(operating_units, write_excel_cirg_meta_by_ou)



# explore solution to export multiple sheets -------------------------------


# only question left is: "how do I export both the meta and CIRG tabs into the same file?
# THE ANSWER IS createWorkbook, addWorksheet, writeData, saveWorkbook
wb <- createWorkbook("country1")
addWorksheet(wb, "meta")
addWorksheet(wb, "CIRG")
writeData(wb, sheet = "meta", head3r_meta, startCol = 2)
writeData(wb, sheet = "CIRG", head3r_meta)
saveWorkbook(wb, "addWorksheetExample.xlsx", overwrite = TRUE)
head3r_meta

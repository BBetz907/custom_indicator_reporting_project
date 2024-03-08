
# FHI360 Custom Indicator Reporting for USAID OHA HQ 
<!-- badges: start -->
<!-- badges: end -->

The purpose of this project is to create file exports for Operatingunit (OU) level reporting for USAIDs Custom Indicator Reporting Group (CIRG). 
Data are first collected in a DHIS2 Aggregage system, called infolink, then downloaded using a PowerBI Query. 
This project ingests quarterly data files for the following indicators and transforms the data format to the OHA CIRG Template:
	TX_NEW_VERIFY, TX_RTT_VERIFY, TX_CURR_VERIFY, TX_PVLS_ELIGIBLE, TX_PVLS_VERIFY, 
	PREP_OFFER, PREP_NEW_VERIFY, PREP_CT_VERIFY
It processes age/sex and KP disaggregates, then harmonizes the orgunit hierarchies between Infolink and Datim. It exports OU-level quarterly data files for submission to the CIRG.

To run, you must have access to datafiles in the established format. Then you can run the following files in order:
	packages.R
	functions.R
	general_cleaning.R
	mech_acquire_from_msd.R
	mech_list_msd.R
	mech_merge.R
	orgunit_levels.R
	dr_orgs.R
	x_automate_orgunit_alignment.R
	z_bind_export.R 

The x_automate_orgunit_alignment file runs the orgunit alignment files following the format orgunits_xxx_matched.R, orgunits_xxx_psnu.R to synchronize orgunit hierarchies for for 20+ country programs (2024_03_08 update). It also is configured to detect data loss (rows) in the process.
If data loss is detected, this can be resolved in the orgunits_countrycode_psnu.R and orgunits_countrycode_matched.R files, which merge to DATIM metadata files. This is routinely necessary when DATIM metadata for orgunits change, at least once per year.  Data is lost because rows from the data source are not matched to the orgunit structure, so this must be remedied at the country level. Country codes specify file names, as per the 3-letter DATIM country codes.
Additionally, EpiC implementation may end in a given program, thus ending data reporting. In this case, country codes can be added to the list for exclusion early on in the data flow in the code:   keep(~!str_detect(., "gha|bdi")) |> 

The z_bind_export file binds data frames and then exports data files for each OU. The only real inputs are to specify countries where the mission reports the data themselves.
Both these files extensively utilize the iteration capabilities of the PURRR package to automate and repeat processes (2023_12 update), with manual input only when an error arises.

Once exported, data must be inserted into a file structure template manually, saved, and submitted at the CIRG submission form via google.

Next step: automate export into CIRG template starting at row 5 with no headers

Updated 2024_03_08
---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*
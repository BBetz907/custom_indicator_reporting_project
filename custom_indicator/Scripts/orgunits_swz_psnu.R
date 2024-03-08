#psnu level 4
table(swz$orgunit_level)

swz_merge_psnu <- swz %>%
  mutate(psnu = if_else(orgunit_level == 4, orgunit, orgunit_parent),
         psnu_uid = if_else(orgunit_level == 4, orgunituid, orgunit_parent_uid)) |> 
  select(-orgunit_name, -orgunit_uid) |> glimpse()

swz_merge_psnu |> count(orgunit) |> group_by() |> summarise(n=sum(n))

nrow(swz_merge_psnu) - nrow(swz)

# This script is used to generate the wwtp_name to site_id mapping

ww_data_path_with_site_id <- file.path(
  "input", "ww_data",
  "nwss_data",
  "2024-04-09.csv"
)

nwss_w_site_id <- readr::read_csv(ww_data_path_with_site_id) |>
  dplyr::mutate(
    reporting_jurisdiction = toupper(reporting_jurisdiction),
    wwtp_jurisdiction = toupper(wwtp_jurisdiction)
  )

nwss_map <- nwss_w_site_id |>
  dplyr::select(
    site_id, wwtp_name, lab_id,
    reporting_jurisdiction, wwtp_jurisdiction
  ) |>
  unique()


site_to_wwtp_map <- nwss_w_site_id |>
  dplyr::select(site_id, wwtp_name) |>
  unique()

# Write the maps to input data
fp <- file.path("input", "site_maps")
cfaforecastrenewalww::create_dir(fp)
write.csv(site_to_wwtp_map, file.path(fp, "site_map.csv"), row.names = FALSE)
write.csv(nwss_map, file.path(fp, "site_lab_map.csv"), row.names = FALSE)

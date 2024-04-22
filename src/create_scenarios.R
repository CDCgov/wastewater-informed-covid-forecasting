# This script is used to generate the list of `wwtp_name` s for each scenario
# and save them as `.tsv` files

# Read in the map of wwtp_name to site
site_map <- readr::read_csv(file.path("input", "site_maps", "site_map.csv"))

# Read in each of the 4 scenarios
raw_scenario_dir <- file.path("input", "raw_scenarios")
coes <- readr::read_csv(file.path(raw_scenario_dir, "s1_anon.csv"))
hhs_regions <- readr::read_csv(file.path(raw_scenario_dir, "s2_anon.csv"))
msas <- readr::read_csv(file.path(raw_scenario_dir, "s3_anon.csv"))
national <- readr::read_csv(file.path(raw_scenario_dir, "s4_anon.csv"))

# Left join wwtp_name, which is the colu,n we have in our dataset
coes_w_wwtp_name <- coes |>
  dplyr::left_join(site_map, by = c("site_id"))
hhs_regions_w_wwtp_name <- hhs_regions |>
  dplyr::left_join(site_map, by = c("site_id"))
msas_w_wwtp_name <- msas |>
  dplyr::left_join(site_map, by = c("site_id"))
national_w_wwtp_name <- national |>
  dplyr::left_join(site_map, by = c("site_id"))

# Write to scenarios folder
scenario_dir <- file.path("input", "config", "eval", "scenarios")

write.table(coes_w_wwtp_name, file.path(scenario_dir, "coes.tsv"), row.names = FALSE)
write.table(hhs_regions_w_wwtp_name, file.path(scenario_dir, "hhs_regions.tsv"), row.names = FALSE)
write.table(msas_w_wwtp_name, file.path(scenario_dir, "msas.tsv"), row.names = FALSE)
write.table(national_w_wwtp_name, file.path(scenario_dir, "national.tsv"), row.names = FALSE)

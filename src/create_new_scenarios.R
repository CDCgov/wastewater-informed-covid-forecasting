# This script is used to generate the list of `wwtp_name` s for each
# of the new scenarios. These are in a different format than the previous.
# There is one csv with a list of the anonymized site ids and a 0 or 1
# indicating whether or not it is included in the scenario. We will create
# the list of site ids and save them save them as `.tsv` files

# Read in the map of wwtp_name to site
site_map <- readr::read_csv(file.path("input", "site_maps", "site_map.csv"))

# Read in each of the 4 scenarios
raw_scenario_dir <- file.path("input", "raw_scenarios")
new_scenarios <- readr::read_csv(file.path(raw_scenario_dir, "new_scenarios.csv"))


scens <- new_scenarios |>
  dplyr::filter(!is.na(site_id_anon))

fed_funded <- scens |>
  dplyr::filter(s0 == 1) |>
  dplyr::select(site_id_anon, wwtp_jurisdiction) |>
  dplyr::left_join(site_map,
    by = c("site_id_anon" = "site_id")
  ) |>
  dplyr::filter(!is.na(wwtp_name))

coe_proxy <- scens |>
  dplyr::filter(s1 == 1) |>
  dplyr::select(site_id_anon, wwtp_jurisdiction) |>
  dplyr::left_join(site_map,
    by = c("site_id_anon" = "site_id")
  ) |>
  dplyr::filter(!is.na(wwtp_name))

coe_regional <- scens |>
  dplyr::filter(s2 == 1) |>
  dplyr::select(site_id_anon, wwtp_jurisdiction) |>
  dplyr::left_join(site_map,
    by = c("site_id_anon" = "site_id")
  ) |>
  dplyr::filter(!is.na(wwtp_name))

max_40 <- scens |>
  dplyr::filter(s3 == 1) |>
  dplyr::select(site_id_anon, wwtp_jurisdiction) |>
  dplyr::left_join(site_map,
    by = c("site_id_anon" = "site_id")
  ) |>
  dplyr::filter(!is.na(wwtp_name))

# Write to scenarios folder
scenario_dir <- file.path("input", "config", "eval", "scenarios")

readr::write_tsv(fed_funded, file.path(scenario_dir, "fed_funded.tsv"))
readr::write_tsv(coe_proxy, file.path(scenario_dir, "coe_proxy.tsv"))
readr::write_tsv(coe_regional, file.path(scenario_dir, "coe_regional.tsv"))
readr::write_tsv(max_40, file.path(scenario_dir, "max_40.tsv"))

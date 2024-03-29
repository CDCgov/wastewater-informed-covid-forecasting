#' Input dataframe for fitting the site-level infection dynamics wastewater
#' model
#'
#' A dataset containing the daily hospital admissons, observed wastewater
#' concentrations and other attributes needed for the model to be fit.
#' Generated via the defaults in `generate_simulated_data.R`
#'  The variables are as follows:
#'
#' @format ## example_df
#' A data frame with 635 rows and 13 columns
#' \describe{
#'   \item{t}{The time index in days}
#'   \item{lab_wwtp_unique_id}{The unique identifier of the wastewater site-lab
#'   combination. For this example, there are 5 unique combinations, so the
#'   indices range from 1 to 5 but would vary for the number of unique
#'   combinations in your input dataset if trying to construct something mirroring
#'   this structure}
#'   \item{log_conc}{The log scale viral genomes per mL collected from the
#'   lab-site specified on day date.
#'   NAs indicate days where a sample wasn't reported in that lab-site.}
#'   \item{date}{Sample collection date, formatted as YYYY-MM-DD}
#'   \item{lod_sewage}{The log scaled limit of detection reported by the
#'   lab-site on that date}
#'   \item{below_LOD}{An indicator (0,1) indicating whether the wastewater
#'   observation on that date is
#'    above (0) or below (1) the LOD. NAs for all days with wastewater
#'    observations in that lab-site}
#'   \item{daily_hosp_admits}{The number of individuals admitted to the
#'   hospital on that date,
#'   available as of the forecast date. Note this is repeated for each site-lab.}
#'   \item{daily_hosp_admits_for_eval}{The number of individuals admitted to
#'   the hospital on that date,
#'   available retrospectively (so including after the forecast date).
#'   Note this is repeated for each site-lab}
#'   \item{pop}{The number of people in the "state" or whatever population
#'   contributes to the daily hospital admissions}
#'   \item{forecast_date}{The date the forecast is made, formatted as YYYY-MM-DD}
#'   \item{hosp_calibration_time}{The duration of time in days to calibrate the
#'    model to hospital admissions data}
#'   \item{site}{The unique identifier for the site (also referred to as a
#'   wastewater catchment area). There can be more than
#'   one lab per site if samples are sent to different/overlapping labs.}
#'   \item{ww_pop}{The population size of the site (wastewater catchment area)}
#' }
#' @source generate_simulated_data.R
"example_df"

#' Input parameters used to generate `example_df`
#'
#' A few key parameters that were created by the default arguments in
#' `generate_simulated_data.R`
#'
#' @format ## param_df
#' A data frame with 305 rows and 4 columns
#' \describe{
#'   \item{name}{Parameter name}
#'   \item{true_value}{ The value that was assigned to this parameter in the
#'   data generation process}
#'   \item{index_rows}{if this is a parameter array, the array row index for
#'   this particular value; for scalar parameters it is always NA }
#'   \item{index_cols}{if this is a parameter array, the array column index for
#'   this particular value; for scalar parameters it is always NA) }
#' }
#' @source generate_simulated_data.R
"param_df"

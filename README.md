# Overview
This repo leverages a semi-mechanistic renewal approach to jointly fit COVID-19 hospital admissions data in the US and viral concentrations in wastewater to forecast hospital admissions. See our [Model Definition page](model_definition.md) for a mathematical description of the generative model, and the [example vignette](cfaforecastrenewalww/vignettes/toy_data_vignette.Rmd) to run the inference and forecasting on the simulated data provided. In brief, our model builds upon [EpiNow2](https://github.com/epiforecasts/EpiNow2/tree/main), a widely used [R](https://www.r-project.org/) and [Stan](https://mc-stan.org/) package for Bayesian epidemiological inference. We modify EpiNow2 to add model for the observed viral RNA concentration in wastewater.

This README is organized into the following sections:
- Our [workflow](#our-workflow-for-covid-19-forecast-hub-submissions) for producing weekly forecasts
- Details on [model input data](#model-input-data)
- A description of our [forecasting pipeline](#forecasting-pipeline)
- A guide to [installing and running our code](#installing-and-running-code)
- Details on [contributing to this project](#contributing-to-this-project)
- [Standard CDCGov open source repository information, notices, and disclaimers](#standard-cdcgov-open-source-repository-information-notices-and-disclaimers)

# Our workflow for Covid-19 Forecast Hub submissions
To produce our submissions to the Covid-19 Forecast Hub, we run a [forecasting pipeline](#forecasting-pipeline) every Saturday evening at 9:10 pm EST. In addition to pulling the latest data and using it to fit our inference models, the pipeline generates summary figures, produces a diagnostic report of Markov Chain Monte Carlo convergence diagnostics, and performs data quality checks on the wastewater data. We examine these outputs manually to check for data or model convergence issues.

We produce forecasts of COVID-19 hospital admissions for the 50 states, Puerto Rico, District of Columbia (DC), and the United States. Most forecasts use both wastewater data and hospital admission data, but if a location does not have any wastewater data, the wastewater input data for the model are deemed unreliable, or the model fails to converge, we use the hospital admissions-only model instead. If that model is also unreliable, we do not submit a forecast for that location. In all cases, we record our choice and the reason for it in a run-specific `metadata.yaml` file, as follows:
- "States without wastewater data": No wastewater data from the past 90 days were available for these locations, so we necessarily used the hospital admissions-only model for them.
- "States we chose to use hospital admissions only model on": We detected anomalies in reported wastewater values for these locations, or the wastewater model fits for these locations did not pass checks for reliability, so we chose to use the hospital admissions-only model for them.
- "States with insufficient wastewater data": We used the wastewater-informed model for these locations, but the actual wastewater data available for them was likely too sparse to meaningfully inform the forecast.
- "States not forecasted": Both the wastewater-informed and the hospital admissions-only models had issues for these locations, so we did not submit forecasts for them.

Individual archived forecasts and their corresponding `metadata.yaml` files can be found in datestamped subdirectories of the [`output/forecasts`](output/forecasts) directory, e.g. [`output/forecasts/2024-02-05`](output/forecasts/2024-02-05).

# Model input data
We store all data and configuration for the model in the [`input`](input) folder.

## Hospitalization data
For real-time production, we pull hospitalization data from the [NHSN HealthData.gov public dataset](https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh) and then is stored locally once it is ingested. For retrospective evaluation on time-stamped data sets, we use the [`covidcast`](https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html) R package.

## Wastewater data
We use the [NWSS API on the DCIPHER platform](https://www.cdc.gov/nwss/reporting.html) (non-public data, requires permission from NWSS to access) to obtain wastewater data at the facility level.

## Data access and API keys
To interact with `covidcast`, HealthData.gov, or DCIPHER/NWSS, we use API keys. `covidcast` and HealthData.gov are public; anyone can request an API key. One must complete a data use agreement to access raw wastewater data from NWSS; see the [NWSS website](https://www.cdc.gov/nwss/about-data.html) for details.

Our data pipeline expects users to store these API keys in a local `secrets.yaml` file. See instructions below for setting up your `secrets.yaml` file in a format the pipeline can parse.

## Data file structure
The data (both inputs and outputs) are currently loaded in either from within the [`input`](input) folder as shown below or directly from the APIs (described above). This folder also contains a file with state-level population data ([`locations.csv`](input/locations.csv)) used by the pipeline.

Model outputs are written to individual folders after each model is run, and the file path to access those model outputs are returned as an output to the targets pipeline, to be used for downstream analysis and plotting. Alongside each pipeline run is a model metadata `.txt` file formatted for the COVID-19 Forecast Hub submission, which can be found in the folder `forecasts`.

```
+--input
    +-- ww_data
        +-- nwss_data
            +-- {date_of_data_pull}.csv
    +-- hosp_data
        +-- vintage_datasets
            +-- {date_of_data_pull}.csv
    +-- config
        +--{test/prod}
            +-- config-{model_type}-{run_id}.yaml
    +--saved_pmfs
        +-- generation_interval.csv
        +-- inf_to_hosp.csv
    +--train_data
        +-- {forecast_date}
		+-- {model_type}
                    +-- train_data.csv
    +--locations.csv
+-- output
 +-- forecasts
            +-- {forecast_date}
		+-- {forecast_date}.tsv
		+-- metadata.yaml
		+-- wastewater_metdata_table.tsv
+-- {forecast_date}
	+-- run-on-{date_of_run}-{run_id}
    		+-- raw
			+-- {individual_state}
				+-- {model_type}
                    			+-- draws.parquet
					+-- quantiles.parquet
					+-- parameters.parquet
					+-- future_hosp_draws.parquet
					+-- diagnostics.csv
					+-- stan_objects
						+-- {model_name}-{timestamp}-{chain}.csv
    		+-- figures
                	+--{individual state}
                    		+-- individual plots of generated quantities + data for all models
    		+-- cleaned
			+-- external
				+--cfa-wwrenewal
					+-- {forecast_date}-cfa-wwrenewal.csv
					+-- pdf of hub submissions
				+--cfa-wwrenewal_hosp_only
					+-- {forecast_date}-cfa-wwrenewal_hosp_only.csv
					+-- pdf of what we would have submitted to hub had we submitted all hosp_only model
				+--cfa-wwrenewal_all_ww
					+-- {forecast_date}-cfa-wwrenewal_all_ww.csv
					+-- pdf of what we would have submitted to hub had we submitted wastewater model in all cases
            		+-- internal
                		+-- diagnostic_report.html
				+-- pdfs of combined quantiles forecasts, hospital admissions forecasts for mult models, wastewater estimates, R(t), etc.
			+-- {submitted/test}_forecasts
				+-- {forecast_date}-cfa-wwrenewal.csv
			+-- all_wastewater_submission
				+-- test_forecasts
					+--cfa_wwrenewal_all_ww
						+{forecast_date}_cfa-wwrenewal_all_ww.csv
			+-- hospital_admissions_only_submission
				+-- test_forecasts
					+--cfa_wwrenewal_hosp_only
						+{forecast_date}_cfa-wwrenewal_hosp_only.csv
  		 +-- pipeline_run_metadata
        		+-- {test/prod}
                   		 +-- {run_id}.yaml
```

# Forecasting pipeline
We use a pipeline to pull data, process it, fit models, and generate forecasts formatted for submission to the [COVID-19 Forecast Hub](https://covid19forecasthub.org/). The [`_targets.R`](_targets.R) script in the project root directory defines the pipeline via the [`targets` R package](https://books.ropensci.org/targets/).

The pipeline does the following, in order:
1. Pulls the latest wastewater and hospital admissions data from NWSS and NHSN, respectively
2. Formats the data properly for ingestion by our Stan models.
3. Fits Bayesian renewal models to those data (links below point to the relevant `.stan` source files):
    - A [model without wastewater](cfaforecastrenewalww/inst/stan/renewal_ww_hosp.stan) (based only on hospital admissions).
	- A [national model using aggregated wastewater](cfaforecastrenewalww/inst/stan/renewal_ww_hosp.stan) concentration data.
    - A [model incorporating site-level wastewater concentration data](cfaforecastrenewalww/inst/stan/renewal_ww_hosp_site_level_inf_dynamics.stan).
4. Post-processes model output to produce forecasts and summary figures, including a table formatted for submission to the Covid-19 Forecast Hub.

See our [model definition page](model_definition.md) for further details on the modeling methods and data pre-processing.

# Installing and running code

## Install R
To run our code, you will need a working installation of [R](https://www.r-project.org/) (version `4.3.0` or later). You can find instructions for installing R on the official [R project website](https://www.r-project.org/).

## Install `cmdstanr` and `CmdStan`
We do inference from our models using [`CmdStan`](https://mc-stan.org/users/interfaces/cmdstan) (version `2.34.1` or later) via its R interface [`cmdstanr`](https://mc-stan.org/cmdstanr/) (version `0.7.1` or later).

Open an R session and run the following command to install `cmdstanr` per that package's [official installation guide](https://mc-stan.org/cmdstanr/#installation).

```R
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
```

`cmdstanr` provides tools for installing `CmdStan` itself. First check that everything is properly configured by running:

```R
cmdstanr::check_cmdstan_toolchain()
```

You should see the following:
```
The C++ toolchain required for CmdStan is setup properly!
```

If you do, you can then install `CmdStan` by running:
```R
cmdstanr::install_cmdstan()
```
If installation succeeds, you should see a message like the following:
```
CmdStan path set to: {a path on your file system}
```

If you run into trouble, consult the official [`cmdstanr`](https://mc-stan.org/cmdstanr/index.html) website for further installation guides and help.

## Download this repository and install the project package (`cfaforecastrenewalww`)
Once `cmdstanr` and `CmdStan` are installed, the next step is to download this repository and install our project package, `cfaforecastrenewalww`. The repository provides an overall structure for running the forecasting analysis; the project package provides tools for specifying and running our models, and installs other needed dependencies.

Once you have downloaded this repository, navigate to it within an R session and run the following:

```R
install.packages('remotes')
remotes::install_local("cfaforecastrenewalww")
```
If that fails, confirm that your R working directory is indeed the project directory by running R's `getwd()` command.

## R dependencies
Installing the project package should take care of almost all dependencies installations. Confirm that package installation has succeeded by running the following within an R session:

```R
library(cfaforecastrenewalww)
```

## Set up API keys
To load in the data you will need to set up a `secrets.yaml` file in the root of the directory with the following format:
```
covidcast_api_key: {key}
NHSN_API_KEY_ID: {key}
NHSN_API_KEY_SECRET: {key}
nwss_data_token: {token}
data_rid: {rid}
```

- covidcast: Go to the CMU Delphi [website](https://cmu-delphi.github.io/delphi-epidata/api/api_keys.html) and request an API key using [their form](https://api.delphi.cmu.edu/epidata/admin/registration_form). The key will come in an email.
- NSHN: Log into HealthData.gov's [developer settings page](https://healthdata.gov/login). At the profile page, click the pencil next to the abstract avatar image, then "Developer Settings," then create an API key. Be sure to copy the key secret to a safe place because you won't be able to see it again.
- NWSS: Ensure you have access to DCIPHER first. Then go to the [tokens page](https://dcipher.cdc.gov/workspace/settings/tokens) to create a new token. Be sure to copy the token to a safe place because you won't be able to see it again.
- RID: The dataset RID is obtained when the data use agreement is approved and the link to the dataset on DCIPHER is provided.

## Run the pipeline
To run the pipeline, type the following at a command prompt from the top-level project directory:

```bash
Rscript --vanilla -e "targets::tar_make()"
```

Alternatively, in an interactive R session with your R working directory set to the project root, run
```R
targets::tar_make()
```

The first time you run the pipeline after installing `cfaforecastrenewalww`, `cmdstan` will compile the model source `.stan` files to executible binaries, which by default are stored in a subdirectory `bin/` of the top-level project directory. Subsequent runs should use those precompiled executibles, without need for recompilation. To force recompilation, delete the binaries stored in the `bin/` directory or reinstall the `cfaforecastrenewalww` R package.

# Contributing to this project

## Git workflow
We store our production code in the `prod` branch; refer to the `HEAD` of that branch for the code used to produce our most recent published forecast. To develop new features or fix bugs, create a feature branch off of `prod`. When the feature is ready, make a pull request into `prod`. All tests should pass within a feature branch before pull request can be merged.

Please see our [contributing guidelines](CONTRIBUTING.md) and [code-of-conduct](code-of-conduct.md) for more details.

# Contact information
We want feedback and questions! Feel free to [submit an issue](../../issues) here on Github, or contact us via this [form](https://www.cdc.gov/forecast-outbreak-analytics/contact-us.html).

# Standard CDCGov open source repository information, notices, and disclaimers

## Public Domain Standard Notice
This repository constitutes a work of the United States Government and is not
subject to domestic copyright protection under 17 USC § 105. This repository is in
the public domain within the United States, and copyright and related rights in
the work worldwide are waived through the [CC0 1.0 Universal public domain dedication](https://creativecommons.org/publicdomain/zero/1.0/).
All contributions to this repository will be released under the CC0 dedication. By
submitting a pull request you are agreeing to comply with this waiver of
copyright interest.

## License Standard Notice
The repository utilizes code licensed under the terms of the Apache Software
License and therefore is licensed under ASL v2 or later.

This source code in this repository is free: you can redistribute it and/or modify it under
the terms of the Apache Software License version 2, or (at your option) any
later version.

This source code in this repository is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the Apache Software License for more details.

You should have received a copy of the Apache Software License along with this
program. If not, see http://www.apache.org/licenses/LICENSE-2.0.html

Any included source code adapted or reused from another open source project inherits that project's license.

## Privacy Standard Notice
This repository contains only non-sensitive, publicly available data and
information. All material and community participation is covered by the
[Disclaimer](DISCLAIMER.md)
and [Code of Conduct](code-of-conduct.md).
For more information about CDC's privacy policy, please visit [http://www.cdc.gov/other/privacy.html](https://www.cdc.gov/other/privacy.html).

## Contributing Standard Notice
Anyone is encouraged to contribute to the repository by [forking](https://help.github.com/articles/fork-a-repo)
and submitting a pull request. (If you are new to GitHub, you might start with a
[basic tutorial](https://help.github.com/articles/set-up-git).) By contributing
to this project, you grant a world-wide, royalty-free, perpetual, irrevocable,
non-exclusive, transferable license to all users under the terms of the
[Apache Software License v2](http://www.apache.org/licenses/LICENSE-2.0.html) or
later.

All comments, messages, pull requests, and other submissions received through
CDC including this GitHub page may be subject to applicable federal law, including but not limited to the Federal Records Act, and may be archived. Learn more at [http://www.cdc.gov/other/privacy.html](http://www.cdc.gov/other/privacy.html).

## Records Management Standard Notice
This repository is not a source of government records, but is a copy to increase
collaboration and collaborative potential. All government records will be
published through the [CDC web site](http://www.cdc.gov).

## Additional Standard Notices
Please refer to [CDC's Template Repository](https://github.com/CDCgov/template) for the standard/template [CDCGov](https://github.com/CDCGov) [README](https://github.com/CDCGov/template/blob/main/README.md), [contribution policy](https://github.com/CDCgov/template/blob/main/CONTRIBUTING.md),
[disclaimer](https://github.com/CDCgov/template/blob/main/DISCLAIMER.md),
and [code of conduct](https://github.com/CDCgov/template/blob/main/code-of-conduct.md) from which the corresponding documents found in this repository have been derived.

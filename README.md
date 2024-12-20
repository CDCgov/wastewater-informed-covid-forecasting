# Bayesian generative modeling for heterogeneous wastewater data applied to COVID-19 forecasting
This repository contains the code to generate the results of evaluating retrospectively the forecast performance of a wastewater-informed forecasting model, both compared to a model without wastewater data and compared to other models submitted to the  [COVID-19 Forecast Hub](https://github.com/reichlab/covid19-forecast-hub/tree/master) over the 2023-24 epidemic season.
The model is run using the [`wwinference` R package](https://github.com/CDCgov/ww-inference-model), please see that GitHub repository for a [mathematical description](https://github.com/CDCgov/ww-inference-model/blob/main/model_definition.md) of the model and details on how to install the package and run the model.

This codebase was previously used for real-time submissioned to the COVID-19 Forecast Hub, spanning the dates from February 5th, 2024 to April 29th, 2024.
The model used to generate those submissions has since been ported over to the [`wwinference` R package](https://github.com/CDCgov/ww-inference-model) and has been modified from its original structure.

This README is organized into the following sections:
- [Project structure](#project-structure) describing the contents of this repository
- A description of our [evaluation pipeline](#evaluation-pipeline), used to evaluate the real-time and retrospective forecasts both with and without wastewater and compared to other Hub models
- Our [workflow](#deprecated-real-time-workflow-for-covid-19-forecast-hub-submissions) for producing weekly forecasts
- Details on [model input data](#model-input-data)
- [Standard CDCGov open source repository information, notices, and disclaimers](#standard-cdcgov-open-source-repository-information-notices-and-disclaimers)

## Project structure
| Folder or file | Purpose |
|---|---|
|[`pipeline`](pipeline)| R scripts used to fit and post-process each model run in the Azure batch workflow
|[`_targets_eval_postprocessing.R`](_targets_eval_postprocessing.R) | The [targets](https://books.ropensci.org/targets/) pipeline used to generate the figures and results in this work, from the post-processed outputs that are generated from running the fitting and postprocessing [`pipeline`](pipeline/) on Azure Batch |
|[`wweval`](wweval) | Code used for the evaluation pipeline, including pre and post-processing and figure generation |
|[`input`](input)| Raw input data used to fit the model, including a parameter file and delay pmfs used by the model |
|[`output/forecasts`](output/forecasts)| Records of the real-time forecasts and associated metadata submitted to the [COVID-19 Forecast Hub](https://github.com/reichlab/covid19-forecast-hub/tree/master) from February 5th through April 29th, 2024 |
|[`output/benchmarking`](output/benchmarking)| Metrics of performance from different versions of the model, used to assess model changes |
|[`src`](src)| Code used to generate the configuration files used in the Azure batch pipeline and the post-processing pipeline |
|[`.github`](.github) | GitHub actions used to set up the CI for the `wweval` functions
|[`docs`](docs)| Record of the evaluation plan, note this has not been updated to reflect the current state of the evaluation workflow |
|[`scratch`](scratch)| Various scratch files used throughout the project |
|[`_targets.R`](_targets.R)| Now deprecated real-time [targets](https://books.ropensci.org/targets/) pipeline |
|[`model_diagnostics`](model_diagnostics)| R markdown presenting summaries of the model diagnostics for use in the ream-time production setting |

## Evaluation pipeline
Retrospective forecasts with and without wastewater data were generated and evaluated for all 22 forecast dates from October 16, 2023 to March 11, 2024, using the `wwinference` package run in Azure batch.
The azure batch pipeline was broken into a `fit` and `post_process` job.
The outputs from the `post_process` job were copied onto a local machine to facilitate downstream analysis.
This included the scores (generated from [`scoringutils` 1.2.2](https://github.com/epiforecasts/scoringutils/releases/tag/v1.2.2)) from the 2,000 posterior draws of hospital admissions forecasts, the quantiled hospital admissions and wastewater concentraitons from the calibration, nowcast, and forecast period and the input hospital admissions and wastewater data used to generate them.

The [`_targets_eval_postprocessing.R`](_targets_eval_postprocessing.R) file provides the pipeline to produce the results for the real-time and retrospective evaluation with and without wastewater and compared to other Hub models.

### A note on reproducibility
Unfortunately, the retrospective forecasts are not fully reproducible because they rely on NWSS data, which is not publicly available.

Additionally, because we originally used this single code base for our modeling and production-level pipelining, and have since moved to a separate modeling package (`wwinference`), we can no longer easily reproduce the model outputs that would have been generated in real-time, as the versions of the model at that time are not in tagged version histories of `wwinference`.

## Deprecated real-time workflow for Covid-19 Forecast Hub submissions
*This process was used to produce the real-time forecasts from February 5th through April 29th, 2024. It is no longer being run in production, but we have maintained the text as a record of our process*

To produce our submissions to the Covid-19 Forecast Hub, we run a [forecasting pipeline](#forecasting-pipeline) every Saturday evening at 9:10 pm EST. In addition to pulling the latest data and using it to fit our inference models, the pipeline generates summary figures, produces a diagnostic report of Markov Chain Monte Carlo convergence diagnostics, and performs data quality checks on the wastewater data. We examine these outputs manually to check for data or model convergence issues.

We produce forecasts of COVID-19 hospital admissions for the 50 states, Puerto Rico, District of Columbia (DC), and the United States. Most forecasts use both wastewater data and hospital admission data, but if a location does not have any wastewater data, the wastewater input data for the model are deemed unreliable, or the model fails to converge, we use the hospital admissions-only model instead. If that model is also unreliable, we do not submit a forecast for that location. In all cases, we record our choice and the reason for it in a run-specific `metadata.yaml` file, as follows:
- "States without wastewater data": No wastewater data from the past 90 days were available for these locations, so we necessarily used the hospital admissions-only model for them.
- "States we chose to use hospital admissions only model on": We detected anomalies in reported wastewater values for these locations, or the wastewater model fits for these locations did not pass checks for reliability, so we chose to use the hospital admissions-only model for them.
- "States with insufficient wastewater data": We used the wastewater-informed model for these locations, but the actual wastewater data available for them was likely too sparse to meaningfully inform the forecast.
- "States not forecasted": Both the wastewater-informed and the hospital admissions-only models had issues for these locations, so we did not submit forecasts for them.

Individual archived forecasts and their corresponding `metadata.yaml` files can be found in datestamped subdirectories of the [`output/forecasts`](output/forecasts) directory, e.g. [`output/forecasts/2024-02-05`](output/forecasts/2024-02-05).

## Model input data
We store all data and configuration for the model in the [`input`](input) folder.
This repository does not contain the input h

### Hospital admissions data data
For real-time production, we pulled hospital admissions data from the [NHSN HealthData.gov public dataset](https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh) and stored a snapshot of that data (a "vintaged dataset") locally each week.
Vintaged data is not stored in this repository, but the pipeline expects this data to live in: `input_data/hosp_data/monday_wednesday_datasets`

### Wastewater data
We used the [NWSS API on the DCIPHER platform](https://www.cdc.gov/nwss/reporting.html) (non-public data, requires permission from NWSS to access) to obtain wastewater data at the facility level each week, storing a vintaged dataset each week.
Vintaged data is not stored in this repository, but the pipeline expects this data to live in: `input_data/ww_data/monday_datasets`

## Contact information
We want feedback and questions! Feel free to [submit an issue](../../issues) here on Github, or contact us via this [form](https://www.cdc.gov/forecast-outbreak-analytics/contact-us.html).

## Standard CDCGov open source repository information, notices, and disclaimers

### Public Domain Standard Notice
This repository constitutes a work of the United States Government and is not
subject to domestic copyright protection under 17 USC § 105. This repository is in
the public domain within the United States, and copyright and related rights in
the work worldwide are waived through the [CC0 1.0 Universal public domain dedication](https://creativecommons.org/publicdomain/zero/1.0/).
All contributions to this repository will be released under the CC0 dedication. By
submitting a pull request you are agreeing to comply with this waiver of
copyright interest.

### License Standard Notice
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

### Privacy Standard Notice
This repository contains only non-sensitive, publicly available data and
information. All material and community participation is covered by the
[Disclaimer](DISCLAIMER.md)
and [Code of Conduct](code-of-conduct.md).
For more information about CDC's privacy policy, please visit [http://www.cdc.gov/other/privacy.html](https://www.cdc.gov/other/privacy.html).

### Contributing Standard Notice
Anyone is encouraged to contribute to the repository by [forking](https://help.github.com/articles/fork-a-repo)
and submitting a pull request. (If you are new to GitHub, you might start with a
[basic tutorial](https://help.github.com/articles/set-up-git).) By contributing
to this project, you grant a world-wide, royalty-free, perpetual, irrevocable,
non-exclusive, transferable license to all users under the terms of the
[Apache Software License v2](http://www.apache.org/licenses/LICENSE-2.0.html) or
later.

All comments, messages, pull requests, and other submissions received through
CDC including this GitHub page may be subject to applicable federal law, including but not limited to the Federal Records Act, and may be archived. Learn more at [http://www.cdc.gov/other/privacy.html](http://www.cdc.gov/other/privacy.html).

### Records Management Standard Notice
This repository is not a source of government records, but is a copy to increase
collaboration and collaborative potential. All government records will be
published through the [CDC web site](http://www.cdc.gov).

### Additional Standard Notices
Please refer to [CDC's Template Repository](https://github.com/CDCgov/template) for the standard/template [CDCGov](https://github.com/CDCGov) [README](https://github.com/CDCGov/template/blob/main/README.md), [contribution policy](https://github.com/CDCgov/template/blob/main/CONTRIBUTING.md),
[disclaimer](https://github.com/CDCgov/template/blob/main/DISCLAIMER.md),
and [code of conduct](https://github.com/CDCgov/template/blob/main/code-of-conduct.md) from which the corresponding documents found in this repository have been derived.

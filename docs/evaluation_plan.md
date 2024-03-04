# Evaluation analysis scoping
This is a draft evaluation plan for the wastewater-informed COVID-19 forecasting models in this repo. Our aim is to complete a first analysis and draft a preprint by April 12th, and post the preprint circa April 26th.

## Key questions this analysis should address:
1. What is the marginal value of incorporating wastewater data into a COVID-19 hospital admission forecasting model? We will quantify the change in forecast performance from incorporating wastewater with standard forecast evaluation metrics: a proper scoring rule and measures of error, bias, and coverage.
a. **The marginal value of wastewater within our model**. How do these metrics change when our model takes in wastewater data in addition to hospital admissions data? We will compare our model's forecast performance when it is fit to wastewater and incident admissions data to its performance when it is fit to admissions data alone.
b. **The real-time performance of our wastewater-informed model in a COVID forecasting challenge**. How did forecasts from our model submitted to the [COVID-19 Forecast Hub](https://github.com/reichlab/covid19-forecast-hub) perform compared to other models submitted to the Hub, and compared to the Hub ensemble forecast?
2. How do the quality and quantity of wastewater data available for a location affect forecast model performance?
a. How much do forecast predictions change as a function of the following wastewater characteristics: (1) reporting lag, (2) sample collection cadence, (3) population coverage etc.?
b. What effect do these data quantity/quality features have on quantifiable forecast model performance?

## Analysis plans
The below outline the specific analyses intended to answer each of these questions
### Retrospective evaluation of our model's performance with and without wastewater data over the winter 2023-2024 season
- Use time-stamped data sets of hospital admissions and site-level wastewater data under two data lags:
  - 9 day hospital data delay:  as if submitting on Monday from data available from wastewater as of Monday night, hospital admissions data up until previous Saturday
  - 4 day hospital data delay: as if submitting on Wednesday from wastewater data available as of Monday night, hospital admissions data up until most recent Saturday
  - **Note: we only have Monday night's NWSS data for the entire duration. If we can get time stamped datasets, we will use Sunday nights and Tuesday nights data**
 - Proposed time: October 16, 2023 - May 1, 2024 (28 weeks)
- Generate forecasts from hospital admissions only renewal model and site-level infection dynamics wastewater model for all weeks, all states + DC, PR (excluding the US). Exclude location-weeks that don't have wastewater data OR that fit our criteria for insufficient wastewater (delayed by 21 days or < max of 5 points per site)
- Compare forecast performance on hospital admissions (nowcasts + forecasts) using proper scoring rules: CRPS for the 28 day ahead forecasts of hospital admissions and compare coverage and bias. We will use the [`scoringutils` R package](https://github.com/epiforecasts/scoringutils) to compute these metrics.

### Real-time evaluation compared to the Hub models
From Feb 5, 2024 - May 4, 2024, use the archived real-time outputs to compare what we would have submitted real-time, for 3 sets of outputs: (i) all wastewater where possible (ii) hospital admissions only (iii) the human judgment filtered wastewater model that we submitted for all locations
  - Use WIS for [log-transformed incidence](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1011393), starting at the most granular (location and forecast date) and exploring different summary options
  - Compare to the ensemble + top 3 performing Hub models by WIS for log transformed incidence over the period we submitted (Feb 5 - May 4).
Potential secondary analysis: hypothetical hub submissions with reduced admissions data latency.
   - Use the exact codebase from Monday's run to generate forecasts on Wednesday using Tuesday's wastewater data, compare outputs from target end dates on Monday from Hub models to our Wednesday wastewater + hospital admissions outputs.
   - We will set up infrastructure to save these outputs going forward (starting on 2/28), but we are not planning to include a comparative analysis of Monday vs  ednesday runs in the first draft of the preprint.


 ### Empirical evaluation of impact of wastewater characteristics on forecast performance
US states are heterogeneous in their wastewater data quantity, coverage, and reporting lag. We can leverage this heterogeneity to investigate empirically which characteristics are most critical to improving forecast performance.
- Use the results from the head-to-head model performance in each state and forecast date to assess the impact of:
    - number of wastewater sites
    - proportion of state population covered by the sites combined
    - reporting lag across sites
    - sample collection cadence across sites
- Capture for each forecast date and location,
  - the difference between the two models' outputs: distance metric (which one?)
  - the difference in forecast performance between the two models: compare CRPS scores
  - We might need to average across forecast dates and locations to get something meaningful/interpretable, but propose we start more granular and go up. Final output could be as simple as avg change in forecasts vs wastewater characteristic avg model performance gain vs wastewater characteristic?

## Additional questions that we might consider addressing
These are probably not in scope now, but they might be worth thinking about in future work
1. What is the added value of incorporating hospital admissions data AND combining wastewater data across sites using a renewal framework at improving forecasting of wastewater concentrations?
 - This would entail coming up with two comparison model for wastewater
   - (1) fits to individual sites alone
   - (2) fits jointly across sites without wastewater data or renewal framework e.g. using brms

The goal here would be to assess the added value of integrating multiple data signals (i.e. _multi-signal fusion_), either just from pooling data across sites without mechanism (might be super helpful in and of itself) or by integrating different types of signals using a semi-mechanistic framework with a shared latent process

2. What is the theoretical impact of the various wastewater characteristics on forecast performance
Since the empirical analysis is subject to a range of biases (for example, states with more sites also probably have better surveillance), follow-up studies to investigate these might be worthwhile. This would entail:
- Simulating data from an underlying process with the ability to vary the number of sites, the proportion of the state population covered by the sites, the average reporting latency, the average collection cadence
- Sub-sampling the observed data to construct datasets with varying lags, number of sites, and population coverage
- Performing a sensitivity analysis across the variables described above, comparing gain in forecast performance across epidemic phases compared to a hospital admissions only model.

# CFA wastewater-informed hospital admissions forecasts

This document describes the data sources, model structure, and implementation details for the wastewater-informed hospital admissions forecasts submitted to the [COVID-19 Forecast Hub](https://github.com/reichlab/covid19-forecast-hub/tree/master) under the name `cfa-wwrenewal`. The methods described will evolve as we continue to develop and improve the model.

We welcome feedback on the model structure and implementation, please feel free to submit an issue directly on GitHub or reach out via this [form](https://www.cdc.gov/forecast-outbreak-analytics/contact-us.html).

For an example of how to fit the model to simulated data, we recommend the [toy data vignette](vignettes/toy_data_vignette.Rmd).

## Data sources

We use two data sources as input to the model: viral genome concentrations in wastewater ("genome concentrations") and incident hospital admissions ("admissions"). We describe each of these data sources in detail below.


### Viral genome concentration in wastewater data

We source viral genome concentration in wastewater from the [CDC National Wastewater Surveillance System](https://www.cdc.gov/nwss/wastewater-surveillance.html) (NWSS) full analytic dataset. Concentrations are typically reported in units of estimated genome copies per unit volume wastewater or per unit mass of dry sludge. At present, we use only measurements per unit volume wastewater.
The dataset is defined in the [data dictionary](https://www.cdc.gov/nwss/files/NWSS-Data-Dictionary_v5.0.0_2023-07-10.xlsx) and is described in plain text on the [NWSS website](https://www.cdc.gov/nwss/reporting.html).
The full dataset is available to researchers [upon request](https://www.cdc.gov/nwss/about-data.html) but a [dashboard](https://www.cdc.gov/nwss/rv/COVID19-nationaltrend.html) with summary statistics is publicly available.

We use the most recent data available at the time of submission to generate the forecast which is typically partly complete for the most recent week.

These data are complex and have many sources of variability and uncertainty. For example,

- Each state has a different number of wastewater sampling sites (e.g., wastewater treatment plants) that each cover a catchment area. This means that different sites cover different populations, and when combining the sites across a state, represent some subset of the state population. This subset varies significantly across states.
- Different sites may use different collection methodologies. These methodologies may vary through time at an individual site.
- Different sites sample at different cadences. Some sites sample nearly daily, while others sample less than once a week. Some sites report that data within the week while others have reporting latencies on the order of weeks.
- Different samples are processed in different labs that may use different methodologies for extraction, concentration, and quantification. These labs may also vary their methodologies through time.
- Not all states submit genome concentrations to [CDC's National Wastewater Surveillance System](https://www.cdc.gov/nwss/wastewater-surveillance.html) (NWSS).

These complexities mean that wastewater data are often difficult to interpret. For example, a change in genome concentration across a state could reflect a change in the underlying number of infections, a change in the number of samples collected or processed, a change in the collection or processing methodologies, a change in the reporting cadence or latency, and/or a change in the viral shedding kinetics of the population (e.g., due to a new variant or a change in the age distribution of cases).

Details about data preprocessing, outlier detection, and aggregation are in the [Appendix](#appendix-wastewater-data-pre-processing).

### Hospital admissions data

We source hospital admissions data for each state from [healthdata.gov](https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh/about_data).
As this data is updated on Friday afternoon with admissions up until the previous Saturday, and we submit forecasts on the following Monday, this means that we have a 9 day delay between the most recent data and when the model is fitted.

Details about data preprocessing and outlier detection are in the [Appendix](#hospital-admissions-pre-processing).

## Model overview

We use three models to forecast COVID-19 hospital admissions.
The models share a common structure and are fit using the same codebase.
The models differ in the data they use as input, in whether or not they include a wastewater component, and the aggregation of the estimates they produce.

[**Model 1**](#model-1) is wastewater-informed, site-level infection dynamics model.
It uses genome concentrations at the site level and admissions at the state level as input and produces forecasts of admissions at the state level.
We use Model 1 to generate forecasts for states with any wastewater data available.
However, if a state has not reported any genome concentration data in the past 21 days or if there are less than 5 data points per site in any site, we flag this in our metadata, as it is unlikely that the wastewater data is meaningfully informing the forecast in that location.

[**Model 2**](#model-2) is a no-wastewater, state-level infection dynamics model.
It uses state-level admissions as input and produces forecasts of state-level admissions as output.
We use Model 2 to generate forecasts for states without any wastewater data.

[**Model 3**](#model-3) is a nationwide aggregated wastewater model.
It uses nationally-aggregated genome concentrations and national-level admissions as input and produces forecasts of national-level admissions.
We use Model 3 to generate nationwide forecasts.
Of the forecasts being submitted to the [COVID-19 Forecast Hub](https://github.com/reichlab/covid19-forecast-hub/), only the nationwide (US) forecasts use this observation model.

| Model | Input WW data | Input hospitalization data | Infection dynamics | Predicted hospitalizations |
| --- | --- | --- | --- | --- |
| Model 1 | Site-level | State-level | Coupled site- and state-level | State-level |
| Model 2 | None | State-level | State-level | State-level |
| Model 3 | Nationally aggregated | Nationally aggregated | National | National |


We provide a metadata table alongside the forecasts that indicate notes on the wastewater data for each state, indicating if it does not have any wastewater data from the past 90 days.
For states with minimal or delayed wastewater data, we will still fit Model 1, but the metadata will indicate that the wastewater data is likely insufficient to be inform the forecast.

### Model components

Our models are constructed from a set of generative components. These are:

- [**Infection component:**](#infection-component) A renewal model for the infection dynamics, which generates estimates of incident latent infections per capita.
- [**Hospital admissions component:**](#hospital-admissions-component) A model for the expected number of hospital admissions given incident latent infections per capita.
- [**Viral genome concentration in wastewater:**](#wastewater-component) A model for the expected genome concentration given incident infections per capita.

Depending on the model, these components are implemented at different spatial scales and with different observation processes. In particular, the link to the observables depends on the model and the form of the observables, see below for detailed descriptions of each component and the following sections for model-specific details.

See the [notation](#appendix-notation) section for an overview of the mathematical notation we use to describe the model components, including how probability distributions are parameterized.

### Infection component

#### Renewal process for incident infections

This component assumes that latent (unobserved) _expected_ incident infections per capita $I(t)$ are generated from a renewal [^Cori][^EpiNow2][^Epidemia] process described by:
$$I(t) = \mathcal{R}(t) \sum_{\tau = 1}^{T_g} I(t-\tau) g(\tau)$$
Where $g(\tau)$ is the discrete generation interval, which describes the distribution of times from incident infection to secondary infection (i.e. infectiousness profile) and $\mathcal{R}(t)$ is the instantaneous reproduction number, representing the expected number of secondary infections occurring at time $t$, divided by the number of currently infected individuals, each scaled by their relative infectiousness at time $t$ [^Gostic2020]. $T_g$ is the maximum generation interval, which is the maximum time from infection to secondary infection that we consider, and is set to 15 days.

This process is initialized by estimating an initial exponential growth[^EpiNow2] of infections for 50 days prior to the calibration start time $t_0$:

$$ I(t) = I_0\exp(rt) $$

where $I_0$ is the initial per capita infection incident infections and $r$ is the exponential growth rate.

#### Instantaneous reproduction number

We decompose the instantaneous reproduction number $\mathcal{R}(t)$ into two components: an _unadjusted_ instantaneous reproduction number $\mathcal{R}^\mathrm{u}(t)$ and a damping term that accounts for the effect of recent infections on the instantaneous reproduction number[^Asher2018].

We assume that the unadjusted reproduction number $\mathcal{R}^\mathrm{u}(t)$ is a piecewise-constant function with weekly change points (i.e., if $t$ and $t'$ are days in the same week, then $\mathcal{R}^\mathrm{u}(t)$ = $\mathcal{R}^\mathrm{u}(t')$ ). To account for the dependence of the unadjusted reproduction number in a given week on the previous week, we use a differenced auto-regressive process for the log-scale reproduction number. A log-scale representation is used to ensure that the reproduction number is positive and so that week-to-week changes are multiplicative rather than additive.

$$
\log[\mathcal{R}^\mathrm{u}(t_3)] \sim \mathrm{Normal}\left(\log[\mathcal{R}^\mathrm{u}(t_2)] + \beta \left(\log[\mathcal{R}^\mathrm{u}(t_2)] - \log[\mathcal{R}^\mathrm{u}(t_1)]\right), \sigma_r \right)
$$

where $t_1$, $t_2$, and $t_3$ are days in three successive weeks, $\beta$ is an autoregression coefficient which serves to make week-to-week changes correlated, and $\sigma_r$ determines the overall variation in week-to-week changes.
We bound $\beta$ to be between 0 and 1 so that any changes in trend in $\mathcal{R}^\mathrm{u}(t)$ are damped over time to a degree determined by $\beta$.

The damping term we use is based on Asher et al. 2018[^Asher2018] but extended to be applicable to a renewal process. It assumes that the instantaneous reproduction number is damped by recent infections weighted by the generation interval. This is a simple way to account for the fact that the instantaneous reproduction number is likely to decrease when there are many infections in the population, due to factors such as immunity, behavioral changes, and public health interventions. The damping term is defined as:

$$ \mathcal{R}(t) = \mathcal{R}^\mathrm{u}(t) \exp \left( -\gamma \sum_{\tau = 1}^{T_f}I(t-\tau)g(\tau) \right) $$

where $\gamma$ is the _infection feedback term_ controlling the strength of the damping on $\mathcal{R}(t)$, and the summation is analogous to the "force of infection." See [Prior Distributions](#prior-distributions) below for description of prior choice on $\gamma$.

### Hospital admissions component

Following other semi-mechanistic renewal frameworks, we model the _expected_ hospital admissions per capita $H(t)$ as a convolution of the _expected_ latent incident infections per capita $I(t)$, and a discrete infection to hospitalization distribution $d(\tau)$, scaled by the probability of being hospitalized $p_\mathrm{hosp}(t)$.

To account for day-of-week effects in hospital reporting, we use an estimated _weekday effect_ $\omega(t)$. If $t$ and $t'$ are the same day of the week, $\omega(t) = \omega(t')$. The seven values that $\omega(t)$ takes on are constrained to have mean 1.

$$H(t) = \omega(t) p_\mathrm{hosp}(t) \sum_{\tau = 0}^{T_d} d(\tau) I(t-\tau)$$

Where $T_d$ is the maximum delay from infection to hospitalization that we consider.

We define the discrete hospital admissions delay distribution $d(\tau)$ as a convolution of the incubation period distribution [^Park2023] and a separate estimate of the distribution of time from symptom onset to hospital admission (see [Parameter section](#model-parameters) below for further details).
#### Infection-hospitalization rate
In the models that include fits to wastewater data, we allow the population-level infection-hospitalization rate (IHR) to change over time. An inferred change in the IHR could reflect either a true change in the rate at which infections result in hospital admissions (e.g. the age distribution of cases could shift, a more or less severe variant could emerge, or vaccine coverage could shift) or a change in the relationship between infections and genomes shed in wastewater $G$ (which we currently treat as fixed, but which could change in time if, for example, immunity reduces wastewater shedding without reducing transmission, or a variant emerges with a different per infection wastewater shedding profile).

Therefore, we model the hospitalization proportion $p_\mathrm{hosp}(t)$ as a piecewise-constant function with weekly change points.
If $t$ and $t'$ are two days in the same week, then $p_\mathrm{hosp}(t) = p_\mathrm{hosp}(t')$.

The values $p_\mathrm{hosp}(t)$ follow a random walk.
For $t_1$ and $t_2$ in two successive weeks:

$$ \mathrm{logit} (p_{\mathrm{hosp}}(t_2)) \sim \mathrm{Normal}(\mathrm{logit}(p_{\mathrm{hosp}}(t_1)), \sigma_p)$$

The process is initialized at the calibration start time $t_0$ via a prior distribution (see [Prior Distributions](#prior-distributions) below).

In hospital admissions only models, we model the IHR as constant. We assign this constant IHR the same prior distribution we assign to the intercept of the time-varying IHR in the wastewater-informed models.

### Hospital admissions observation model
We model the observed hospital admission counts $h_t$ as:

$$h_t \sim \mathrm{NegBinom}(n H(t), \phi)$$

where the state population size $n$ is used to convert from per-capita hospitalization rate $H(t)$ to hospitalization counts.

Currently, we do not explicitly model the delay from hospital admission to reporting of hospital admissions. In reality, corrections (upwards or downwards) in the admissions data after the report date are possible and do happen. See [outlier detection and removal](#appendix-wastewater-data-pre-processing) for further details.

### Viral genome concentration in wastewater component

We model viral genome concentrations in wastewater $C(t)$ as a convolution of the _expected_ latent incident infections per capita $I(t)$ and a normalized shedding kinetics function $s(\tau)$, multiplied by  $G$ the number of genomes shed per infected individual over the course of their infection and divided by $\alpha$ the volume of wastewater generated per person per day:

$$C(t) = \frac{G}{\alpha} \sum_{\tau = 0}^{\tau_\mathrm{shed}} s(\tau) I(t-\tau)$$

where $\tau_\mathrm{shed}$ is the total duration of fecal shedding.
Note there is no need to scale by wastewater catchment population size because $I(t)$ is measured as new infections per capita.

This approach assumes that $G$ and $\alpha$ are constant through time and across individuals.
In fact, there is substantial inter-individual variability in shedding kinetics and total shedding.
This approximation is more accurate when population sizes are large.

We model the shedding kinetics $s(\tau)$ as a discretized, scaled triangular distribution[^Larremore2021]:

```math
\log_{10}[s^\mathrm{cont}(\tau)] = \begin{cases}
  V_\mathrm{peak} \frac{\tau}{\tau_\mathrm{peak}} & \tau \leq \tau_\mathrm{peak} \\
  V_\mathrm{peak} \left( 1 - \frac{\tau - \tau_\mathrm{peak}}{\tau_\mathrm{shed} - \tau_\mathrm{peak}} \right) & \tau_\mathrm{peak} < \tau \leq \tau_\mathrm{shed} \\
  0 & \tau > \tau_\mathrm{shed}
\end{cases}
```

where $V_\mathrm{peak}$ is the peak number or viral genomes shed on any day, $\tau_\mathrm{peak}$ is the time from infection to peak shedding, and $\tau_\mathrm{shed}$ is the total duration of shedding. Then:

We model the log observed genome concentrations as Normally distributed:

$$
\log[c_t] \sim \mathrm{Normal}(C(t), \sigma_c)
$$

This component does not mechanistically simulate each step involved in sample collection, processing, and reporting.
Instead, it aims to to account for these processes, at a summary level.
Future iterations of this model will evaluate the utility of mechanistic modeling of wastewater collection and processing.

## Model 1: Site-level infection dynamics

In this model, we represent hospital admissions at the state-level, and viral genome concentrations at the site-level. We represent site-level infection dynamics using a hierarchical model: site-level infection dynamics are distributed about a central state-level infection dynamic.

This model uses all of the components described above with following modifications.

### Site-level infections

We couple the site- and state-level dynamics at the level of the un-damped instantaneous reproduction number $\mathcal{R}^\mathrm{u}(t)$.

We assume that the populations represented by wastewater treatment sites have infection dynamics that are _similar_ to one another but can vary from the state-level mean.

We represent this with a hierarchical model where we first model a state-level un-damped effective reproductive number $\mathcal{R}^\mathrm{u}(t)$, but then allow individual sites $i$ to have individual site-level values of $\mathcal{R}^\mathrm{u}_{i}(t)$

The state-level model for $\mathcal{R}^\mathrm{u}(t)$ is the same as the infection model described above, with $\mathcal{R}^\mathrm{u}(t)$ generating the state-level hospital admissions.
Site-level deviations from the state-level reproduction number are modeled via a log-scale AR(1) process. Specifically, for site $i$:

$$
\log[\mathcal{R}^\mathrm{u}_{i}(t)] = \log[\mathcal{R}^\mathrm{u}(t)] + \delta_i(t)
$$

where $\delta_i(t)$ is the time-varying site effect on $\mathcal{R}(t)$, modeled as,

$$\delta_i(t) = \varphi \delta_i(t-1) + \epsilon_{it}$$

where $0 < \varphi < 1$ and $\epsilon_{it} \sim \mathrm{Normal}(0, \sigma_\delta)$.

The site-level $\mathcal{R}_{i}(t)$ is also subject to the infection feedback described above such that:

```math
\mathcal{R}_i(t) = \mathcal{R}^\mathrm{u}_i(t) \exp \left(-\gamma \sum_{\tau = 1}^{T_f} I_i(t-\tau) g(\tau) \right)
```

From $\mathcal{R}_{i}(t)$, we generate estimates of site-level _expected_ latent incident infections per capita $I_i(t)$ using the renewal process described in [the infection component](#infection-component).

We infer the site level initial per capita incidence $I_i(0)$ hierarchically. Specifically, we treat $\mathrm{logit}[I_i(0)]$ as Normally distributed about the corresponding state-level value $\mathrm{logit}[I(0)]$, with an estimated standard deviation $\sigma_{i0}$:

$$
\mathrm{logit}[I_i(0)] \sim \mathrm{Normal}(\mathrm{logit}[I(0)], \sigma_{i0})
$$

### Viral genome concentration in wastewater

We model site-specific viral genome concentrations in wastewater $C_i(t)$ independently for each site $i$ using the same model as described in [the wastewater component](#wastewater-viral-concentration-component)

Genome concentration measurements can vary between sites, and even within a site through time, because of differences in sample collection and lab processing methods. To account for this variability, we add a scaling term $M_{ij}$ and a variablity term $\sigma_{cij}$ that vary across sites $i$ and also within sites across labs $j$:

$$\log[c_{ijt}] \sim \mathrm{Normal}(\log[M_{ij} C_i(t)], \sigma_{cij})$$

Both $M_{ij}$ and $\sigma_{cij}$ are modeled as site-level random effects.

In the rare cases when a site submits multiple concentrations for a single date and lab method, we treat each record as an independent observation.

### Censoring of wastewater observations below the limit of detection

Lab processing methods have a finite limit of detection (LOD), such that not all wastewater measurements can be modeled using the log-normal approach above.
This limit of detection varies across sites, between methods, and potentially also over time.

If an observed value $c_{ijt}$ is above the corresponding LOD, then the likelihood is:

$$
f_\mathrm{Normal}(\log[c_{ijt}]; \log[M_{ij} C_i(t)], \sigma_{cij})
$$

where $f_\mathrm{Normal}(x; \mu, \sigma)$ is the probability density function of the Normal distribution.
When the observed value is below the LOD, we use a censored likelihood:

```math
\int_{-\infty}^{\log [\mathrm{LOD}_{ijt}]} f_\mathrm{Normal}(x; \log[M_{ij} C_i(t)], \sigma_{cij}) \mathrm{d}x
```

(This is mathematically equivalent to integrating the probability density function of the log-normal distribution from zero to the LOD.)

If a sample is flagged in the NWSS data as below the LOD (field `pcr_target_below_lod`) but is missing a reported LOD (field `lod_sewage`), the 95th percentile of LOD values across the entire data is used as the integral's upper limit.

If a sample has a reported concentration (field `pcr_target_avg_conc`) above the corresponding reported LOD, but the sample is nevertheless flagged as below the LOD (field `pct_target_below_lod`), we assume the flag takes precedence and treat the sample as below LOD for the purposes of censoring.

## Model 2: no wastewater

The no wastewater, state-level infection dynamics model is the simplest model, because it does not include wastewater data.
Each state is modeled as a separate population according to the infection component and hospitalization component described above.
We use this model when no wastewater data is available for a state.
This model also serves as a baseline for comparison to the wastewater-informed models.

## Model 3: nationwide wastewater

In this model, the entire US population is treated as a single population. This model uses the general infection, hospitalization, and wastewater components described above, but with the following modifications:

We first generate a thresholded population-weighted average viral genome concentration in wastewater:

1. For each week and site, if the site has more than one sample in that week, compute the mean genome concentration across those samples. This associates each site and week with a single genome concentration.
1. For each week, compute the weighted mean genome concentration across all sites. The weights are the site population, or 300,000, whichever is lower. The aim of this thresholding is to prevent large sites from dominating the average concentration.

This approach is similar to the algorithm used by Biobot Analytics to derive regional and national aggregate genome concentration.
Future iterations of the model will evaluate the validity of this approach.

We then use this thresholded population-weighted average concentration as the input to the [wastewater viral concentration component](#wastewater-viral-concentration-component).

## Model parameters

### Prior distributions

We use informative priors for parameters that have been well characterized in the literature and weakly informative priors for parameters that have been less well characterized.

| Parameter | Prior distribution | Source |
|---|---|---|
| Initial hospitalization probability | $\mathrm{logit}[p_{\mathrm{hosp}}(t_0)] \sim \mathrm{Normal}(\mathrm{logit}[0.01], 0.3)$ | Perez-Guzman et al. 2023 [^Perez] |
| Time to peak fecal shedding |  $\tau_\mathrm{peak} \sim \mathrm{Normal}(5 \text{ days}, 1 \text{ day})$ | Russell et al. 2023 [^Russell], Huisman et al. 2022 [^Huisman], Cavany et al. 2022 [^Cavany] |
| Peak viral shedding $V_\mathrm{peak}$| $\log_{10}[V_\mathrm{peak}] \sim \mathrm{Normal}(5.1, 0.5)$ | Miura et al. 2021 [^Muira] |
| Duration of shedding | $\tau_\mathrm{shed} \sim \mathrm{Normal}(17 \text{ days}, 3 \text{ days})$  | Cevik et al. 2021 [^Cevik], Russell et al. 2023 [^Russell]   |
| Total genomes shed per infected individual | $\log_{10}[G] \sim \mathrm{Normal}(9, 2)$ | Watson et al 2023[^Watson]   |
| Initial infections per capita $I_0$ | $I_0 \sim \mathrm{Beta}(1 + k i_\mathrm{est}, 1 + k (1-i_\mathrm{est}))$ | where $i_\mathrm{est}$ is the sum of the last 7 days of hospital admissions, divided by state population, and divided by the prior mode for $p_\mathrm{hosp}$, and $k = 5$ is a parameter governing the informativeness ("certainty") of the Beta distribution |
| Initial exponential growth rate | $r \sim \mathrm{Normal}(0, 0.01)$ | Chosen to assume flat dynamics prior to observations |
| Infection feedback term | $\gamma \sim \mathrm{logNormal}(6.37, 0.4)$ | Weakly informative prior chosen to have a mode of 500 in natural scale, based on posterior estimates of peaks from prior seasons in a few locations|

### Scalar parameters

| Parameter | Value | Source |
|---|---|---|
| Maximum generation interval | $T_g = 15$ days | |
| Maximum infection to hospital admissions delay | $T_d = 55$ days| |
| Wastewater produced per person-day | $\alpha=$ 378,500 mL per person-day | Ortiz 2024[^Ortiz] |

### Distributional parameters

The discrete generation interval probability mass function $g(\tau)$ approximates a log-normal distribution[^Park2023] with log-mean 2.9 and log-standard deviation of 1.64.
To approximate the double censoring process necessary to discretize the continuous log-normal distribution, we use a simulation-based approach as recommended by Park et al.[^Park2024].
This assumes that the primary event is uniformly distributed (this ignores the influence of the growth rate within the primary interval but is a good approximation in most settings). The secondary event is then a sum of this primary interval and the continuous distribution and is observed within a day (see Figure 9 in [^Park2024]).
As the renewal process is not defined if there is probability mass on day zero we further left truncate this distribution.
For more details refer to [^Park2024].

We derive the distribution $\delta(\tau)$ of the delay from infection to hospital admission as the sum of the incubation period (delay from infection to symptom onset) and the period from symptom onset to hospital admission.

We model the incubation period with a discretized, modified Weibull distribution[^Park2023] with probability mass function $\delta(\tau)$:

$$
\delta^\mathrm{cont}(\tau) = \exp[0.15\tau] f_\mathrm{Weibull}(\tau; \mathrm{shape}=1.5, \mathrm{scale}=3.6)
$$

```math
\delta(\tau) = \begin{cases}
\delta^\mathrm{cont}(\tau) / \left( {\sum}_{\tau'=0}^{23} g^\mathrm{cont}(\delta') \right) & 0 \leq \tau \leq 23 \\
0 & \text{otherwise}
\end{cases}
```

We model the symptom onset to hospital admission delay distribution with a Negative Binomial distribution with probability mass function $\gamma(\tau)$ fit to line list patient data from Dananché et al. 2022[^Danache2022].

$$
\gamma(\tau) = f_\mathrm{NegBin}(\tau; 6.99 \text{ days}, 2.49 \text{ days})
$$

The infection-to-hospitalization delay distribution $d(\tau)$ is the convolution:

$$
d(\tau) = \sum_{x=0}^\tau \delta(x) \gamma(\tau - x)
$$

This resulting infection to hospital admission delay distribution has a mean of 12.2 days and a standard deviation of 5.67 days.

## Implementation

Our framework is an extension of the widely used [^CDCRtestimates] [^CDCtechnicalblog], semi-mechanistic renewal framework `{EpiNow2}` [^epinow2paper][^EpiNow2], using a Bayesian latent variable approach implemented in the probabilistic programming language Stan [^stan] using [^cmdstanr] to interface with R.
For submission to the [COVID-19 Forecast Hub](https://github.com/reichlab/covid19-forecast-hub/tree/master), the model is run on Saturday to generate forecasts each Monday.
For each location, we run 4 chains for 250 warm-up iterations and 500 sampling iterations, with a target average acceptance probability of 99% and a maximum tree depth of 12.
To generate forecasts per the hub submission guidelines, we calculate the necessary quantiles from the 2,000 draws from the posterior of the expected observed hospital admissions 28 days ahead of the Monday forecast date.

## Appendix: Notation

The notation $X \sim \mathrm{Distribution}$ indicates that a random variable $X$ is distributed according to a given distribution.

We parameterize Normal distributions in terms of their mean and standard deviation: $\mathrm{Normal}(\mathrm{mean, standard\ deviation})$.

We parameterize Beta distributions in terms of their two standard shape parameters $\alpha$ and $\beta$, which can be [interpreted in terms of the counts of observed successes and failures](https://stats.stackexchange.com/questions/47771/what-is-the-intuition-behind-beta-distribution), respectively, in a Binomial experiment to infer a probability: $\mathrm{Beta}(\alpha, \beta)$.

We parameterize Negative Binomial distributions in terms of their mean and their positive-constrained dispersion parameter (often denoted $\phi$): $\mathrm{NegBinom}(\mathrm{mean, dispersion})$. As the dispersion parameter goes to 0, a Negative Binomial distribution becomes increasingly over-dispersed. As it goes to positive infinity, the Negative Binomial approximates a Poisson distribution with the same mean.

We write $\mathrm{logit}(x)$ to refer to the logistic transform: $\mathrm{logit}(x) \equiv \log(x) - \log(1 - x)$.

Observed data are labeled by data source: $c$ for wastewater concentrations, $h$ for hospital admissions.
Hospitalization data are indexed by day $t$ (i.e., $h_t$).
Wastewater data are indexed by site $i$, wastewater testing lab $j$, and day $t$ (e.g., $c_{ijt}$).

## Appendix: Wastewater data pre-processing

Field names are references to [fields in the analytic dataset](https://www.cdc.gov/nwss/files/NWSS-Data-Dictionary_v5.0.0_2023-07-10.xlsx).

### Data filtering

- Primary wastewater treatment plants only (field `sample_location` equals `wwtp`)
- SARS-CoV-2 amplification targets only (field `pcr_target` equals `sars-cov-2`)
- No solid samples (field `sample_matrix` is not `primary_sludge`, and field `pcr_target_units` is not `copies/g dry sludge`)
- No samples flagged for quality issues (field `quality_flag` is not `yes`)
- Outliers removed (see below)

### Viral genome concentration in wastewater outlier detection and removal

We identify potential outlier genome concentrations for each unique site and lab pair with an approach based on $z$-scores.

Briefly, we compute $z$-scores for the concentrations and their finite differences and remove any observations above a threshold values for either metric. In detail:

1. For purposes of outlier detection, exclude wastewater observations below the LOD.
1. For purposes of outlier detection, exclude observations more than 90 days before the forecast date.
1. For each site $i$, compute the change per unit time between successive observations $t$ and $t'$: $(\log[c_{it'}] - \log[c_{it}])/(t' - t)$.
1. Compute $z$-scores for $\log[c_{it}]$ across all sites $i$ and timepoints $t$. Flag values with $z$-scores over 3 as outliers and remove them from model calibration.
1. Compute $z$-scores for the change per unit time values across all sites and pairs of timepoints. For values with $z$-scores over 2, flag the corresponding wastewater concentrations $c_{it}$ as outliers and remove them from model calibration.

The $z$-score thresholds were chosen by visual inspection of the data.

### Other data pre-processing

- For consistency, the reported viral genome concentration in wastewater (field `pcr_target_avg_conc`) and LOD (field `lod_sewage`) are converted to genome copies per mL wastewater using the reported measurement units (field `pcr_target_units`).
- For wastewater catchment population $n_{it}$, we use field `population_served`.
- We identify the unique combinations of sites and labs and add this as a column to our pre-processed dataset.

### Hospital admissions pre-processing

We visually inspect the hospital admissions data for each state before producing a forecast, identifying anomalies that seem implausible.
We then remove these observations from the model inference, treating them as missing data (i.e. as NA values in the model).
Often these implausible observations are due to reporting errors, such as a hospital reporting a large number of admissions on a single day that should have been spread out over multiple days and are later corrected. When this happens, we add the corrected data back into the model inference when it gets updated.

## References

[^epinow2paper]: Abbott, S. et al. Estimating the time-varying reproduction number of SARS-CoV-2 using national and subnational case counts. _Wellcome Open Res_. 5:112 (2020). https://doi.org/10.12688/wellcomeopenres.16006.2
[^Asher2018]: Asher, J. Forecasting Ebola with a regression transmission model. _Epidemics._ **22**, 50-55 (2018). https://doi.org/10.1016/j.epidem.2017.02.009
[^stan]: Stan Development Team. _Stan Modeling Language Users Guide and Reference Manual_. (2023). https://mc-stan.org
[^CDCRtestimates]: US Centers for Disease Control and Prevention. _Current Epidemic Growth Status (Based on Rt) for States and Territories_. https://www.cdc.gov/forecast-outbreak-analytics/about/rt-estimates.html (2024).
[^CDCtechnicalblog]: US Centers for Disease Control and Prevention. _Technical Blog: Improving CDC’s Tools for Assessing Epidemic Growth_ (2024). https://www.cdc.gov/forecast-outbreak-analytics/about/technical-blog-rt.html
[^Park2023]: Park, S.W. et al. Inferring the differences in incubation-period and generation-interval distributions of the Delta and Omicron variants of SARS-CoV-2. _Proc Natl Acad Sci U S A_. 120(22):e2221887120 (2023). https://doi.org/10.1073/pnas.2221887120
[^Danache2022]: Danaché, C. et al. Baseline clinical features of COVID-19 patients, delay of hospital admission and clinical outcome: A complex relationship. _PLoS One_ 17(1):e0261428 (2022). https://doi.org/10.1371/journal.pone.0261428
[^Perez]: Perez-Guzman, P.N. et al. Epidemiological drivers of transmissibility and severity of SARS-CoV-2 in England. _Nat Commun_ 14, 4279 (2023). https://doi.org/10.1038/s41467-023-39661-5
[^Muira]: Miura F, Kitajima M, Omori R. Duration of SARS-CoV-2 viral shedding in faeces as a parameter for wastewater-based epidemiology: Re-analysis of patient data using a shedding dynamics model. _Sci Total Environ_ 769:144549 (2021). https://doi.org/10.1016/j.scitotenv.2020.144549
[^Huisman]: Huisman, J.S. et al. Estimation and worldwide monitoring of the effective reproductive number of SARS-CoV-2 _eLife_ 11:e71345 (2022). https://doi.org/10.7554/eLife.71345
[^Cavany]: Cavany S, et al. Inferring SARS-CoV-2 RNA shedding into wastewater relative to the time of infection. _Epidemiology and Infection_ 150:e21 (2022). https://doi.org/10.1017/S0950268821002752
[^Russell]: Russell, T.W. et al. Within-host SARS-CoV-2 viral kinetics informed by complex life course exposures reveals different intrinsic properties of Omicron and Delta variants. _medRxiv_ (2023).  https://doi.org/10.1101/2023.05.17.23290105
[^Cevik]: Cevik, M. et al. SARS-CoV-2, SARS-CoV, and MERS-CoV viral load dynamics, duration of viral shedding, and infectiousness: a systematic review and meta-analysis. _Lancet Microbe_ **2(1)**,e13-e22 (2021). https://doi.org/10.1016/S2666-5247(20)30172-5
[^Watson]: Leighton, M. et al. Improving estimates of epidemiological quantities by combining reported cases with wastewater data: a statistical framework with applications to COVID-19 in Aotearoa New Zealand. _medRxiv_ (2023). https://doi.org/10.1101/2023.08.14.23294060
[^Ortiz]: Ortiz, P. _Wastewater facts - statistics and household data in 2024_. https://housegrail.com/wastewater-facts-statistics/
[^Larremore2021]: Larremore, D.B. et al. Test sensitivity is secondary to frequency and turnaround time for COVID-19 screening. _Science Advances_ (2021). https://doi.org/10.1126/sciadv.abd5393
[^Cori]: Cori, A., Ferguson, N. M., Fraser, C., & Cauchemez, S. A new framework and software to estimate time-varying reproduction numbers during epidemics. _Am. J. Epidemiol._ **178**, 1505-1512 (2013). https://doi.org/10.1093/aje/kwt133
[^EpiNow2]: Abbott, S. et al. _EpiNow2: Estimate real-time case counts and time-varying epidemiological parameters._ https://doi.org/10.5281/zenodo.3957489
[^Epidemia]: Fraser, C. (2007). Estimating individual and household reproduction numbers in an emerging epidemic. _PLoS One_, **2**(8), e758 (2007). https://doi.org/10.1371/journal.pone.0000758
[^cmdstanr]: _CmdStanR: the R interface to CmdStan_. (2024). https://mc-stan.org/cmdstanr/index.html
[^Park2024]: Park, S.W. et al. Estimating epidemiological delay distributions for infectious diseases.
_medRxiv_ (2024). https://doi.org/10.1101/2024.01.12.24301247
[^Gostic2020]: Gostic, K.M. et al. Practical Considerations for Measuring the Effective Reproductive Number, Rt. _PLoS Comput Biol_. **16**(12) (2020). https://doi.org/10.1371/journal.pcbi.1008409

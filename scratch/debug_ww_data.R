# Take a look at the SCAN data
library(zoo)
library(ggridges)
library(magrittr)

nwss <- readr::read_csv(here::here("input", "ww_data", "nwss_data", "2024-01-30.csv"))

test <- nwss %>% dplyr::filter(lab_id == 4702)

# EDA on outliers
nwss_subset <- wweval::init_subset_nwss_data(nwss)

ww_target_type <- "pcr_target_avg_conc"
ww_data <- nwss_subset %>%
  ungroup() %>%
  rename(
    date = sample_collect_date,
    ww = {{ ww_target_type }},
    ww_pop = population_served
  ) %>%
  mutate(
    location = toupper(wwtp_jurisdiction),
    site = wwtp_name,
    lab = lab_id
    # since we might expect to see
  ) %>%
  select(
    date, location, ww, site, lab, lab_wwtp_unique_id, ww_pop,
    below_LOD, lod_sewage
  )

# Add the county names to the WW data
site_county_map <- get_site_county_map(
  nwss,
  county_site_map_path = file.path("input", "ww_data", "county_site_map.csv")
)
ww_data2 <- ww_data %>%
  left_join(site_county_map,
    by = "site"
  ) %>%
  mutate(
    full_county_name = ifelse(is.na(full_county_name),
      glue::glue("{county_codes}, {location}"),
      full_county_name
    )
  )
ww_data_outliers_flagged <- wwinference::flag_ww_outliers(ww_data2)

ww_data_mod <- ww_data_outliers_flagged %>%
  group_by(lab_wwtp_unique_id) %>%
  arrange(date, "desc") %>%
  mutate(
    log_conc = log(ww)
  ) %>%
  mutate(
    log_conc_t_min_1 = lag(log_conc, 1),
    log_conc_t_min_2 = lag(log_conc, 2),
    log_conc_t_plus_1 = lead(log_conc, 1),
    log_conc_t_plus_2 = lead(log_conc, 2)
  ) %>%
  mutate(
    exp_log_conc = mean(
      c(
        log_conc_t_min_1, log_conc_t_min_2,
        log_conc_t_plus_2, log_conc_t_plus_2
      ),
      na.rm = TRUE
    )
  ) %>%
  mutate(
    norm_dif_true_v_exp = abs((log_conc - exp_log_conc) / (exp_log_conc)),
    dif_true_v_exp = abs(log_conc - exp_log_conc)
  )

mean_norm_dif <- mean(ww_data_mod$norm_dif_true_v_exp, na.rm = TRUE)
median_norm_dif <- median(ww_data_mod$norm_dif_true_v_exp, na.rm = TRUE)

ggplot(ww_data_mod) +
  geom_density(
    aes(
      x = norm_dif_true_v_exp,
      fill = as.factor(flag_as_ww_outlier)
    ),
    alpha = 0.3
  ) +
  geom_vline(xintercept = mean_norm_dif, linetype = "dashed") +
  geom_vline(xintercept = median_norm_dif, linetype = "dotted") +
  scale_x_continuous(trans = "log") +
  theme_bw() +
  guides(fill = guide_legend(title = "Outlier?")) +
  xlab("Normalized difference expected and real")

ggplot(ww_data_mod) +
  geom_density(
    aes(
      x = dif_true_v_exp,
      fill = as.factor(flag_as_ww_outlier)
    ),
    alpha = 0.3
  ) +
  scale_x_continuous(trans = "log") +
  theme_bw() +
  guides(fill = guide_legend(title = "Outlier?")) +
  xlab("Difference expected and real")
ggplot(ww_data_mod) +
  geom_density(aes(x = dif_true_v_exp),
    alpha = 0.3
  ) +
  scale_x_continuous(trans = "log") +
  theme_bw() +
  xlab("Difference expected and real")

ggplot(ww_data_mod) +
  geom_density(aes(x = norm_dif_true_v_exp)) +
  geom_vline(xintercept = median_norm_dif, linetype = "dotted") +
  geom_vline(xintercept = mean_norm_dif, linetype = "dashed") +
  scale_x_continuous(trans = "log") +
  theme_bw() +
  xlab("Normalized difference expected and real")

ggplot(ww_data_mod) +
  geom_point(
    aes(
      x = log_conc, y = norm_dif_true_v_exp,
      color = as.factor(flag_as_ww_outlier)
    ),
    size = 0.1, alpha = 0.3
  ) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  theme_bw() +
  guides(color = guide_legend(title = "Outlier?")) +
  ylab("Normalized difference expected and real") +
  xlab("log(conc)")

ggplot(ww_data_mod) +
  geom_point(
    aes(
      x = log_conc, y = dif_true_v_exp,
      color = as.factor(flag_as_ww_outlier)
    ),
    size = 0.1, alpha = 0.3
  ) +
  scale_x_continuous(trans = "log") +
  scale_y_continuous(trans = "log") +
  theme_bw() +
  guides(color = guide_legend(title = "Outlier?")) +
  ylab("Difference expected and real") +
  xlab("log(conc)")








# check for duplicates
test <- nwss %>%
  select(
    sample_collect_date, wwtp_name,
    lab_id
  ) %>%
  unique()
unique_wwtps <- nwss %>%
  select(wwtp_name) %>%
  unique()
unique_labs <- nwss %>%
  select(lab_id) %>%
  unique()
unique_combos_map <- nwss %>%
  select(wwtp_name, lab_id) %>%
  unique() %>%
  mutate(lab_wwtp_unique_id = row_number())

nwss_w_unique_ids <- nwss %>% left_join(unique_combos_map,
  by = c("wwtp_name", "lab_id")
)

test2 <- nwss %>%
  select(sample_collect_date, wwtp_name, lab_id) %>%
  unique()
test3 <- nwss %>%
  select(sample_collect_date, wwtp_name) %>%
  unique()

test <- nwss %>%
  group_by(sample_collect_date, wwtp_name) %>%
  reframe(lab_ids = (unique(lab_id)))

source("R/pre_processing.R")

ww_data_summarized <- nwss %>%
  group_by(wwtp_jurisdiction) %>%
  select(wwtp_name) %>%
  unique() %>%
  summarise(n_sites_per_state = n()) %>%
  left_join(
    nwss %>%
      filter(sample_collect_date >= ymd(today() - months(1))) %>%
      group_by(wwtp_jurisdiction) %>%
      select(wwtp_name) %>%
      unique() %>%
      summarise(n_recent_sites_per_state = n())
  )

median(ww_data_summarized$n_recent_sites_per_state)

ggplot(ww_data_summarized) +
  geom_histogram(aes(x = n_recent_sites_per_state), fill = "blue", alpha = 0.3) +
  geom_vline(aes(xintercept = 14), linetype = "dashed") +
  xlab("Number of sites per state") +
  ylab("Number of states") +
  theme_bw() +
  ggtitle("Distribution of number of sites per state")

ggplot(nwss %>% filter(wwtp_jurisdiction %in% c("nj"))) +
  geom_line(
    aes(
      x = ymd(sample_collect_date), y = pcr_target_avg_conc,
      color = as.factor(wwtp_name)
    ),
    show.legend = FALSE
  ) +
  facet_wrap(~sample_matrix, nrow = 3) +
  xlab("") +
  ylab("Avg PCR concentration") +
  theme_bw() +
  ggtitle("MA WW broken down by type of sampling")

ggplot(nwss %>% filter(wwtp_jurisdiction %in% c("ca"))) +
  geom_line(
    aes(
      x = ymd(sample_collect_date), y = pcr_target_avg_conc,
      color = as.factor(wwtp_name)
    ),
    alpha = 0.5, size = 0.5,
    show.legend = FALSE
  ) +
  # facet_wrap(~wwtp_name, scales = 'free') +
  facet_wrap(~pcr_target_units, nrow = 3) +
  xlab("") +
  ylab("Avg PCR concentration") + # scale_y_log10()+
  theme_bw() +
  ggtitle("CA WW broken down by unit type")







nwss_subset_raw <- wweval::init_subset_nwss_data(nwss)




ggplot(nwss_subset_raw %>% filter(wwtp_jurisdiction %in% c("ny"))) +
  geom_line(
    aes(
      x = ymd(sample_collect_date), y = pcr_target_avg_conc,
      group = wwtp_name, color = wwtp_name
    ),
    show.legend = FALSE
  ) +
  facet_wrap(~wwtp_jurisdiction, scales = "free") +
  xlab("") +
  ylab("Avg PCR concentration") +
  theme_bw()

ggplot(nwss_subset_raw %>% filter(wwtp_name == 2023)) +
  geom_point(
    aes(
      x = ymd(sample_collect_date), y = pcr_target_avg_conc,
      group = wwtp_name, color = wwtp_name
    ),
    show.legend = FALSE
  ) +
  facet_wrap(~wwtp_name, scales = "free") +
  xlab("") +
  ylab("Avg PCR concentration") +
  scale_y_log10() +
  theme_bw()

states <- c("vt", "nj", "ny", "tx", "ct", "ma", "ri", "ca", "al")
states <- "ma"
single_state_raw <- nwss_subset_raw %>%
  remove_outliers() %>%
  filter(
    wwtp_jurisdiction %in% c(states),
    sample_collect_date >= "2022-10-10",
    sample_collect_date <= "2022-12-26"
  )
state <- "ma"
ggplot(nwss_subset_raw %>% remove_outliers() %>% filter(wwtp_jurisdiction == state)) +
  geom_density_ridges_gradient(aes(
    y = ymd(sample_collect_date),
    x = pcr_target_avg_conc,
    group = sample_collect_date
  ), jittered_points = TRUE) +
  scale_fill_viridis_d() +
  coord_flip() +
  facet_wrap(~wwtp_jurisdiction) +
  xlab("Site specific pcr_target_avc_conc ") +
  ylab("") +
  theme_bw() +
  ggtitle(paste0("Within state WW concentration distributions across sites in ", toupper(state)))


ggplot(single_state_raw) +
  geom_line(
    aes(
      x = ymd(sample_collect_date),
      y = pcr_target_avg_conc,
      color = as.factor(wwtp_name)
    ),
    show.legend = FALSE, size = 0.5, alpha = 0.5
  ) +
  theme_bw() +
  facet_wrap(~wwtp_jurisdiction, scales = "free") +
  ylab("Site specific pcr_target_avg_conc") +
  xlab("") +
  labs(color = "Site") +
  ggtitle(paste0("Distribution of concentrations across days and sites "))

ggplot(single_state_raw) +
  geom_line(
    aes(
      x = ymd(sample_collect_date),
      y = pcr_target_flowpop_lin,
      color = as.factor(wwtp_name)
    ),
    show.legend = FALSE, size = 0.5, alpha = 0.5
  ) +
  theme_bw() +
  facet_wrap(~wwtp_jurisdiction, scales = "free") +
  ylab("Site specific pcr_target_flowpop_lin") +
  xlab("") +
  labs(color = "Site") +
  ggtitle(paste0("Distribution of concentrations across days and sites"))



var_by_pop <- nwss_subset_raw %>%
  remove_outliers() %>%
  group_by(wwtp_name) %>%
  summarise(
    pop = max(population_served),
    variance = var(pcr_target_avg_conc)
  )

ggplot(var_by_pop) +
  geom_point(aes(x = pop, y = variance)) +
  theme_bw() +
  xlab("N") +
  ylab("Variance in site level concentrations across time") +
  scale_y_log10() +
  scale_x_log10()





nwss_subset <- remove_outliers(nwss_subset_raw)

omicron <- nwss_subset %>% filter(
  sample_collect_date >= "2022-10-10",
  sample_collect_date <= "2023-01-16"
)

ggplot(omicron %>% filter(wwtp_jurisdiction %in% c("ma"))) +
  geom_line(
    aes(
      x = ymd(sample_collect_date), y = pcr_target_avg_conc,
      group = wwtp_name, color = as.factor(wwtp_name)
    ),
    show.legend = FALSE
  ) +
  xlab("") +
  ylab("Avg PCR concentration") +
  scale_x_date(
    date_breaks = "2 weeks",
    labels = scales::date_format("%Y-%m-%d")
  ) +
  theme(
    axis.text.x = element_text(
      size = 10, vjust = 0.5,
      hjust = 1, angle = 45
    ),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    plot.title = element_text(
      size = 9,
      vjust = 0.5, hjust = 0.5
    )
  ) +
  ggtitle("Site-level PCR concentration in Massachusetts winter 2022-2023") +
  theme_bw()



ggplot(nwss_subset %>% filter(wwtp_name == 2023)) +
  geom_line(
    aes(
      x = ymd(sample_collect_date), y = pcr_target_avg_conc,
      group = wwtp_name, color = wwtp_name
    ),
    alpha = 0.3, show.legend = FALSE
  ) +
  facet_wrap(~wwtp_jurisdiction, scales = "free") +
  xlab("") +
  ylab("Avg PCR concentration") +
  theme_bw()

ggplot(nwss_subset %>% filter(wwtp_jurisdiction %in% c("ny"))) +
  geom_line(
    aes(
      x = ymd(sample_collect_date), y = pcr_target_avg_conc,
      group = wwtp_name, color = wwtp_name
    ),
    alpha = 0.3, show.legend = FALSE
  ) +
  facet_wrap(~wwtp_jurisdiction, scales = "free") +
  xlab("") +
  ylab("Avg PCR concentration") +
  theme_bw()





nwss_by_week <- get_weekly_summary(nwss_subset)




nwss_by_state <- get_state_level_summary(nwss_by_week)
# Naive population-weighted state level averaging with national level averaging
ggplot(Omicron %>% filter(wwtp_jurisdiction %in% c("ma"))) +
  geom_line(
    aes(
      x = ymd(sample_collect_date), y = pcr_target_avg_conc,
      group = wwtp_name, color = as.factor(wwtp_name)
    ),
    show.legend = FALSE
  ) +
  geom_point(
    data = nwss_by_state %>% filter(
      wwtp_jurisdiction == "ma",
      midweek_date >= "2022-10-10",
      midweek_date <= "2023-01-16"
    ),
    aes(x = midweek_date, y = pop_weighted_conc_w_thres),
    shape = 24, fill = "black"
  ) +
  xlab("") +
  ylab("Avg PCR concentration") +
  scale_x_date(
    date_breaks = "2 weeks",
    labels = scales::date_format("%Y-%m-%d")
  ) +
  theme(
    axis.text.x = element_text(
      size = 10, vjust = 0.5,
      hjust = 1, angle = 45
    ),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    plot.title = element_text(
      size = 9,
      vjust = 0.5, hjust = 0.5
    )
  ) +
  ggtitle("Site-level PCR concentration in Massachusetts winter 2022-2023") +
  theme_bw()


omicron_by_state <- nwss_by_state %>% filter(
  midweek_date <= "2021-12-31",
  midweek_date >= "2021-07-01"
)

ggplot(omicron_by_state %>% filter(wwtp_jurisdiction %in% c(
  "ny", "mo", "nc", "ca",
  "va", "ma"
))) +
  geom_line(aes(x = ymd(midweek_date), y = pop_weighted_conc_w_thres),
    show.legend = FALSE
  ) +
  facet_wrap(~wwtp_jurisdiction, scales = "free") +
  xlab("") +
  ylab("Avg PCR concentration") + # scale_y_log10()+
  theme_bw()



ggplot(nwss_by_state %>% filter(
  wwtp_jurisdiction %in% c(
    "ny", "va", "ca",
    "tx", "fl", "ma"
  ),
  midweek_date >= "2023-01-01"
)) +
  geom_line(aes(x = ymd(midweek_date), y = pop_weighted_conc), color = "gray") +
  geom_line(aes(x = ymd(midweek_date), y = unweighted_avg_conc), color = "darkblue") +
  geom_line(aes(x = ymd(midweek_date), y = pop_weighted_conc_w_thres), color = "darkred") +
  geom_line(aes(x = ymd(midweek_date), y = rlng_avg_pop_weighted_conc_w_thres), color = "purple4") +
  facet_wrap(~wwtp_jurisdiction, scales = "free") +
  coord_cartesian(xlim = c(ymd("2023-01-01"), ymd("2023-06-28"))) +
  xlab("") +
  ylab("Avg PCR concentration") +
  theme_bw()

ggplot(nwss_by_state %>% filter(wwtp_jurisdiction %in% c("ny"))) +
  geom_line(aes(x = ymd(midweek_date), y = pop_weighted_conc),
    color = "gray", alpha = 0.5
  ) +
  geom_line(aes(x = ymd(midweek_date), y = unweighted_avg_conc),
    color = "darkblue", alpha = 0.5
  ) +
  geom_line(aes(x = ymd(midweek_date), y = pop_weighted_conc_w_thres),
    color = "darkred", alpha = 0.5
  ) +
  geom_line(aes(x = ymd(midweek_date), y = rlng_avg_pop_weighted_conc_w_thres),
    color = "purple4", alpha = 0.5
  ) +
  geom_line(aes(x = ymd(midweek_date), y = ntl_pop_weighted_conc),
    color = "gray"
  ) +
  geom_line(aes(x = ymd(midweek_date), y = ntl_unweighted_avg_conc),
    color = "darkblue"
  ) +
  geom_line(aes(x = ymd(midweek_date), y = ntl_pop_weighted_conc_w_thres),
    color = "darkred"
  ) +
  geom_line(aes(x = ymd(midweek_date), y = rlng_avg_ntl_pop_weighted_conc_w_thres),
    color = "purple4"
  ) +
  facet_wrap(~wwtp_jurisdiction, scales = "free") +
  coord_cartesian(xlim = c(ymd("2021-01-01"), ymd("2023-06-28"))) +
  xlab("") +
  ylab("Avg PCR concentration") +
  theme_bw() +
  ggtitle("Viral concentration in WW calculated 3 ways")

ggplot(nwss_by_state) +
  geom_line(aes(
    x = ymd(midweek_date), y = pop_weighted_conc_w_thres,
    group = wwtp_jurisdiction
  ), color = "darkred", alpha = 0.1) +
  geom_line(aes(x = ymd(midweek_date), y = ntl_pop_weighted_conc_w_thres),
    color = "black", alpha = 1
  ) +
  coord_cartesian(xlim = c(ymd("2021-01-01"), ymd("2023-06-28"))) +
  xlab("") +
  ylab("Avg PCR concentration") +
  theme_bw() +
  ggtitle("Viral concentration state-level averages")

# Would be cool to use ggdist to plot distributions over time within a state
# across wwtps to see how they vary

ggplot(nwss_by_week %>% filter(wwtp_jurisdiction == "nj")) +
  geom_density_ridges_gradient(aes(
    y = midweek_date,
    x = site_weekly_avg_conc,
    group = midweek_date
  ), jittered_points = TRUE) +
  scale_fill_viridis_d() +
  coord_flip() +
  xlab("Site specific concentration") +
  ylab("") +
  theme_bw() +
  ggtitle("Within state WW concentration distributions across sites")

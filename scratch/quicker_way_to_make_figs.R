## Fig2 combined-------------------------------------------------------------
tar_load(fig2_ct_1)
tar_load(fig2_ct_2)
tar_load(fig2_ct_3)
tar_load(fig2_hosp_t_1)
tar_load(fig2_hosp_t_2)
tar_load(fig2_hosp_t_3)


fig2 <- fig2_hosp_t_1 + fig2_ct_1 +
  fig2_hosp_t_2 + fig2_ct_2 +
  fig2_hosp_t_3 + fig2_ct_3 +
  patchwork::plot_layout(
    guides = "collect",
    nrow = 3, ncol = 2,
    axes = "collect",
    widths = c(1, 1.5)
  ) & theme(
  legend.position = "top",
  legend.justification = "left"
)
fig2

## Fig 3 combined --------------------------------------------------------------
tar_load(fig3_crps_single_loc1)
tar_load(fig3_forecast_comparison_nowcast1)
tar_load(fig3_forecast_comparison_1wk1)
tar_load(fig3_forecast_comparison_4wks1)
tar_load(fig3_crps_underlay_nowcast1)
tar_load(fig3_crps_underlay_1wk1)
tar_load(fig3_crps_underlay_4wks1)
tar_load(fig3_crps_single_loc2)
tar_load(fig3_forecast_comparison_nowcast2)
tar_load(fig3_forecast_comparison_1wk2)
tar_load(fig3_forecast_comparison_4wks2)
tar_load(fig3_crps_underlay_nowcast2)
tar_load(fig3_crps_underlay_1wk2)
tar_load(fig3_crps_underlay_4wks2)
tar_load(fig3_crps_single_loc3)
tar_load(fig3_forecast_comparison_nowcast3)
tar_load(fig3_forecast_comparison_1wk3)
tar_load(fig3_forecast_comparison_4wks3)
tar_load(fig3_crps_underlay_nowcast3)
tar_load(fig3_crps_underlay_1wk3)
tar_load(fig3_crps_underlay_4wks3)


layout <- "
ABCD
AEFG
HIJK
HLMN
OPQR
OSTU
"
fig3 <- fig3_crps_single_loc1 + fig3_forecast_comparison_nowcast1 +
  fig3_forecast_comparison_1wk1 +
  fig3_forecast_comparison_4wks1 + fig3_crps_underlay_nowcast1 +
  fig3_crps_underlay_1wk1 + fig3_crps_underlay_4wks1 +
  fig3_crps_single_loc2 + fig3_forecast_comparison_nowcast2 +
  fig3_forecast_comparison_1wk2 +
  fig3_forecast_comparison_4wks2 + fig3_crps_underlay_nowcast2 +
  fig3_crps_underlay_1wk2 + fig3_crps_underlay_4wks2 +
  fig3_crps_single_loc3 + fig3_forecast_comparison_nowcast3 +
  fig3_forecast_comparison_1wk3 +
  fig3_forecast_comparison_4wks3 + fig3_crps_underlay_nowcast3 +
  fig3_crps_underlay_1wk3 + fig3_crps_underlay_4wks3 +
  patchwork::plot_layout(
    design = layout,
    guides = "collect",
    axes = "collect"
  ) & theme(
  legend.position = "top",
  legend.justification = "left"
)

fig3

# Fig 4 combined ------------------------------------------------------
tar_load(fig4_rel_crps_by_phase)
tar_load(fig4_rel_crps_overall)
tar_load(fig4_rel_crps_over_time)
tar_load(fig4_rel_crps_by_location)
tar_load(fig4_qq_plot_overall)
tar_load(fig4_plot_coverage_range)

layout <- "
AABB
CCCC
DDDD
EFFF
EFFF
"

fig4 <- fig4_rel_crps_overall + fig4_rel_crps_by_phase +
  fig4_rel_crps_over_time +
  fig4_rel_crps_by_location +
  fig4_plot_coverage_range +
  fig4_qq_plot_overall +
  patchwork::plot_layout(
    design = layout,
    axes = "collect",
    guides = "collect"
  ) & theme(
  legend.position = "top",
  legend.justification = "left"
)
fig4

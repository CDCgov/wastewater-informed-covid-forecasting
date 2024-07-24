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

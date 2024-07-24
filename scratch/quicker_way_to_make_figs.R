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
fig2
ggsave(fig4,
  filename = file.path("output", "eval", "plots", "manuscript", "fig2.png"),
  width = 10, height = 7
)

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
ggsave(fig3,
  filename = file.path("output", "eval", "plots", "manuscript", "fig3.png"),
  width = 10, height = 7
)

# Fig 4 combined ------------------------------------------------------
tar_load(fig4_rel_crps_by_phase)
tar_load(fig4_rel_crps_overall)
tar_load(fig4_ntl_admissions)
tar_load(fig4_rel_crps_over_time)
tar_load(fig4_rel_crps_by_location)
tar_load(fig4_qq_plot_overall)
tar_load(fig4_plot_coverage_range)

layout <- "
AABB
CCCC
DDDD
EEEE
FGGG
FGGG
"

fig4 <- fig4_rel_crps_overall + fig4_rel_crps_by_phase +
  fig4_ntl_admissions +
  fig4_rel_crps_over_time +
  fig4_rel_crps_by_location +
  fig4_plot_coverage_range +
  fig4_qq_plot_overall +
  patchwork::plot_layout(
    design = layout,
    axes = "collect"
  ) & theme(
  legend.position = "top",
  legend.justification = "left"
)
fig4
ggsave(fig4,
  filename = file.path("output", "eval", "plots", "manuscript", "fig4.png"),
  width = 7, height = 10
)

# Fig 5 combined----------------------------------------------
tar_load(fig5_plot_wis_over_time)
tar_load(fig5_overall_performance)
tar_load(fig5_heatmap_relative_wis_all_time)
tar_load(fig5_heatmap_relative_wis_Feb_Mar)
tar_load(fig5_qq_plot_all_time)
tar_load(fig5_qq_plot_feb_mar)
tar_load(fig5_std_rank_feb_mar)
tar_load(fig5_std_rank_all_time)

layout <- "
ABB
CDE
FGH
"
fig5 <- fig5_overall_performance + fig5_plot_wis_over_time +
  fig5_heatmap_relative_wis_Feb_Mar + fig5_qq_plot_feb_mar +
  fig5_std_rank_feb_mar +
  fig5_heatmap_relative_wis_all_time + fig5_qq_plot_all_time +
  fig5_std_rank_all_time +
  patchwork::plot_layout(
    design = layout,
    axes = "collect",
    guides = "collect"
  ) & theme(
  legend.position = "top",
  legend.justification = "left"
)

fig5
ggsave(fig5,
  filename = file.path("output", "eval", "plots", "manuscript", "fig5.png"),
  width = 12, height = 12
)

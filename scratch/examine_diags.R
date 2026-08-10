devtools::load_all('wweval')

loader <- get_object_loader(
  "WY",
  "2023-10-16",
  "status_quo",
  "output/eval_latest/raw_output"
)
fit_obj_name <- "ww_fit_obj"
fit_obj <- loader(fit_obj_name)
stanfit <- fit_obj$fit$result

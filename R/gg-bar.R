gg_bar <- function(data, dic = NULL, vars = NULL, ...) {
  opts <- dsopts::dsopts_merge(...)
  data_opts <- list("agg" = opts$agg)#dsopts_merge(opts, categories = "dataprep")
  data_prep <- gg_data(data, dic, vars = vars, opts = data_opts)
  #d_viz <- data_prep$data
  #hdtype <-  data_prep$hdtype
  data_prep
}

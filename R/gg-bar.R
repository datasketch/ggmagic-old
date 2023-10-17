gg_bar <- function(data, dic = NULL, vars = NULL, ...) {
  opts <- dsopts::dsopts_merge(...)
  bar_opts <-  dsopts_merge(..., categories = "bar")
  data_opts <- list(agg = opts$agg)#dsopts_merge(opts, categories = "dataprep"
  color_opts <- dsopts_merge(..., categories = "colorprep")
  color_opts$bar_graph_type <- bar_opts$bar_graph_type
  data_prep <- gg_data(data, dic, vars = vars, opts = data_opts)
  hdtype <-  data_prep$hdtype
  vars <- data_prep$vars
  data <-  data_prep$data
  data <- gg_data_color(data = data, opts = color_opts, viz = "bar")

  gg <- gg_basic_bar(data = data,
                     x_col = vars$var_cat,
                     y_col = vars$var_num,
                     fill = vars$var_fill,
                     opts = bar_opts) |>
    gg_add_text(viz == "bar", opts = list(datalabel_show = FALSE)) |>
    gg_color(opts = color_opts, data = data, viz = "bar") +
    gg_theme(dsopts_merge(..., categories = "theme"))
  gg
}

gg_bar <- function(data, dic = NULL, vars = NULL, ...) {
 # opts <- dsopts::dsopts_merge(...)
  data_opts <- list("agg" = "sum")#dsopts_merge(opts, categories = "dataprep")
  data_prep <- gg_data(data, dic, vars = vars, opts = data_opts)
  color_opts <- NULL#dsopts_merge(opts, categories = "dataprep")
  hdtype <-  data_prep$hdtype
  vars <- data_prep$vars
  data <-  data_prep$data
  data <- gg_data_color(data = data, opts = color_opts, viz = "bar")

  opts_bar <- list(
    bar_graph_type = "stacked",
    bar_orientation = "hor"
  )

  gg <- gg_basic_bar(data = data,
                     x_col = vars$var_cat,
                     y_col = vars$var_num,
                     fill = vars$var_fill,
                     opts = opts_bar) |>
    gg_add_text(viz == "bar", opts = list(datalabel_show = FALSE)) |>
    gg_color(opts = NULL, data = NULL, viz = "bar")
  gg
}

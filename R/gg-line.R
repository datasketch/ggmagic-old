gg_line <- function(data, dic = NULL, vars = NULL, ...) {
  opts <- dsopts::dsopts_merge(...)
  data_opts <- list(agg = opts$agg)#dsopts_merge(opts, categories = "dataprep")
  data_prep <- gg_data(data, dic, vars = vars, opts = data_opts)
  color_opts <- dsopts_merge(..., categories = "colorprep")
  hdtype <-  data_prep$hdtype
  vars <- data_prep$vars
  data <- gg_data_color(data = data, opts = color_opts, viz = "line")

  opts_line <- NULL

  gg <- gg_basic_lines(data = data,
                         x_col = vars$var_dat,
                         y_col = vars$var_num,
                         fill = vars$var_cat,
                         opts = opts_line) |>
    gg_add_text(viz = "line", opts = list(datalabel_show = FALSE)) |>
    gg_color(opts = color_opts, data = data, viz = "line") +
    gg_theme(dsopts_merge(..., categories = "theme"))
  gg
}

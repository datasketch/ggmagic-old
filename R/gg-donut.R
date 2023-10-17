gg_donut <- function(data, dic = NULL, vars = NULL, ...) {
  opts <- dsopts::dsopts_merge(...)
  data_opts <- list(agg = opts$agg)#dsopts_merge(opts, categories = "dataprep")
  data_prep <- gg_data(data, dic, vars = vars, opts = data_opts)
  color_opts <- dsopts_merge(..., categories = "colorprep")
  hdtype <-  data_prep$hdtype
  vars <- data_prep$vars
  data <-  data_prep$data
  data <- gg_data_color(data = data, opts = color_opts, viz = "donut")
  gg_validator(data_prep$dic, var_cat = vars$var_cat, viz = "donut")
  gg <- gg_basic_donut(data = data,
                       x_col = vars$var_cat,
                       y_col = vars$var_num) |>
    gg_add_text(viz == "donut", opts = list(datalabel_show = FALSE)) |>
    gg_color(opts = color_opts, data = data, viz = "donut") +
    gg_clean_theme(dsopts_merge(..., categories = "theme"))
  gg
}

gg_donut_Cat <- function(data, dic = NULL, ...) {
  vars <- NULL
  if (is.null(dic)) dic <- create_dic(data)
  vars <- dic |> filter(hdtype %in% "Cat") %>% .$id
  vars <- vars[1]
  gg_donut(data = data, dic = dic, vars = vars, ..., agg = "count")
}

gg_donut_CatNum <- function(data, dic = NULL, ...) {
  vars <- NULL
  if (is.null(dic)) dic <- create_dic(data)
  var_cat <- dic |> filter(hdtype %in% "Cat") %>% .$id
  var_num <- dic |> filter(hdtype %in% "Num") %>% .$id
  vars <- c(var_cat[1], var_num[1])
  gg_donut(data = data, dic = dic, vars = vars, ...)
}


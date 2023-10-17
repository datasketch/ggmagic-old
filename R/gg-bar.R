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

gg_bar_Cat <- function(data, dic = NULL, ...) {
  vars <- NULL
  if (is.null(dic)) dic <- create_dic(data)
  vars <- dic |> filter(hdtype %in% "Cat") %>% .$id
  vars <- vars[1]
  gg_bar(data = data, dic = dic, vars = vars, ..., agg = "count")
}

gg_bar_CatNum <- function(data, dic = NULL, ...) {
  vars <- NULL
  if (is.null(dic)) dic <- create_dic(data)
  var_cat <- dic |> filter(hdtype %in% "Cat") %>% .$id
  var_num <- dic |> filter(hdtype %in% "Num") %>% .$id
  vars <- c(var_cat[1], var_num[1])
  gg_bar(data = data, dic = dic, vars = vars, ...)
}

gg_bar_CatCatNum <- function(data, dic = NULL, ...) {
  vars <- NULL
  if (is.null(dic)) dic <- create_dic(data)
  var_cat <- dic |> filter(hdtype %in% "Cat") %>% .$id
  var_num <- dic |> filter(hdtype %in% "Num") %>% .$id
  vars <- c(var_cat[1], var_cat[2], var_num[1])
  gg_bar(data = data, dic = dic, vars = vars, color_by = var_cat[1], ...)
}

gg_bar_CatYeaNum <- function(data, dic = NULL, ...) {
  vars <- NULL
  if (is.null(dic)) dic <- create_dic(data)
  var_cat <- dic |> filter(hdtype %in% "Cat") %>% .$id
  var_yea <- dic |> filter(hdtype %in% "Yea") %>% .$id
  var_num <- dic |> filter(hdtype %in% "Num") %>% .$id
  vars <- c(var_cat[1], var_yea[1], var_num[1])
  gg_bar(data = data, dic = dic, vars = vars, color_by = var_cat[1], ...)
}




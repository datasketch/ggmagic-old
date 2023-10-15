#' @keywords internal
gg_transform_hdtable <- function(data, dic = NULL) {
  if ("hdtable" %in% class(data)) return(data)
  if (is.null(dic)) dic <- hdtable::create_dic(data)
  names(data) <- dic$id
  data <- hdtable::hdtable(d = data, dic = dic)
  data
}

#' @keywords internal
gg_hdtype <- function(dic, vars) {
  if (!is.null(vars)) {
    dic <- dic[dic$id %in% vars,]
  }
  dic <- dic |>
    arrange(factor(id, levels = vars),
            factor(hdtype, levels = c("Cat", "Dat", "Num")),)

  l <- dic |>
    group_by(hdtype) |>
    summarise(id_list = list(id))  %>%
    {setNames(.$id_list, .$hdtype)}
  l
}

#' @keywords internal
gg_extract_hdtype <- function(l) {
  l |>
    purrr::imap_chr(~ paste(rep(.y, length(.x)), collapse = "")) |>
    paste0(collapse = "")
}


gg_data <- function(data, dic = NULL, vars = NULL, opts = NULL, opts_tooltip = NULL) {
  if (is.null(data)) return()
  data <- gg_transform_hdtable(data, dic)
  if (!"hdtable" %in% class(data)) return()
  l_hdtype <- gg_hdtype(data$dic, vars = vars)
  hdtype <- gg_extract_hdtype(l_hdtype)
  opts$group_var <- l_hdtype$Cat
  opts$to_agg <- l_hdtype$Num
  opts$data <- data$data
  data <- do.call("aggregation_data", opts)
  # opts_tooltip$data <- data
  # data$..label <- do.call("prep_tooltip", opts_tooltip)
  list(
  data = data,
  hdtype = hdtype
  )
}

gg_stacked_prep_data <- function(data, x_col ,y_col) {
  if (is.null(data)) return()
  if (nrow(data) == 0) return()
  x_col <- sym(x_col)
  y_col <- sym(y_col)

  data <- data |>
    group_by(!!x_col) |>
    mutate(label_y = cumsum(!!y_col) - 0.5 * !!y_col)

  data
}

gg_pie_prep_data <- function(data, x_col ,y_col) {
  if (is.null(data)) return()
  if (nrow(data) == 0) return()
  x_col <- sym(x_col)
  y_col <- sym(y_col)

  data <- data |>
    arrange(desc(!!x_col)) |>
    mutate(prop = !!y_col / sum(data[[y_col]]) *100) |>
    mutate(ypos = cumsum(prop) - 0.5 * prop)

  data
}

gg_donut_prep_data <- function(data, y_col) {
  if (is.null(data)) return()
  if (nrow(data) == 0) return()
  data$fraction <- data[[y_col]] / sum(data[[y_col]])
  data$ymax = cumsum(data$fraction)
  data$ymin = c(0, head(data$ymax, n=-1))
  data$label_position = (data$ymax + data$ymin) / 2
  data
}








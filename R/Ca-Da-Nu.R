#' Horizontal scatter
#' pointlines
#' @name gg_scatter_hor_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_scatter_hor_CaDaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel = NULL, clab = NULL, angle_x = 0, shape_type = 19, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  clab <- clab %||% nms[1]
  d <- f$d

  d <- d %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  graph <- ggplot(d, aes(x = as.Date(b), y = c, colour = a)) +
           geom_point(shape = shape_type) +
           scale_color_manual(values = getPalette()) +
           theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption, x= xlab, y = ylab) +
           theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)
  graph
}

#' Vertical scatter
#' pointlines
#' @name gg_scatter_ver_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_scatter_ver_CaDaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel = NULL, clab = NULL, angle_x = 0, shape_type = 19, leg_pos = "right", ...){

  graph <- gg_scatter_hor_CaDaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, clab, angle_x, shape_type, leg_pos, ...)
  graph <- graph + coord_flip()

  graph
}

#' Steam
#' Steam
#' @name gg_steam_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_steam_CaDaNu. <- function (data, titleLabel = "", subtitle = "", caption = "",
                              xLabel = NULL, yLabel = NULL, leg_pos = "right", angle_x = 0, ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  data_graph <- data %>%
                tidyr::drop_na(a,b) %>%
                dplyr::group_by(a) %>%
                tidyr::spread(b, c) %>%
                tidyr::gather(b, c, -a)

  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data_graph, aes(x = as.Date(b, origin = data[1,2]), y = c, group = a, fill = a)) +
           stat_steamgraph() +
           theme_ds() +
           theme(axis.text.x = element_text(angle = angle_x,hjust = 1)) +
           scale_fill_manual(values = getPalette()) +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = yLabel) +
           theme(legend.position = leg_pos)
  graph
}



#' Vertical stacked area
#' Stacked Vertical Area
#' @name gg_area_stacked_ver_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_ver_CaDaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                            yLabel = NULL, fillLabel = NULL,
                                            leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  data_graph <- data %>%
                tidyr::drop_na(a,b) %>%
                tidyr::spread(b, c) %>%
                tidyr::gather(b, c, -a)

  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data = data_graph, aes(x = as.Date(b, origin = data[1,2]), y = c, group = a)) +
           geom_area(aes(fill = a), position = "stack")
  graph <- graph +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xLabel, y = ylab, fill = flabel)
  graph <- graph +
           theme_ds() +
           theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
           scale_fill_manual(values = getPalette())
  graph
}

#' Horizontal stacked area
#' Stacked Horizontal Area
#' @name gg_area_stacked_hor_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_hor_CaDaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, fillLabel = NULL,
                                        leg_pos = "right", angle_x = 0,...){

  graph <- gg_area_stacked_ver_CaDaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical 100% stacked area
#' Stacked Vertical Area 100
#' @name gg_area_stacked_100_ver_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_ver_CaDaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, fillLabel = NULL,
                                          leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  data_graph <- data %>%
                tidyr::drop_na(a,b) %>%
                tidyr::spread(b, c) %>%
                tidyr::gather(b, c, -a)

  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data = data_graph, aes(x = as.Date(b, origin = data[1,2]), y = c, group = a)) +
           geom_area(aes(fill = a), position = "fill")
  graph <- graph +
           labs(title = titleLabel, subtitle = subtitle, caption =  caption, x = xlab, y = ylab, fill = flabel)
  graph <- graph +
           theme_ds() +
           theme(legend.position=leg_pos) +
    scale_y_continuous(labels = percent) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
           scale_fill_manual(values = getPalette())
  graph
}

#' Horizontal 100% stacked area
#' Stacked Horizontal Area 100
#' @name gg_area_stacked_100_hor_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_hor_CaDaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                            yLabel = NULL, fillLabel = NULL,
                                            leg_pos = "right", angle_x = 0, ...){

  graph <- gg_area_stacked_100_ver_CaDaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Grouped line + point
#' Grouped Line Color Point
#' @name gg_multi_line_point_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_multi_line_point_CaDaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                        fillLabel = NULL, leg_pos="right", shape_type = 19,
                                        angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  graph <- ggplot(data, aes(x = b, y = c, group = a)) +
             geom_point(aes(color = a), shape = shape_type) +
             geom_line(aes(color = a))
  graph <- graph +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = flabel)
  graph <- graph +
           theme_ds() +
           scale_color_manual(values = getPalette()) +
           scale_x_date() +
    theme(legend.position=leg_pos) +
           theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Grouped line
#' Grouped Line Coloured
#' @name gg_multi_line_CaDaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_multi_line_CaDaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                  fillLabel = NULL, leg_pos="right", shape_type = 19,
                                  angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  graph <- ggplot(data, aes(x = b, y = c, group = a))  + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = flabel)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette()) +
    scale_x_date() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Vertical stacked bar
#' vertical stacked bar graph
#' @name gg_bar_stacked_ver_CaDaNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_ver_CaDaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, leg_pos = "right", angle_x = 0, aggregation = "sum",
                                       hline = NULL, text = TRUE, type = "count", color_text = "black", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c)) %>%
    dplyr::mutate(b = as.Date(b))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(total = sum(c)) %>%
    dplyr::mutate(percent = 100 * round(c / total, 4))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c)) %>%
    tidyr::spread(b, c, fill = 0) %>%
    tidyr::gather(b, c, -a) %>%
    dplyr::mutate(b = as.Date(b)) %>%
    dplyr::left_join(., data_graph, by = c("a", "b", "c")) %>%
    dplyr::mutate(c = ifelse(c == 0, NA, c),
                  percent = ifelse(percent == 0, NA, percent)) %>%
    dplyr::mutate(percent = ifelse(percent == 0, NA, percent),
                  c = ifelse(c == 0, NA, c))

  graph <- ggplot(data, aes(x = reorder(b, c), y = c, fill = a)) + geom_bar(stat = "identity", position = "stack")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) +
    guides(text = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(text == TRUE & type == 'count'){
    return(graph + geom_text(data = data, aes(y = c, label = round(c,2)),
                             check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.9)))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(data = data, aes(y = c, label = paste(percent, "%", sep = "")),
                               check_overlap = TRUE, color = color_text, position = position_stack(vjust = 0.9)))
    }else{
      graph
    }
  }

  if(!is.null(hline)){
    graph <- graph + geom_hline(data = data.frame(valores = hline),
                                aes(yintercept = valores), linetype="dotted")
  }

  graph
}


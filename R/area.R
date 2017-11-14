#' Vertical area
#' Area
#' @name gg_area_YeaNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Yea-Num
#' @examples
#' gg_area_YeaNum(sampleData("Yea-Num"))
gg_area_YeaNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                           yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(x = as.character(a), y = b, group=1)) +
    geom_area(aes(fill = ""), show.legend = FALSE) +
    scale_fill_manual(values = getPalette()) + theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}


#' Vertical area facet
#' Facet Vertical Area
#' @name gg_area_ver_facet_CatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' gg_area_ver_facet_CatNum(sampleData("Cat-Num"))
gg_area_ver_facet_CatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(xorder = 1:n())

  # count <- data_count$count
  # count <- unlist(lapply(count, function(i){
  #   return(1:i)
  # }))
  #
  # data$xorder <- count

  graph <- ggplot(data = data_count, aes(x=xorder, y=b, group=a)) + geom_area(aes(fill = ""), show.legend = FALSE) +
    scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + facet_wrap(~a) + theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}


#' Horizontal area facet
#' Facet Horizontal Area
#' @name gg_area_hor_facet_CatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' gg_area_hor_facet_CatNum(sampleData("Cat-Num"))
gg_area_hor_facet_CatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel = NULL, angle_x = 0, ...){

  graph <- gg_area_ver_facet_CatNum(data, titleLabel, subtitle, caption, xLabel, yLabel, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical 100% stacked area
#' Stacked Vertical Area 100
#' @name gg_area_stacked_100_ver_CatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' gg_area_stacked_100_ver_CatNum(sampleData("Cat-Num"))
gg_area_stacked_100_ver_CatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                           yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% paste("%", nms[2])
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_count <- data %>%
    dplyr::group_by(a) %>%
    dplyr::mutate(xorder = 1:n())

  # count <- data_count$count
  # count <- unlist(lapply(count, function(i){
  #   return(1:i)
  # }))
  #
  # data$xorder <- count

  data_graph <- data_count %>% arrange(xorder) %>%
    tidyr::spread(xorder, b) %>% tidyr::gather(xorder, b, -a)
  data_graph[is.na(data_graph)] <- 0
  data_graph$xorder <- as.numeric(data_graph$xorder)

  graph <- ggplot(data = data_graph,
                  aes(x=xorder, y=b, group=a)) +
    geom_area(aes(fill = a), position = "fill")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = percent) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph
}

#' Horizontal 100% stacked area
#' Stacked Horizontal Area 100
#' @name gg_area_stacked_100_hor_CatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' gg_area_stacked_100_hor_CatNum(sampleData("Cat-Num"))
gg_area_stacked_100_hor_CatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                            yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  graph <- gg_area_stacked_100_ver_CatNum(data, titleLabel,subtitle, caption, xLabel, yLabel, fillLabel = NULL, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical stacked area
#' Stacked Vertical Area
#' @name gg_area_stacked_ver_CatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' gg_area_stacked_ver_CatNum(sampleData("Cat-Num"))
gg_area_stacked_ver_CatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                        fillLabel = NULL, leg_pos = "right", text = TRUE, color_text = "black",
                                        type = "count", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_count <- data %>%
    dplyr::group_by(a)  %>%
    dplyr::mutate(xorder = 1:n())

  # count <- data_count$count
  # count <- unlist(lapply(count, function(i){
  #   return(1:i)
  # }))
  #
  # data$xorder <- count

  data_graph <- data_count %>% arrange(xorder) %>%
    tidyr::spread(xorder, b) %>% tidyr::gather(xorder, b, -a)
  data_graph[is.na(data_graph)] <- 0
  data_graph$xorder <- as.numeric(data_graph$xorder)

  graph <- ggplot(data = data_graph,
                  aes(x=xorder, y=b, group=a)) +
    geom_area(aes(fill = a), position = "stack")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) + theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph
}

#' Horizontal stacked area
#' Stacked Horizontal Area
#' @name gg_area_stacked_hor_CatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' gg_area_stacked_hor_CatNum(sampleData("Cat-Num"))
gg_area_stacked_hor_CatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, fillLabel = NULL, leg_pos = "right", text = TRUE, color_text = "black", type = "count", angle_x = 0, ...){

  graph <- gg_area_stacked_ver_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos, text, color_text, type, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Filled density distribution
#' Filled Density Distribution
#' @name gg_area_multi_density_dist_CatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' gg_area_multi_density_dist_CatNum(sampleData("Cat-Num"))
gg_area_multi_density_dist_CatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, fillLabel = NULL, leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_density(aes(fill = a)) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    theme_ds() +
    scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) + theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Area
#' Area
#' @name gg_area_DatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Dat-Num
#' @examples
#' gg_area_DatNum(sampleData("Dat-Num"))
gg_area_DatNum <- function(data, titleLabel = "", subtitle = "", caption = "",
                            xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>%
    dplyr::filter(!is.na(a), !is.na(b))

  graph <- ggplot(data, aes(x = a, y = b, group=1)) +
    geom_area(aes(fill = ""), show.legend = FALSE) +
    scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = comma) +
    theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Stepped area
#' Stepped stacked area
#' @name gg_area_stepped_DatNum
#' @param x A data.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Dat-Num
#' @examples
#' gg_area_stepped_DatNum(sampleData("Dat-Num"))
gg_area_stepped_DatNum <- function(data, titleLabel = "", subtitle = "", caption = "",
                                   xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>%
    dplyr::filter(!is.na(a), !is.na(b))

  graph <-  ggplot(data) +
    geom_step(aes(x = a, y = b, color = ""), show.legend = FALSE) +
    theme_ds() + scale_color_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph
}

#' Vertical stacked area
#' Stacked Vertical Area
#' @name gg_area_stacked_ver_CatDatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat-Num
#' @examples
#' gg_area_stacked_ver_CatDatNum(sampleData("Cat-Dat-Num"))
gg_area_stacked_ver_CatDatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, fillLabel = NULL, aggregation = "sum",
                                          leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, c)) %>%
    tidyr::spread(b, count) %>%
    tidyr::gather(b, count, -a)

  data_graph$b <- lubridate::as_date(data_graph$b)
  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data_graph, aes( b, count))
  graph <- graph + geom_area(aes(fill= a), position = 'stack')
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph +
    theme_ds() +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_fill_manual(values = getPalette())
  graph
}

#' Horizontal stacked area
#' Stacked Horizontal Area
#' @name gg_area_stacked_hor_CatDatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat-Num
#' @examples
#' gg_area_stacked_hor_CatDatNum(sampleData("Cat-Dat-Num"))
gg_area_stacked_hor_CatDatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, fillLabel = NULL, aggregation = "sum",
                                          leg_pos = "right", angle_x = 0,...){

  graph <- gg_area_stacked_ver_CatDatNum(data, titleLabel, subtitle, caption, xLabel, yLabel,
                                         fillLabel, aggregation, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical 100% stacked area
#' Stacked Vertical Area 100
#' @name gg_area_stacked_100_ver_CatDatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat-Num
#' @examples
#' gg_area_stacked_100_ver_CatDatNum(sampleData("Cat-Dat-Num"))
gg_area_stacked_100_ver_CatDatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel = NULL, fillLabel = NULL, aggregation = "sum",
                                              leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% paste("%", nms[3])
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  data_graph <- data %>% dplyr::group_by(a, b) %>%
    dplyr::summarise(count = agg(aggregation, c)) %>%
    dplyr::group_by(b) %>%
    dplyr::mutate(percent = 100 * round(count / sum(count), 4))
  # data_graph <- data %>%
  #   dplyr::group_by(a, b) %>%
  #  # dplyr::mutate(percent = 100 * round(c / sum(c), 4)
  #   tidyr::spread(b, c) %>%
  #   tidyr::gather(b, c, -a) %>%
  #   dplyr::summarise(percent = 100 * round(c / sum(c), 4))

  # data_graph <- data %>%
  #   tidyr::drop_na(a,b) %>%
  #   tidyr::spread(b, c) %>%
  #   tidyr::gather(b, c, -a)
  data_graph$b <- lubridate::as_date(data_graph$b)
  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data_graph, aes(b, percent))
  graph <- graph + geom_area(aes(fill= a), position = 'stack')
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph +
    theme_ds() +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_fill_manual(values = getPalette())
  graph

  #
  #
  # data_graph$b <- lubridate::as_date(data_graph$b)
  # data_graph[is.na(data_graph)] <- 0
  #
  # graph <- ggplot(data = data_graph, aes(x = as.Date(b, origin = data[1,2]), y = c, group = a)) +
  #   geom_area(aes(fill = a), position = "fill")
  # graph <- graph +
  #   labs(title = titleLabel, subtitle = subtitle, caption =  caption, x = xlab, y = ylab, fill = clab)
  # graph <- graph +
  #   theme_ds() +
  #   theme(legend.position=leg_pos) +
  #   scale_y_continuous(labels = percent) +
  #   theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
  #   scale_fill_manual(values = getPalette())
  # graph
}

#' Horizontal 100% stacked area
#' Stacked Horizontal Area 100
#' @name gg_area_stacked_100_hor_CatDatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_hor_CatDatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, fillLabel = NULL,
                                               leg_pos = "right", angle_x = 0, ...){

  graph <- gg_area_stacked_100_ver_CatDatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Horizontal stacked area
#' Stacked horizontal Area
#' @name gg_area_stacked_hor_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_area_stacked_hor_CatCat(sampleData("Cat-Cat"))
gg_area_stacked_hor_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% nms[2]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count=n()) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)
  data_graph[is.na(data_graph)] <- 0
  graph <- ggplot(data = data_graph,
                  aes(x=a, y=count, group=b)) + geom_area(aes(fill = b), position = "stack")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph + guides(text = FALSE)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))


  graph
}

#' Vertical stacked area
#' Stacked vertical Area
#' @name gg_area_stacked_ver_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_area_stacked_ver_CatCat(sampleData("Cat-Cat"))
gg_area_stacked_ver_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  graph <- gg_area_stacked_hor_CatCat.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical 100% stacked area
#' Stacked vertical Area 100pct
#' @name gg_area_stacked_100_ver_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_area_stacked_100_ver_CatCat(sampleData("Cat-Cat"))
gg_area_stacked_100_ver_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                            yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 1, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  clab <- fillLabel %||% nms[2]
  ylab <- yLabel %||% "Porcentaje"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>% dplyr::group_by(a, b) %>% dplyr::summarise(count = n()) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)
  data_graph[is.na(data_graph)] <- 0
  graph <- ggplot(data = data_graph,
                  aes(x=a, y=count, group=b)) + geom_area(aes(fill = b), position = "fill")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = percent) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal 100% stacked area
#' Stacked horizontal Area 100pct
#' @name gg_area_stacked_100_hor_CatCat
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' gg_area_stacked_100_hor_CatCat(sampleData("Cat-Cat"))
gg_area_stacked_100_hor_CatCat <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                            yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  graph <- gg_area_stacked_100_ver_CatCat.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical stacked area
#' Stacked vertical Area
#' @name gg_area_stacked_ver_CatCatNum
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' gg_area_stacked_ver_CatCatNum(sampleData("Cat-Cat-Num"))
gg_area_stacked_ver_CatCatNum <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, leg_pos = "right", aggregation = "sum", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count=agg(aggregation, c)) %>%
    tidyr::spread(b, count) %>% tidyr::gather(b, count, -a)

  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data = data_graph, aes(x=a, y=count, group=b)) +
    geom_area(aes(fill = b), position = "stack")

  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos) +
    scale_fill_manual(values = getPalette())

  graph
}

#' Horizontal stacked area
#' Stacked area
#' @name gg_area_stacked_hor_CatCatNum.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_hor_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                           yLabel = NULL, leg_pos = "right", aggregation = "sum", angle_x = 0,...){


  graph <- gg_area_stacked_ver_CatCatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, aggregation, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical 100% stacked area
#' Stacked vertical Area 100
#' @name gg_area_stacked_100_ver_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_ver_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, fillLabel = NULL, leg_pos = "right", aggregation = "sum", angle_x = 0,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% paste("%", aggregation, nms[3], sep = " ")
  clab <- fillLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count=agg(aggregation,c)) %>%
    tidyr::spread(b, count) %>%
    tidyr::gather(b, count, -a)

  data_graph[is.na(data_graph)] <- 0

  graph <- ggplot(data = data_graph, aes(x=a, y=count, group=b)) +
    geom_area(aes(fill = b), position = "fill")
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph <- graph +
    theme_ds() + theme(legend.position=leg_pos) +
    scale_fill_manual(values = getPalette()) +
    scale_y_continuous(labels = percent) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal 100% stacked area
#' Stacked area 100
#' @name gg_area_stacked_100_hor_CatCatNum.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stacked_100_hor_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, fillLabel = NULL, leg_pos = "right", aggregation = "sum", angle_x = 0, ...){


  graph <- gg_area_stacked_100_ver_CatCatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, leg_pos, aggregation, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

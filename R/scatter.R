#' Scatter facet
#' Facet Point
#' @name gg_point_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel = NULL, shape_type = 19, angle_x = 0, ...){

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

  graph <- ggplot(data_count, aes(x=xorder, y=b)) + geom_point(shape = shape_type, aes(color = ""), show.legend = FALSE) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + facet_wrap(~a) + theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}


#' Grouped scatter
#' Grouped Color Point
#' @name gg_point_grouped_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_grouped_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel = NULL, fillLabel = NULL, leg_pos="right", shape_type = 19, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
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

  graph <- ggplot(data_count, aes(x=xorder, y=b)) + geom_point(aes(color = a), shape = shape_type) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab)
  graph <- graph + theme_ds() + theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}


#' Vertical scatter
#' Scatter plot
#' @name gg_point_Num.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                          yLabel = NULL, shape_type = 19, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[1]
  xlab <- xLabel %||% "Index"
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a))

  data_graph <- data %>%
    dplyr::mutate(order = 1:nrow(data))

  graph <- ggplot(data_graph, aes(x=order, y=a)) + geom_point(shape = shape_type, aes(color = ""), show.legend = FALSE)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal scatter
#' Line plot
#' @name gg_point_flip_Num.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_flip_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = NULL, shape_type = 19, angle_x = 0, ...){

  graph <- gg_point_Num.(data, titleLabel, subtitle, caption, xLabel, yLabel, shape_type, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}


#' Scatter
#' Scatter plot
#' @name gg_point_NumNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_NumNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel = NULL, shape_type = 19, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a), !is.na(b))

  graph <- ggplot(data = data, aes(x = a, y = b)) +
    geom_point(shape = shape_type, aes(color = ''), show.legend = FALSE) + theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, y = ylab, x = xlab) +
    scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  return(graph)

}

#' Scatter
#' Point scatter plot
#' @name gg_point_DatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_DatNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
                             xLabel = NULL, yLabel = NULL, angle_x = 0, shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>%
    dplyr::filter(!is.na(a), !is.na(b))

  graph <- ggplot(data, aes(x = a, y = b)) +
    geom_point(aes(color = ""), show.legend = FALSE, shape = shape_type) +
    scale_y_continuous(labels = comma) + scale_color_manual(values = getPalette()) +
    theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))


  graph
}

#' Scatter
#' ciculos
#' @name gg_circle_CatYeaNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Yea-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_circle_CatYeaNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                 xLabel = NULL, yLabel = NULL, leg_pos = "right", shape_type = 19, angle_x = 0,  ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[1]
  data <- f$d


  graph <- ggplot(data, aes(x = b, y = a)) +
    geom_point(aes(size = c, colour = ""), show.legend = FALSE, shape = shape_type) +
    theme_ds() + scale_color_manual(values = getPalette()) +
    theme(legend.position = leg_pos) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Scatter + Aggregation on both numeric variables
#' pointlines
#' @name gg_scatter_agg_CatNumNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_scatter_agg_CatNumNum. <- function(data,titleLabel = "", subtitle = "", caption = "",
                                      xLabel = NULL, yLabel = NULL, fillLabel = NULL, angle_x = 0,
                                      aggregation = "sum", shape_type = 19, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% paste(aggregation, nms[2], sep = " ")
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  data <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b=agg(aggregation,b),c=agg(aggregation,c))

  graph <- ggplot(data, aes(x = b, y = c, colour = a, label = a)) +
    geom_point(shape = shape_type)
  graph <- graph + scale_color_manual(values = getPalette()) +
    theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption,
                      x= xlab, y = ylab, colour = clab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position = leg_pos)
  graph
}


#' Scatter + Aggregation on both numeric variables + trend
#' pointlines
#' @name gg_scatter_agg_trend_CatNumNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_scatter_agg_trend_CatNumNum. <- function(data,titleLabel = "", subtitle = "", caption = "",
                                            xLabel = NULL, yLabel=NULL, fillLabel = NULL, angle_x = 0,
                                            aggregation = "sum", se = FALSE, shape_type = 19, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% paste(aggregation, nms[2], sep = " ")
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  data <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b=agg(aggregation,b),c=agg(aggregation,c))

  formula <- y ~ poly(x, 1, raw = TRUE)

  graph <- ggplot(data, aes(x = b, y = c)) +
    geom_point(aes(color = a), shape = shape_type) #+ geom_text(aes(label = a, color = a), vjust = 1.2)
  graph <- graph + geom_smooth(method = "lm",
                               formula = formula, color = getPalette()[1],
                               se = se) +
    stat_poly_eq(aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                 vjust = 1,
                 formula = formula, parse = TRUE)

  graph <- graph +
    scale_color_manual(values = getPalette()) + #guides(color = FALSE) +
    theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption,
                      x= xlab, y = ylab, colour = clab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position = leg_pos)
  graph
}

#' Scatter
#' Point
#' @name gg_scatter_CatNumNum.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_scatter_CatNumNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                  yLabel = NULL, fillLabel = NULL, leg_pos="right", shape_type = 19, angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  graph <- ggplot(data, aes(x = b , y = c, fill = a, color  = a))  +
    geom_point(shape = shape_type) +
    scale_color_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab, color = clab) +
    theme(legend.position = leg_pos)

  return(graph)
}

#' Scatter + Trend
#' Point
#' @name gg_scatter_trend_CatNumNum.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_scatter_trend_CatNumNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, fillLabel = NULL, leg_pos="right", shape_type = 19, angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  graph <- ggplot(data, aes(x = b , y = c, fill = a, color  = a))  +
    geom_point(shape = shape_type)

  formula <- y ~ poly(x, 1, raw = TRUE)

  graph <- graph + geom_smooth(method = "lm",
                               formula = formula,
                               se = FALSE, aes(color = a))

  graph <- graph +
    scale_color_manual(values = getPalette()) +
    theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption,
                      x = xlab, y = ylab, fill = clab, color = clab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position = leg_pos)

  return(graph)
}

#' Horizontal scatter
#' pointlines
#' @name gg_scatter_hor_CatDatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_scatter_hor_CatDatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, fillLabel = NULL, angle_x = 0, shape_type = 19, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  clab <- fillLabel %||% nms[1]
  d <- f$d

  d <- d %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  graph <- ggplot(d, aes(x = as.Date(b), y = c, colour = a)) +
    geom_point(shape = shape_type) +
    scale_color_manual(values = getPalette()) +
    theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption, x= xlab, y = ylab, color = clab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)
  graph
}

#' Vertical scatter
#' pointlines
#' @name gg_scatter_ver_CatDatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_scatter_ver_CatDatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, fillLabel = NULL, angle_x = 0, shape_type = 19, leg_pos = "right", ...){

  graph <- gg_scatter_hor_CatDatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel, angle_x, shape_type, leg_pos, ...)
  graph <- graph + coord_flip()

  graph
}

#' Scatter
#' Coloured Point
#' @name gg_point_CatCatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_CatCatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                fillLabel = NULL, angle_x = 0, shape_type = 19, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  clab <- fillLabel %||% nms[3]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b),
                                 c = ifelse(is.na(c), "NA", c))

  graph <- ggplot(data, aes(x = factor(a), y = factor(b), color = c)) +
    geom_point(shape = shape_type) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab) + theme_ds() +
    scale_x_discrete(labels = scales::comma) +
    scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Points facet by years
#' Points facet by years
#' @name gg_points_facet_DatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_points_facet_DatNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                    xLabel = NULL, yLabel = NULL, angle_x = 0, shape_type = 19, ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>%
    dplyr::filter(!is.na(a), !is.na(b))

  data$Year <- format(data$a, "%Y")
  data$Month <- format(data$a, "%b")
  data$Daty <- format(data$a, "%d")

  data$MonthDaty <- format(data$a, "%d-%b")

  #data$CommonDatte <- as.Datte(paste0("2000-",format(data$a, "%j")), "%Y-%j")

  graph <- ggplot(data = data, mapping = aes(x = a, y = b, colour = Year)) +
    geom_point(shape = shape_type, show.legend = FALSE) + scale_color_manual(values = getPalette()) +
    facet_wrap(~Year, scales = "free") +
    theme_ds() +
    #scale_x_date(labels = function(x) format(x, "%d-%b")) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

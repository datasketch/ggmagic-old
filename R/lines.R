#' Horizontal line
#' Horizontal Lines
#' @name gg_line_hor_YeaNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Yea-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_hor_YeaNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                xLabel = NULL, yLabel = NULL, angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>%
    dplyr::filter(!is.na(a),!is.na(b))

  ggplot(data, aes(x= a,y=b,group=1)) +
    geom_line(stat = "identity", aes(colour = ""), show.legend = FALSE) +
    theme_ds() +
    scale_color_manual(values = getPalette()) +
    scale_y_continuous(labels = comma) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
}


#' Line + point facet
#' Facet Line Point
#' @name gg_line_point_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
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

  graph <- ggplot(data_count, aes(x = xorder, y = b)) + geom_point(shape = shape_type, aes(color = ""), show.legend = FALSE) +
    geom_line(aes(color = ""), show.legend = FALSE) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + facet_wrap(~a) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Line facet
#' Facet Line
#' @name gg_line_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
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

  graph <- ggplot(data_count, aes(x=xorder, y=b)) + geom_line(aes(color = ""), show.legend = FALSE) +
    scale_color_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + facet_wrap(~a) + theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Grouped line + point
#' Grouped Line Color Point
#' @name gg_line_point_multi_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_multi_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                        fillLabel = NULL, leg_pos="right", shape_type = 19, angle_x = 0, ...){

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

  # data$xorder <- count

  graph <- ggplot(data_count, aes(x=xorder, y=b)) + geom_point(aes(color = a), shape = shape_type) + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Grouped line
#' Grouped Line Coloured
#' @name gg_line_multi_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_multi_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                  fillLabel = NULL, leg_pos="right", angle_x = 0, ...){

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

  graph <- ggplot(data_count, aes(x=xorder, y=b)) + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Trend line facet
#' Facet Trend Line
#' @name gg_point_trend_line_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_trend_line_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                              yLabel = NULL, shape_type = 19, alpha = 0.3, se = FALSE, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
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

  graph <- ggplot(data_count, aes(x = xorder, y = b)) + geom_point(shape = shape_type, aes(color = ""), show.legend = FALSE) +
    geom_smooth(method=lm, se=se, aes(colour = "*", fill = "*"), alpha = alpha, show.legend = FALSE) + facet_wrap(~a) +
    scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Slope
#' Slope
#' @name gg_slope_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_slope_CatNum. <-  function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                              leg_pos="right", overlap = TRUE, text_size = 6,
                              size_point = 3, size_line = 1,...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>% group_by(a) %>% dplyr::mutate(xorder = 1:n())

  graph <- ggplot(data_graph) +
    geom_line(aes(x = as.factor(xorder), y = b, group = a, color = a), size = size_line) +
    geom_point(aes(x = as.factor(xorder), y = b, group = a, color = a), size = size_point)+
    theme_ds_clean() +  labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    geom_text(aes(x = as.factor(xorder), y = min(b) - mean(b), label = xorder),
              size = text_size, show.legend = FALSE, check_overlap = TRUE) +
    annotate("text", x = filter(data_graph,xorder == 1)$xorder-.15, y = filter(data_graph,xorder == 1)$b,
             label = filter(data_graph,xorder == 1)$b, check_overlap = overlap) +
    annotate("text", x = filter(data_graph,xorder == 2)$xorder+.15, y = filter(data_graph,xorder == 2)$b,
             label = filter(data_graph,xorder == 2)$b, check_overlap = overlap)+
    scale_color_manual(values = getPalette()) + theme(legend.position = leg_pos)

  return(graph)

}

#' Vertical line + point
#' Line wiht point plot
#' @name gg_line_point_Num.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = NULL, shape_type = 19, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[1]
  xlab <- xLabel %||% "Index"
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a))

  data_graph <- data %>%
    dplyr::mutate(order = 1:nrow(data))

  graph <- ggplot(data_graph, aes(x=order, y=a)) + geom_line(aes(color = ""), show.legend = FALSE) +
    geom_point(aes(color = ""), shape = shape_type, show.legend = FALSE)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}


#' Horizontal line + point
#' Line with point plot
#' @name gg_line_point_flip_Num.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_flip_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = NULL, shape_type = 19, angle_x = 0, ...){

  graph <- gg_line_point_Num.(data, titleLabel, subtitle, caption, xLabel, yLabel, shape_type, angle_x = 0, ...)
  graph <- graph + coord_flip()

  graph
}
#' Line for each numeric variable
#' Line for each Num
#' @name gg_line_multi_NumNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_multi_NumNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                  xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a), !is.na(b))

  data_graph <- data %>% dplyr::mutate(xorder = 1:nrow(.)) %>%
    tidyr::gather(type, value, -xorder)

  graph <- ggplot(data_graph, aes(x = xorder, y = value, group = type, colour = type)) +
    geom_line() + theme_ds() +
    scale_color_manual(values = getPalette(),
                       breaks = unique(data_graph$type),
                       labels = nms)
  graph <- graph  + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  return(graph)
}

#' Line + point
#' Line point plot
#' @name gg_line_point_NumNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_NumNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                  yLabel = NULL, shape_type = 19, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a), !is.na(b))

  graph <- ggplot(data = data, aes(x = a, y = b)) +
    geom_point(shape = shape_type, aes(color = ''), show.legend = FALSE) +
    geom_line(aes(color = ''), show.legend = FALSE) + theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, y = ylab, x = xlab) +
    scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  return(graph)

}

#' Line
#' Lines
#' @name gg_line_DatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_DatNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
                            xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>%
    dplyr::filter(!is.na(a), !is.na(b))

  graph <- ggplot(data, aes(x = a, y = b, group=1)) +
    geom_line(stat = "identity", aes(color = ""), show.legend = FALSE) +
    scale_color_manual(values =  getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph
}

#' Line + point
#' Lines
#' @name gg_line_points_DatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_points_DatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel = NULL, shape_type = 19, hline = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>%
    dplyr::filter(!is.na(a), !is.na(b))

  graph <- ggplot(data, aes(x = a, y = b, group=1)) +
    geom_line(stat = "identity", aes(color = ""), show.legend = FALSE) +
    geom_point(aes(color = ""), shape = shape_type, show.legend = FALSE) +
    scale_color_manual(values =  getPalette()) +
    theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(!is.null(hline)){
    graph <- graph + geom_hline(data = data.frame(valores = hline),
                                aes(yintercept = valores), linetype="dotted")
  }

  graph
}

#' Line + point facet by years
#' Line Points facet by years
#' @name gg_line_points_facet_DatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_points_facet_DatNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
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

  graph <- ggplot(data = data, mapping = aes(x = a, y = b, shape = Year, colour = Year)) +
    geom_line(show.legend = FALSE) + geom_point(shape = shape_type, show.legend = FALSE) + scale_color_manual(values = getPalette()) + theme_ds() +
    facet_wrap(~Year, scales = "free") +
    theme_ds() +
    #scale_x_date(labels = function(x) format(x, "%d-%b")) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal line
#' Horizontal Line
#' @name gg_line_hor_Cat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_hor_Cat. <- function(data, titleLabel = '', xLabel = NULL, subtitle = "",
                             caption = "", yLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(order = c(1:nrow(.)))

  graph <- ggplot(data = data_graph, aes(x = a, y = count, group=1, colour = "")) + geom_line()
  graph <- graph + labs(title = titleLabel, x = xlab, y = ylab, subtitle = subtitle, caption = caption)

  graph <- graph + theme_minimal() + theme_ds() + theme(legend.position=leg_pos) +
    scale_color_manual(values = getPalette()) + guides(colour = FALSE) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Vertical line
#' Vertical Line
#' @name gg_line_ver_Cat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_ver_Cat. <- function(data, titleLabel = '', xLabel = NULL, subtitle = "",
                             caption = "", yLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  graph <- gg_line_hor_Cat.(data, titleLabel, xLabel, subtitle, caption, yLabel, leg_pos, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Horizontal line + point
#' Horizontal Line Point
#' @name gg_line_point_hor_Cat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_hor_Cat. <- function(data, titleLabel = '', xLabel = NULL, subtitle = "",
                                   caption = "", yLabel = NULL, leg_pos = "right",
                                   shape_type = 19, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(order = c(1:nrow(.)))

  graph <- ggplot(data = data_graph, aes(x = a, y = count, group=1, colour = "")) + geom_line() + geom_point(type = shape_type)
  graph <- graph + labs(title = titleLabel, x = xlab, y = ylab, subtitle = subtitle, caption = caption)

  graph <- graph + theme_minimal() + theme_ds() + theme(legend.position=leg_pos) +
    scale_color_manual(values = getPalette()) + guides(colour =FALSE) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Vertical line + point
#' Vertical Line Point
#' @name gg_line_point_ver_Cat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_ver_Cat. <- function(data, titleLabel = '', xLabel = NULL, subtitle = "",
                                   caption = "", yLabel = NULL, leg_pos = "right",
                                   shape_type = 19, angle_x = 0, ...){

  graph <- gg_line_point_hor_Cat.(data, titleLabel, xLabel, subtitle, caption, yLabel, leg_pos, shape_type, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Horizontal line
#' Tiene múltiples líneas
#' @name gg_line_hor_CatYeaNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Yea-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_hor_CatYeaNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, nbreaks = NULL,
                                   shape_type = 19,
                                   aggregation = "sum", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% paste(aggregation, nms[3])
  data <- f$d

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c=agg(aggregation,c))

  xValues <- as.numeric(data$b)
  xValues <- xValues[!is.na(xValues)]
  defaultNBreaks <- ifelse(length(unique(xValues)) <= 7, length(unique(xValues)), 5)
  nbreaks <- nbreaks %||% defaultNBreaks

  customBreaks <- round(seq(min(xValues),max(xValues), length.out = nbreaks))

  graph <- ggplot(data, aes(x = b ,y=c,group=a,colour=a)) +
    geom_line(stat = "identity")
  graph <- graph + geom_point(shape = shape_type)
  graph <- graph + theme_ds() +
    scale_shape(solid = TRUE) +
    scale_y_continuous(labels = comma) +
    scale_x_discrete(breaks = customBreaks) +
    scale_color_manual(values = getPalette())  +
    theme(legend.position = leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab)
  graph
}

#' Slope
#' Slope
#' @name gg_slope_CatYeaNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Yea-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_slope_CatYeaNum. <-  function(data, titleLabel = "",  subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                 leg_pos="right", text_size = 6, size_vjust = 1.5, overlap = TRUE,
                                 size_hjust = 0.5, size_point = 3, size_line = 1,...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data <- data %>% group_by(a) %>% dplyr::mutate(xorder = 1:n())

  graph <- ggplot(data) +
    geom_text(aes(x = as.factor(b), y = min(c) - mean(c), label = b),
              size = text_size, show.legend = FALSE, check_overlap = TRUE) +
    geom_line(aes(x = as.factor(b), y = c, group = a, color = a), size = size_line) +
    geom_point(aes(x = as.factor(b), y = c, group = a, color = a), size = size_point) +
    theme_ds() + theme_ds_clean() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    scale_color_manual(values = getPalette()) + theme(legend.position = leg_pos) +
    annotate("text", x = filter(data,xorder == 1)$xorder-.1, y = filter(data,xorder == 1)$c,
             label = filter(data,xorder == 1)$c, check_overlap = overlap) +
    annotate("text", x = filter(data,xorder == 2)$xorder+.1, y = filter(data,xorder == 2)$c,
             label = filter(data,xorder == 2)$c, check_overlap = overlap)


  return(graph)

}

#' Line
#' lines
#' @name gg_line_CatNumNum.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_CatNumNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = NULL, fillLabel = NULL, leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  graph <- ggplot(data) +  geom_line(aes(x = b, y = c, group = a, colour = a))  + theme_ds() +
    scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab, fill = clab) +
    theme(legend.position = leg_pos)

  return(graph)

}

#' Line + point
#' Point Lines
#' @name gg_point_line_CatNumNum.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_line_CatNumNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                     yLabel = NULL, fillLabel = NULL, leg_pos = "right", shape_type = 19, angle_x = 0,  ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  graph <- ggplot(data) +  geom_line(aes(x = b, y = c, group = a, colour = a))  +
    geom_point(aes(x = b, y = c, group = a, colour = a), shape = shape_type) + theme_ds() +
    scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab, fill = clab) +
    theme(legend.position = leg_pos)

  return(graph)
}

#' Horizontal line + point
#' pointlines
#' @name gg_pointline_hor_CatDat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pointline_hor_CatDat. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                     xLabel = NULL, yLabel = NULL, fillLabel = NULL, angle_x = 0, shape_type = 19, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[1]
  clab <- fillLabel %||% nms[1]
  d <- f$d

  d <- d %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(d, aes(x = b, y = a, colour = a)) +
    geom_point(shape = shape_type) +
    theme_ds() + scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_x_date() + #guides(color = FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab)

  return(graph)
}

#' Vertical line + point
#' pointlines
#' @name gg_pointline_ver_CatDat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pointline_ver_CatDat. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                     xLabel = NULL, yLabel = NULL, fillLabel = NULL, angle_x = 0, shape_type = 19, ...){

  graph <- gg_pointline_hor_CatDat.(data, titleLabel, subtitle, caption, xLabel, yLabel, fillLabel,
                                    angle_x, shape_type, ...)

  graph <- graph + coord_flip()

  return(graph)
}

#' Grouped line + point
#' Grouped Line Color Point
#' @name gg_multi_line_point_CatDatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_multi_line_point_CatDatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                           fillLabel = NULL, leg_pos="right", shape_type = 19,
                                           angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  graph <- ggplot(data, aes(x = b, y = c, group = a)) +
    geom_point(aes(color = a), shape = shape_type) +
    geom_line(aes(color = a))
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab)
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
#' @name gg_multi_line_CatDatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_multi_line_CatDatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                     fillLabel = NULL, leg_pos="right", shape_type = 19,
                                     angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c))

  graph <- ggplot(data, aes(x = b, y = c, group = a))  + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette()) +
    scale_x_date() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal line facet
#' Horizontal Line
#' @name gg_line_hor_facet_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_hor_facet_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data = data_graph, aes(x = a, y = count, group=b, colour = "")) + geom_line() +
    facet_wrap(~b)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + guides(color = FALSE)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))


  graph
}

#' Vertical line facet
#' Vertical Line
#' @name gg_line_ver_facet_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_ver_facet_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, angle_x = 0, ...){

  graph <- gg_line_hor_facet_CatCat.(data, titleLabel, subtitle, caption, xLabel, yLabel, angle_x = 0, ...)
  graph <- graph + coord_flip()

  graph
}

#' Horizontal line + point facet
#' Horizontal Line Point
#' @name gg_line_point_hor_facet_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_hor_facet_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                            yLabel = NULL, angle_x = 0, shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b))

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data = data_graph, aes(x = a, y = count, group=b, colour = "")) + geom_line() +
    geom_point(shape = shape_type) + facet_wrap(~b) + theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    guides(color = FALSE)
  graph <- graph +
    scale_color_manual(values = getPalette())

  graph
}

#' Vertical line + point facet
#' Vertical Line Point
#' @name gg_line_point_ver_facet_CatCat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_ver_facet_CatCat. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                            yLabel = NULL, angle_x = 0, shape_point = 19, ...){

  graph <- gg_line_point_hor_facet_CatCat.(data, titleLabel, subtitle, caption, xLabel, yLabel, angle_x, shape_point, ...)
  graph <- graph + coord_flip()

  graph
}

#' Horizontal line facet
#' horizontal linegraph
#' @name gg_line_hor_facet_CatCatNum.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_hor_facet_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL, aggregation = "sum", angle_x = 0,
                                         shape_type = 19, ...){
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
    dplyr::summarise(sum = agg(aggregation, c)) %>%
    dplyr::arrange(desc(sum))

  graph <- ggplot(data = data_graph, aes(x = a, y = sum, group=b, colour = "")) +
    geom_line(show.legend = FALSE) +
    geom_point(shape = shape_type, show.legend = FALSE) + scale_color_manual(values = getPalette()) +
    facet_wrap(~b)
  graph <- graph +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Vertical line facet
#' vertical linegraph
#' @name gg_line_ver_facet_CatCatNum.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_ver_facet_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                         yLabel = NULL, aggregation = "sum", angle_x = 0, ...){

  graph <- gg_line_hor_facet_CatCatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, aggregation, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Grouped line + points by first variable
#' Grouped Line Color Point
#' @name gg_multi_line_point_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_multi_line_point_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                           fillLabel = NULL, leg_pos ="right", aggregation = "sum", shape_type = 19, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c))

  graph <- ggplot(data, aes(x = b, y = c, group = a)) + geom_point(aes(color = a), shape = shape_type) + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Grouped line by first variable
#' Grouped Line Coloured
#' @name gg_multi_line_CatCatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_multi_line_CatCatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                     fillLabel = NULL, aggregation = "sum", leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% paste(aggregation, nms[3], sep = " ")
  clab <- fillLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a),
                                 b = ifelse(is.na(b), "NA", b)) %>%
    dplyr::filter(!is.na(c))

  data <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(c = agg(aggregation, c))

  graph <- ggplot(data, aes(x = as.factor(b), y = c, group = a))  + geom_line(aes(color = a))
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab)
  graph <- graph + theme_ds() + scale_color_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Lollipop
#' lollipop. segment and point
#' gg_lollipop_YeaNum.
#' @name gg_lollipop_YeaNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Yea-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_lollipop_YeaNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                xLabel = NULL, yLabel = NULL, angle_x = 0, shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(x = a, y = b)) +
    geom_segment(aes(xend=a, yend=0)) + geom_point(aes(color = ""), show.legend = FALSE, shape = shape_type) +
    theme_ds() +
    scale_color_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Smooth
#' smooth
#' @name gg_smooth_DatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_smooth_DatNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
                              xLabel = NULL, yLabel = NULL, angle_x = 0, shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>%
    dplyr::filter(!is.na(a), !is.na(b))

  graph <- ggplot(data, aes(x = a, y = b)) + geom_point(aes(color = ""), show.legend = FALSE, shape = shape_type) +
    scale_x_date() + geom_smooth(aes(color = "*"), show.legend = FALSE) + theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  return(graph)
}

#' Lollipop
#' lollipop
#' @name gg_lollipop_DatNum.
#' @param x A data.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Dat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_lollipop_DatNum. <- function(data, titleLabel = "", subtitle = "",
                                caption = "", xLabel = NULL, yLabel = NULL, angle_x = 0,
                                shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  data <- data %>%
    dplyr::filter(!is.na(a), !is.na(b))

  graph <-  ggplot(data, aes(x = a, y = b)) +
    geom_segment(aes(xend=a, yend=0)) + geom_point(aes(color = ""), show.legend = FALSE, shape = shape_type) +
    theme_ds() + scale_color_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph
}

#' Trend ribbon facet
#' Facet Trend ribbon
#' @name gg_trend_ribbon_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)

gg_trend_ribbon_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, shape_type = 19, alpha = 0.3, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
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

  graph <- ggplot(data_count, aes(x = xorder, y = b)) + geom_point(aes(color = ""), shape = shape_type, show.legend = FALSE) +
    geom_smooth(aes(colour="*", fill = "*"), alpha = alpha, show.legend = FALSE) + facet_wrap(~a) +
    scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}


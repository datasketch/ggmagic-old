
#' Horizontal line
#' Tiene múltiples líneas
#' @name gg_line_hor_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_hor_CaYeNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, nbreaks = NULL,
                                shape_type = 19,
                                aggregation = "sum", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
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
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)
  graph
}


#' Scatter
#' ciculos
#' @name gg_circle_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_circle_CaYeNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
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



#' Steam
#' Steam
#' @name gg_steam_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_steam_CaYeNu. <-  function(data, titleLabel = "",  subtitle = "", caption = "", xLabel = NULL,
                              yLabel = NULL, fillLabel = NULL, leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    tidyr::spread(b, c) %>% tidyr::gather(b, c, -a)
  data_graph[is.na(data_graph)] <- 0
  data_graph$b <- as.numeric(data_graph$b)

  graph <- ggplot(data_graph, aes(x = b, y = c, group = a, fill = a)) +
    stat_steamgraph() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_manual(values = getPalette()) +
    theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab)  +
    theme(legend.position = leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Slope
#' Slope
#' @name gg_slope_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_slope_CaYeNu. <-  function(data, titleLabel = "",  subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
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


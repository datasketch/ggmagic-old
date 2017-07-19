#' Vertical stacked histogram
#' Stacked Vertical Histogram
#' @name gg_hist_stacked_ver_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_stacked_ver_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                        yLabel = NULL, fillLabel = NULL, leg_pos = "right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_histogram(aes(fill = a), binwidth = 10) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    theme_ds() + theme(legend.position=leg_pos) + scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph
}


#' Horizontal histogram + mean facet
#' Facet Horizontal Histogram + Mean
#' @name gg_hist_hor_mean_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_hor_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                           yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  graph <- gg_hist_ver_mean_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical histogram facet
#' Facet Vertical Histogram
#' @name gg_hist_ver_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, leg_pos='right', angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) + facet_wrap(~a) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
    scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal histogram facet
#' Facet Horizontal Histogram
#' @name gg_hist_hor_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_hor_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  graph <- gg_hist_ver_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical histogram + distribution facet
#' Facet Vertical Histogram + Dist
#' @name gg_dist_hist_ver_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                           yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
    geom_density(aes(color = ""), show.legend = FALSE) +
    theme_ds() + theme(legend.position=leg_pos) +
    scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
  graph <- graph + facet_wrap(~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal histogram + distribution facet
#' Facet Horizontal Histogram + Dist
#' @name gg_dist_hist_hor_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hist_hor_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                           yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  graph <- gg_dist_hist_ver_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical histogram + distribution + mean facet
#' Facet Vertical Histogram + Dist + Mean
#' @name gg_dist_hist_ver_mean_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hist_ver_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(prom = mean(b, na.rm = TRUE))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
    geom_density(aes(color = ""), show.legend = FALSE) +
    geom_vline(data = data_graph, aes(xintercept = prom, color = "*"), linetype = "dotted", size = 1, show.legend = FALSE) +
    theme_ds() + theme(legend.position=leg_pos) +
    scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
  graph <- graph + facet_wrap(~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal histogram + distribution + mean facet
#' Facet Horizontal Histogram + Dist + Mean
#' @name gg_dist_hist_hor_mean_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hist_hor_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  graph <- gg_dist_hist_ver_mean_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical density dot + distribution facet
#' Facet Vertical Dot Dist
#' @name gg_dot_dist_ver_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_density(aes(colour = a), show.legend = FALSE) +
    geom_point(aes(y = 0), color = "#D55E00", alpha = alpha, show.legend = FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
    theme(legend.position=leg_pos) +
    scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph <- graph + facet_wrap(~a)

  graph
}


#' Horizontal histogram + mean facet
#' Facet Horizontal Histogram + Mean
#' @name gg_hist_hor_mean_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_hor_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                           yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  graph <- gg_hist_ver_mean_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical histogram facet
#' Facet Vertical Histogram
#' @name gg_hist_ver_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, leg_pos='right', angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) + facet_wrap(~a) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
    scale_fill_manual(values = getPalette()) +
    theme(legend.position=leg_pos) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal histogram facet
#' Facet Horizontal Histogram
#' @name gg_hist_hor_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_hor_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  graph <- gg_hist_ver_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical histogram + distribution facet
#' Facet Vertical Histogram + Dist
#' @name gg_dist_hist_ver_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                           yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
    geom_density(aes(color = ""), show.legend = FALSE) +
    theme_ds() + theme(legend.position=leg_pos) +
    scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
  graph <- graph + facet_wrap(~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal histogram + distribution facet
#' Facet Horizontal Histogram + Dist
#' @name gg_dist_hist_hor_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hist_hor_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                           yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  graph <- gg_dist_hist_ver_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical histogram + distribution + mean facet
#' Facet Vertical Histogram + Dist + Mean
#' @name gg_dist_hist_ver_mean_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hist_ver_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(prom = mean(b, na.rm = TRUE))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
    geom_density(aes(color = ""), show.legend = FALSE) +
    geom_vline(data = data_graph, aes(xintercept = prom, color = "*"), linetype = "dotted", size = 1, show.legend = FALSE) +
    theme_ds() + theme(legend.position=leg_pos) +
    scale_color_manual(values = getPalette()) + scale_fill_manual(values = getPalette())
  graph <- graph + facet_wrap(~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal histogram + distribution + mean facet
#' Facet Horizontal Histogram + Dist + Mean
#' @name gg_dist_hist_hor_mean_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hist_hor_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                                yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  graph <- gg_dist_hist_ver_mean_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}



#' Vertical density dot + histogram facet
#' Facet Vertical Dot Histogram
#' @name gg_dot_hist_ver_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_hist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, leg_pos='right', alpha = 0.3, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) +
    geom_point(aes(y=0), alpha = alpha, color = "#D55E00") +
    facet_wrap(~a) +
    scale_fill_manual(values = getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal density dot + histogram facet
#' Facet Horizontal Histogram + Dot
#' @name gg_dot_hist_hor_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_hist_hor_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){

  graph <- gg_dot_hist_ver_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, alpha, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical density dot + histogram + mean facet
#' Facet Vertical Histogram + Mean + Dot
#' @name gg_dot_hist_ver_mean_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_hist_ver_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, leg_pos='right', alpha = 0.3, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(prom = mean(b, na.rm = TRUE))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) +
    facet_wrap(~a) +
    geom_vline(data = data_graph, aes(xintercept = prom, color = ""), linetype = "dotted", size = 1, show.legend = FALSE) +
    geom_point(aes(y = 0),  alpha = alpha, color = "#D55E00")
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
    scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette()[2]) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal density dot + histogram + mean facet
#' Facet Horizontal Histogram + Mean + Dot
#' @name gg_dot_hist_hor_mean_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_hist_hor_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){

  graph <- gg_dot_hist_ver_mean_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, alpha, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical density dot + histogram + distribution facet
#' Facet Vertical Histogram + Dist + Dot
#' @name gg_dot_dist_hist_ver_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_hist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
    geom_density(aes(color=""), show.legend = FALSE) +
    geom_point(aes(y = 0), alpha = alpha, color = "#D55E00") +
    theme_ds() +
    scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette())
  graph <- graph + facet_wrap(~a) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

  graph
}

#' Horizontal density dot + histogram + distribution facet
#' Facet Horizontal Histogram + Dist + Dot
#' @name gg_dot_dist_hist_hor_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_hist_hor_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                               yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){

  graph <- gg_dot_dist_hist_ver_facet_CatNum.(data, titleLabel,subtitle, caption, xLabel, yLabel, leg_pos, alpha, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}


#' Vertical density dot + histogram + distribution + mean facet
#' Facet Vertical Histogram + Dist + Mean + Dot
#' @name gg_dot_dist_hist_ver_mean_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_hist_ver_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL,
                                                    leg_pos = "right", alpha = 0.3, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(prom = mean(b, na.rm = TRUE))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
    geom_density(aes(color = ""), show.legend = FALSE) +
    geom_vline(data = data_graph, aes(xintercept = prom, colour = "*"), linetype = "dotted", size = 1, show.legend = FALSE) +
    geom_point(aes(y=0), alpha = alpha, color = "#D55E00") +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme_ds() + scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette())
  graph <- graph + facet_wrap(~a) + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal density dot + histogram + distribution + mean facet
#' Facet Horizontal Histogram + Dist + Mean + Dot
#' @name gg_dot_dist_hist_hor_mean_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_hist_hor_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){

  graph <- gg_dot_dist_hist_ver_mean_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, alpha, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Vertical histogram + mean facet
#' Facet Vertical Histogram + Mean
#' @name gg_hist_ver_mean_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_ver_mean_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                           yLabel = NULL, leg_pos='right', angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(prom = mean(b, na.rm = TRUE))

  graph <- ggplot(data, aes(x = b)) + geom_histogram(aes(fill = ""), show.legend = FALSE) +
    geom_vline(data = data_graph, aes(xintercept = prom, color = ""), linetype = "dotted", size = 1, show.legend = FALSE) +
    facet_wrap(~a)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds() +
    scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette()[2]) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Histogram
#' Histogram
#' @name gg_hist_Num.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                         yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a))

  graph <- ggplot(data, aes(x=a)) + geom_histogram(aes(fill= ""), show.legend = FALSE)

  graph <- graph + geom_vline(aes(xintercept=mean(a), color = ""), linetype="dotted",
                              show.legend = FALSE) +
    scale_color_manual(values = getPalette()[2]) + scale_fill_manual(values = getPalette())


  graph <- graph + theme_ds()
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = yLabel) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  return(graph)

}

#' Histogram + density
#' Histograms with density
#' @name gg_hist_dens_Num.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_dens_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                              yLabel = NULL,  alfa = 0.5, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a))

  graph <- ggplot(data, aes(x=a)) + geom_histogram(aes(y=..density.., fill = ""), show.legend = FALSE) +
    geom_density(alpha=alfa, aes(color = ""), show.legend = FALSE)
  graph <- graph + geom_vline(aes(xintercept=mean(a), color = ""),
                              linetype = "dotted", show.legend = FALSE) +
    scale_fill_manual(values = getPalette()) + scale_color_manual(values = getPalette()[2])

  graph <- graph + theme_ds()
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  return(graph)

}


#' Histogram density
#' density histogram
#' @name gg_density_hist_Num.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_density_hist_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                 yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% "Índice"
  ylab <- yLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a))

  graph <- ggplot(data, aes(x=a)) + geom_density(aes(fill = ""), show.legend = FALSE)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}
#' Vertical histogram dot bar
#' Dot bar
#' @name gg_dot_bar_Num.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_Num. <- function(data, titleLabel = "", subtitle = "", caption = "",
                            xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[1]
  xlab <- xLabel %||% "Index"
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a))

  graph <- ggplot(data, aes(a)) + geom_dotplot(aes(fill = ""), show.legend = FALSE)
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph <- graph + theme_ds() + scale_fill_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal histogram dot bar
#' Dot bar flipped
#' @name gg_dot_bar_flip_Num.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_flip_Num. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                 xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  graph <- gg_dot_bar_Num.(data, titleLabel, subtitle, caption, xLabel, yLabel, angle_x, ...)
  graph <- graph + coord_flip()

  graph
}

#' Vertical histogram
#' Histogram 2D
#' @name gg_hist_NumNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_NumNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
                            xLabel = NULL, yLabel = NULL, reverse = FALSE, angle_x = 0, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a), !is.na(b))

  binNummber <- floor(sqrt(nrow(data)))
  graph <- ggplot(data, aes(x = a, y = b)) + stat_bin2d(bins = binNummber) +
    theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position = leg_pos)

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }
  graph
}

#' Horizontal histogram
#' Histogram 2d flipped
#' @name gg_hist_flip_NumNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_hist_flip_NumNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                 xLabel = NULL, yLabel = NULL, reverse = FALSE, angle_x = 0, leg_pos = "right", ...){

  graph <- gg_hist_NumNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, reverse, angle_x, leg_pos, ...)
  graph <- graph + coord_flip()
  graph
}


#' Histogram
#' histogram
#' @name gg_histogram_CatDat.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Dat
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_histogram_CatDat. <- function(data, titleLabel = "", subtitle = "", caption = "",xLabel = NULL,
                                 yLabel = NULL, fillLabel = NULL, angle_x = 0, leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% "Conteo"
  clab <- fillLabel %||% nms[1]
  d <- f$d

  d <- d %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  g <- ggplot(d, aes(x=as.Date(b), fill= a)) +
    stat_bin(binwidth=1, position="identity") +
    #scale_x_date(breaks=date_breaks(width="1 month")) +
    scale_fill_manual(values = getPalette()) +
    theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = clab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)
  g
}



#' Density distribution
#' Coloured Density Distribution
#' @name gg_density_multi_dist_coloured_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_density_multi_dist_coloured_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                                   xLabel = NULL, yLabel = NULL, fillLabel = NULL, leg_pos = "right",
                                                   angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- fillLabel %||% nms[1]
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_density(aes(colour = a)) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, color = clab) +
    theme_ds() + theme(legend.position=leg_pos) + scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}


#' Vertical distribution facet
#' Facet Vertical Dist
#' @name gg_dist_ver_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_ver_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% "Conteo"
  xlab <- xLabel %||% nms[2]
  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(data, aes(b))
  graph <- graph + geom_density(aes(colour = a), show.legend = FALSE) + facet_wrap(~a) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) + theme_ds()
  graph <- graph + scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)

  graph
}

#' Horizontal distribution facet
#' Facet Horizontal Dist
#' @name gg_dist_hor_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_hor_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                      yLabel = NULL, leg_pos="right", angle_x = 0, ...){

  graph <- gg_dist_ver_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, angle_x)

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

#' Horizontal density dot + distribution facet
#' Facet Horizontal Dot Dist
#' @name gg_dot_dist_hor_facet_CatNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Cat-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_dist_hor_facet_CatNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                          yLabel = NULL, leg_pos="right", alpha = 0.3, angle_x = 0, ...){

  graph <- gg_dot_dist_ver_facet_CatNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, leg_pos, alpha, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}


#' Horizontal density 2D bin
#' Density in 2D
#' @name gg_dens_NumNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dens_NumNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                            yLabel = NULL, reverse = FALSE, angle_x = 0, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a), !is.na(b))

  graph <- ggplot(data, aes(x = a, y = b)) +
    stat_density2d(geom = "tile", aes(fill = ..density..), contour = FALSE) +
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
  return(graph)
}


#' Vertical density 2D bin
#' Density 2D flipped
#' @name gg_dens_flip_NumNum.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num-Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dens_flip_NumNum. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                 yLabel = NULL, reverse = FALSE, angle_x = 0, leg_pos = "right", ...){

  graph <- gg_dens_NumNum.(data, titleLabel, subtitle, caption, xLabel, yLabel, reverse, angle_x, leg_pos, ...)
  graph <- graph + coord_flip()
  graph
}



#' Cumulative distribution function
#' Cumulative distribution function
#' @name gg_dist_cum_Num.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Num
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dist_cum_Num. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel = NULL, angle_x = 0, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[1]
  xlab <- xLabel %||% "index"
  data <- f$d

  data <- data %>% dplyr::filter(!is.na(a))

  graph <- ggplot(data, aes(a)) + geom_step(aes(y=..y.., color = ""), stat="ecdf", show.legend = FALSE) +
    scale_color_manual(values = getPalette())

  graph <- graph + theme_ds()
  graph <- graph + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  return(graph)

}





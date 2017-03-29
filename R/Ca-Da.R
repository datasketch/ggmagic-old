#' Horizontal line + point
#' pointlines
#' @name gg_pointline_hor_CaDa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pointline_hor_CaDa. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                   xLabel = NULL, yLabel = NULL, angle_x = 0, shape_type = 19, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[1]
  d <- f$d

  d <- d %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  graph <- ggplot(d, aes(x = b, y = a, colour = a)) +
       geom_point(shape = shape_type) +
       theme_ds() + scale_color_manual(values = getPalette()) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    scale_x_date() + guides(color = FALSE) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

 return(graph)
}

#' Vertical line + point
#' pointlines
#' @name gg_pointline_ver_CaDa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pointline_ver_CaDa. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                   xLabel = NULL, yLabel = NULL, angle_x = 0, shape_type = 19, ...){

  graph <- gg_pointline_hor_CaDa.(data, titleLabel, subtitle, caption, xLabel, yLabel,
                                  angle_x, shape_type, ...)

  graph <- graph + coord_flip()

  return(graph)
}



#' Histogram
#' histogram
#' @name gg_histogram_CaDa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Da
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_histogram_CaDa. <- function(data, titleLabel = "", subtitle = "", caption = "",xLabel = NULL,
                               yLabel = NULL, angle_x = 0, leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% "Conteo"
  d <- f$d

  d <- d %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b))

  g <- ggplot(d, aes(x=as.Date(b), fill= a)) +
    stat_bin(binwidth=1, position="identity") +
    #scale_x_date(breaks=date_breaks(width="1 month")) +
    scale_fill_manual(values = getPalette()) +
    theme_ds() + labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
    theme(legend.position=leg_pos)
  g
}


#' gg_bar_facet_ver_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_bar_facet_ver_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_ver_CaYeNu. <- function(data,...){
  data <- fringe(data)
  gg_bar_facet_ver_CaCaNu.(selectFringeCols(data,c(2,1,3)),...)
}


#' gg_bar_facet_hor_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_bar_facet_hor_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_facet_hor_CaYeNu. <- function(data,...){
  data <- fringe(data)
  gg_bar_facet_hor_CaCaNu.(selectFringeCols(data,c(2,1,3)),...)
}

#' gg_bar_grouped_ver_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_bar_grouped_ver_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
#'
gg_bar_grouped_ver_CaYeNu. <- function(data,...){
  data <- fringe(data)
  gg_bar_grouped_ver_CaCaNu.(selectFringeCols(data,c(2,1,3)),...)
}

#' gg_bar_grouped_hor_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_bar_grouped_hor_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_grouped_hor_CaYeNu. <- function(data,...){
  graph <- gg_bar_grouped_ver_CaYeNu.(data, ...) +
    coord_flip()
  graph
}


#' gg_bar_grouped2_ver_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_bar_grouped2_ver_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
#'
gg_bar_grouped2_ver_CaYeNu. <- function(data,...){
  data <- fringe(data)
  gg_bar_grouped_ver_CaCaNu.(data,...)
}

#' gg_bar_grouped2_hor_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_bar_grouped2_hor_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_grouped2_hor_CaYeNu. <- function(data, ...){
  graph <- gg_bar_grouped2_ver_CaYeNu.(data, ...) +
    coord_flip()
  graph
}



#' gg_bar_stacked_ver_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_bar_stacked_ver_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu, Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_ver_CaYeNu. <- function(data,...){
  data <- fringe(data)
  gg_bar_stacked_ver_CaCaNu.(selectFringeCols(data,c(2,1,3)),...)
}


#' gg_bar_stacked_hor_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_bar_stacked_hor_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu, Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_hor_CaYeNu. <- function(data, ...){

  graph <- gg_bar_stacked_ver_CaYeNu.(data, ...)
  graph + coord_flip()
}


#' gg_bar_stacked_100_ver_CaYeNu.
#' 100 horizontal stacked bar graph
#' @name gg_bar_stacked_100_ver_CaYeNu.
#' @param x A category.
#' @param y A category.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_ver_CaYeNu. <- function(data,...){
  data <- fringe(data)
  gg_bar_stacked_100_ver_CaCaNu.(selectFringeCols(data,c(2,1,3)),...)
}


#' gg_bar_stacked_100_hor_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_bar_stacked_100_hor_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu, Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_hor_CaYeNu. <- function(data, title = "",subtitle = "", caption = "",
                                           xLabel = NULL, yLabel = NULL,
                                           leg_pos = "right", ...){

  graph <- gg_bar_stacked_100_ver_CaYeNu.(data, title, subtitle, caption,
                                          xLabel, yLabel, leg_pos)
  graph + coord_flip()
}



#' gg_line_hor_CaYeNu.: title.
#' Lines
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
gg_line_hor_CaYeNu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                                 yLabel = NULL, leg_pos = "right", angle = 0, by = NULL, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[2]
  ylab <- yLabel %||% nms[3]
  data <- f$d
  if(nrow(data)==0) return()

  nuestroBy <- ifelse(length(unique(data$b )) <= 7, length(unique(data$b)), 5)
  by <- by %||% nuestroBy

  graph <- ggplot(data, aes(x = b ,y=c,group=a,colour=a)) +
    geom_line(stat = "identity") + theme_ds() +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(breaks = round(seq(min(data$b),max(data$b), length.out = by))) +
    scale_color_manual(values = getPalette())  +
    theme(legend.position = leg_pos) +
    theme(axis.text.x = element_text(angle = angle, hjust = 1)) +
    labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph
}

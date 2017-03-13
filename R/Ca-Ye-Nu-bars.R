#' gg_bar_facet_ver_CaYeNu
#' Barras stacked
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


#' gg_bar_facet_hor_CaYeNu.
#' Barras stacked
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

#' gg_bar_grouped_ver_CaYeNu.
#' Barras stacked
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

#' gg_bar_grouped_hor_CaYeNu.
#' Barras stacked
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


#' gg_bar_grouped2_ver_CaYeNu.
#' Barras stacked
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

#' gg_bar_grouped2_hor_CaYeNu.
#' Barras stacked
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



#' gg_bar_stacked_ver_CaYeNu.
#' Barras stacked
#' @name gg_bar_stacked_ver_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_ver_CaYeNu. <- function(data,...){
  data <- fringe(data)
  gg_bar_stacked_ver_CaCaNu.(selectFringeCols(data,c(2,1,3)),...)
}


#' gg_bar_stacked_hor_CaYeNu.
#' Barras stacked
#' @name gg_bar_stacked_hor_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
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
#' @section ftypes: Ca-Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_stacked_100_ver_CaYeNu. <- function(data,...){
  data <- fringe(data)
  gg_bar_stacked_100_ver_CaCaNu.(selectFringeCols(data,c(2,1,3)),...)
}


#' gg_bar_stacked_100_hor_CaYeNu.
#' Barras stacked
#' @name gg_bar_stacked_100_hor_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu
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

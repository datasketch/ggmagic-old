#' Vertical stacked bar facet
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
  f <- fringe(data)

  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c)) %>% select(a = b, b = a, c)

  graph <- gg_bar_facet_ver_CaCaNu.(data, ...)
  graph
}


#' Horizontal stacked bar facet
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

  f <- fringe(data)

  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c)) #%>% select(a = b, b = a, c)

  f$d <- data

  graph <- gg_bar_facet_hor_CaCaNu.(selectFringeCols(f,c(2,1,3)), ...)

  graph
}

#' Vertical grouped bar by first variable
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

  f <- fringe(data)

  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c)) #%>% select(a = b, b = a, c)

  f$d <- data

  graph <- gg_bar_grouped_ver_CaCaNu.(selectFringeCols(f,c(2,1,3)), ...)

  graph
}

#' Horizontal grouped bar by first variable
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


#' Vertical grouped bar by second variable
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
  gg_bar_grouped_ver_CaCaNu.(data, ...)
}

#' Horizontal grouped bar by second variable
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


#' Vertical stacked bar by second variable
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

  f <- fringe(data)

  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c)) #%>% select(a = b, b = a, c)

  f$d <- data

  graph <- gg_bar_stacked_ver_CaCaNu.(selectFringeCols(f,c(2,1,3)), ...)
  graph
}


#' Horizontal stacked bar by second variable
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


#' Horizontal 100% stacked bar by second variable
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

  f <- fringe(data)

  data <- f$d

  data <- data %>% dplyr::mutate(a = ifelse(is.na(a), "NA", a)) %>%
    dplyr::filter(!is.na(b), !is.na(c)) #%>% select(a = b, b = a, c)

  f$d <- data
  graph <- gg_bar_stacked_100_ver_CaCaNu.(selectFringeCols(f,c(2,1,3)), ...)
  graph
}


#' Vertical 100% stacked bar by second variable
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
gg_bar_stacked_100_hor_CaYeNu. <- function(data, ...){

  graph <- gg_bar_stacked_100_ver_CaYeNu.(data, ...)
  graph + coord_flip()
}

#' gg_bars_stk_ver_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_bars_stk_ver_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bars_stk_ver_CaYeNu. <- function(data, title = NULL,xlab = NULL, ylab = NULL, clab = NULL){
  f <- fringe(data)
  nms <- getCnames(f)
  data <- f$d
  xlab <- xlab %||% nms[2]
  ylab <- ylab %||% nms[3]
  clab <- clab %||% nms[1]
  ggplot(data, aes(x=b,y=c,group=a,fill=a)) +
    geom_bar(stat = "identity") +
    ylab(ylab) +
    xlab(xlab) +
    scale_y_continuous(labels = comma) +
    scale_fill_discrete(name = clab) +
    ggtitle(title)
}

#' gg_bars_grp_ver_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_bars_grp_ver_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bars_grp_ver_CaYeNu. <- function(data, title = NULL,xlab = NULL, ylab = NULL, clab = NULL){
  f <- fringe(data)
  nms <- getCnames(f)
  data <- f$d
  xlab <- xlab %||% nms[2]
  ylab <- ylab %||% nms[3]
  clab <- clab %||% nms[1]
  ggplot(data, aes(x=b,y=c,group=a,fill=a)) +
    geom_bar( position = "dodge", stat="identity") +
    ylab(ylab) +
    xlab(xlab) +
    scale_y_continuous(labels = comma) +
    scale_fill_discrete(name = clab) +
    ggtitle(title)
}



#' gg_bars_stk_hor_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_bars_stk_hor_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bars_stk_hor_CaYeNu. <- function(data, title = NULL,xlab = NULL, ylab = NULL, clab = NULL){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[3]
  clab <- clab %||% nms[2]
  data <- f$d
  ggplot(data, aes(x=a,y=c,group=factor(b),fill=factor(b))) +
    geom_bar(stat = "identity") +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    scale_y_continuous(labels = comma) +
    scale_fill_discrete(name = clab) +
    coord_flip()
}

#' gg_lines_hor_CaYeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_lines_hor_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_lines_hor_CaYeNu. <- function(data, title = "",xlab = NULL, ylab = NULL, clab = NULL){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[2]
  ylab <- ylab %||% nms[3]
  clab <- clab %||% nms[1]
  data <- f$d
  ggplot(data, aes(x=b,y=c,group=a,colour=a)) +
    geom_line(stat = "identity") +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    scale_y_continuous(labels = comma) +
    scale_colour_discrete(name = clab)
}


#' gg_circle_CaYeNu.: title.
#' circle
#' ciculos
#' @name gg_circle_CaYeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_circle_CaYeNu. <- function(data, title = "",xlab = NULL, ylab = NULL, clab = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[2]
  ylab <- ylab %||% nms[3]
  clab <- clab %||% nms[1]
  data <- f$d


      ggplot(data, aes(x = b, y = a)) +
        geom_point(aes(size = c)) +
        theme_bw() +
        ylab(ylab) +
        xlab(xlab) +
        ggtitle(title)
}

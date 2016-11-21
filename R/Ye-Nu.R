#' gg_lines_hor_YeNu.: title.
#' Barras stacked
#' Tiene múltiples líneas
#' @name gg_lines_hor_YeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_lines_hor_YeNu. <- function(data, title = "", subtitle = "", caption = "",
                               xlab = NULL, ylab = NULL,...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d
  ggplot(data, aes(x= a,y=b,group=1)) +
    geom_line(stat = "identity",colour = "#009EE3") +
    theme_ds() +
    scale_y_continuous(labels = comma) +
    labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
}

#' gg_lollipop_YeNu.
#' lollipop
#' segment and point
#' @name gg_lollipop_YeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)

gg_lollipop_YeNu. <- function(data, title = "", subtitle = "", caption = "", xlab = NULL, ylab = NULL, size = 7,...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(x = a, y = b)) +
    geom_segment(aes(xend=a, yend=0)) + geom_point(colour = "#009EE3", size = size) +
    theme_ds() +
    labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
}

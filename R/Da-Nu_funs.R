
#' gg_lines_DaNu. : title.
#' Lines
#' @name gg_lines_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_lines_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(x = a, y = b, group=1)) +
    geom_line(stat = "identity") +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    scale_y_continuous(labels = comma) +
    ggtitle(title)
}

#' gg_scatter_DaNu. : title.
#' Scatter
#' @name gg_scatter_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_scatter_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(x = a, y = b)) +
    geom_point() +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    scale_y_continuous(labels = comma) +
    ggtitle(title)
}

#' gg_box_DaNu. : title.
#' Scatter
#' @name gg_box_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_box_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(x = a, y = b, group=1)) +
    geom_boxplot() +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    scale_y_continuous(labels = comma) +
    ggtitle(title)
}

#' gg_violin_DaNu. : title.
#' Violin
#' @name gg_violin_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_violin_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(x = a, y = b, group=1)) +
    geom_violin() +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    scale_y_continuous(labels = comma) +
    ggtitle(title)
}

#' gg_area_DaNu. : title.
#' Area
#' @name gg_area_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(x = a, y = b, group=1)) +
    geom_area() +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    scale_y_continuous(labels = comma) +
    ggtitle(title)
}

#' gg_kagi_DaNu. : title.
#' Kagi
#' @name gg_kagi_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_kagi_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(x = a, y = b)) +
    geom_line(aes(color=ifelse(c(diff(b),NA) > 0, "Gain", "Loss"), group=NA)) +
    scale_color_manual(guide="none",values=c(Gain="Green", Loss="Red")) +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    scale_y_continuous(labels = comma) +
    ggtitle(title)
}



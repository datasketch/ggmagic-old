#' gg_horizon_DaNu.
#' Horizon
#' @name gg_horizon_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_horizon_DaNu. <- function(data, titleLabel = "Report", xLabel = NULL,
                             yLabel =  NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d
  data$a <- as.Date(data$a)
  graph <- ggplot_horizon(data, 'a', 'b')
  graph <- graph + scale_fill_continuous(low = 'green', high = 'red') + theme_minimal() +
    labs(tittle = titleLabel, x = xlab, y = ylab)

  return(graph)
}

#' gg_waterfall_DaNu.
#' Waterfall
#' @name gg_waterfall_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_waterfall_DaNu. <- function(data, titleLabel = "Report", xLabel = NULL,
                             yLabel =  NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d
  data$a <- as.Date(data$a)
  graph <- ggplot_waterfall(data,'a','b') + theme_minimal() +
    labs(tittle = titleLabel, x = xlab, y = ylab)

  return(graph)
}


#' gg_lines_DaNu.
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
gg_lines_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(x = as.Date(a), y = b, group=1)) +
    geom_line(stat = "identity") +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title)
}

#' gg_scatter_DaNu.
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
gg_scatter_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(x =as.Data(a), y = b)) +
    geom_point() +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    scale_y_continuous(labels = comma) +
    ggtitle(title)
}

#' gg_box_DaNu.
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
gg_box_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(x = as.Data(a), y = b, group=1)) +
    geom_boxplot() +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title) +
    scale_y_continuous(labels = comma) +
    ggtitle(title)
}

#' gg_violin_DaNu.
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
gg_violin_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(x = as.Data(a), y = b, group=1)) +
    geom_violin() +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title)
}

#' gg_area_DaNu. :
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
gg_area_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(x = as.Data(a), y = b, group=1)) +
    geom_area() +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title)
}

#' gg_kagi_DaNu.
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
gg_kagi_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(x = as.Date(a), y = b)) +
    geom_line(aes(color=ifelse(c(diff(b),NA) > 0, "Gain", "Loss"), group=NA)) +
    scale_color_manual(guide="none",values=c(Gain="Green", Loss="Red")) +
    ylab(ylab) +
    xlab(xlab) +
    ggtitle(title)
}


#' gg_smooth_DaNu.
#' smooth
#' @name gg_smooth_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)


gg_smooth_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(x = a, y = b)) + geom_point() +
  scale_x_date() + geom_smooth() + ggtitle(title) + xlab(xlab) + ylab(ylab)

}

#' gg_div_DaNu.
#' div
#' @name gg_div_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)

gg_div_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL, ...){

  data$Year <- format(data$a, "%Y")
  data$Month <- format(data$a, "%b")
  data$Day <- format(data$a, "%d")

  data$MonthDay <- format(data$a, "%d-%b")

  data$CommonDate <- as.Date(paste0("2000-",format(data$a, "%j")), "%Y-%j")

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data = data,
         mapping = aes(x = a, y = b, shape = Year, colour = Year)) +
    geom_point() +
    geom_line() +
    facet_grid(facets = Year ~ .) +
    scale_x_date(labels = function(x) format(x, "%d-%b")) + ggtitle(title) + xlab(xlab) + ylab(ylab)
}


#' gg_bar_DaNu.
#' bar
#' @name gg_bar_DaNu.
#' @param x A data.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(a, b)) +
  geom_bar(stat="identity", na.rm = TRUE) +
  scale_x_date(labels = date_format("%b %y")) +  theme_bw() +
  xlab(xlab) + ylab(ylab) + ggtitle(title)
}


#' gg_bubbles_DaNu.
#' bubbles
#' @name gg_bar_DaNu.
#' @param x A data.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubbles_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(x = a, y = b, size = b) )+
  geom_point(shape = 21, colour = "#000000", fill = "#40b8d0") + theme(legend.position="none") +
    xlab(xlab) + ylab(ylab) + ggtitle(title)
}


#' gg_lollipop_DaNu.
#' lollipop
#' @name gg_bar_DaNu.
#' @param x A data.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_lollipop_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data, aes(x = a, y = b)) +
  geom_segment(aes(xend=a, yend=0)) + geom_point() +
  xlab(xlab) + ylab(ylab) + ggtitle(title)
}


#' gg_stepped_stacked_area_DaNu.
#' stepped stacked area.
#' @name gg_bar_DaNu.
#' @param x A data.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_stepped_stacked_area_DaNu. <- function(data, title = "", xlab = NULL, ylab = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  ggplot(data) +
  geom_step(aes(x = seq_along(a), y = b)) + theme_bw() +
  xlab(xlab) + ylab(ylab) + ggtitle(title)
}





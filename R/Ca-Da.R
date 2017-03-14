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
gg_pointline_hor_CaDa. <- function(data,title = "", subtitle = "", caption = "",
                                   xlab = NULL, clab = NULL, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xlab %||% nms[2]
  clab <- clab %||% nms[1]
  d <- f$d

  graph <- ggplot(d, aes(x = b, y = 0, colour = a)) +
       geom_point() +
       scale_colour_brewer(clab,palette = 'Set1') +
       theme_ds()  + scale_color_manual(values = getPalette()) +
       labs(title = title, subtitle = subtitle, caption = caption, x = xlab)+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())

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
gg_pointline_ver_CaDa. <- function(data,title = "", subtitle = "", caption = "",ylab = NULL, clab = NULL, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  clab <- clab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(x = b, y = 0, colour = a)) +
       geom_point() +
       scale_colour_brewer(clab,palette = 'Set1') +
       coord_flip() +
       theme_ds() +
       theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) + labs(title = title, subtitle = subtitle, caption = caption )
  graph
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
gg_histogram_CaDa. <- function(data,title = "", subtitle = "", caption = "",xlab = NULL, clab = NULL, ...){
  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xlab %||% nms[2]
  ylab <- "count"
  clab <- clab %||% nms[1]
  d <- f$d
  g <- ggplot(d, aes(x=as.Date(b), fill= a)) +
    stat_bin(binwidth=1, position="identity") +
    #scale_x_date(breaks=date_breaks(width="1 month")) +
    xlab(xlab) +
    ylab(ylab) +
    scale_fill_brewer(clab,palette = 'Set1') +
    ggtitle(title) +
    theme_ds()
  g
}


#' gg_pointline_hor_CaDa.: title.
#' pointlines
#' @name gg_pointline_hor_CaDa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pointline_hor_CaDa. <- function(data,title = "",xlab = NULL, clab = NULL){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[2]
  clab <- clab %||% nms[1]
  d <- f$d
g <- ggplot(d, aes(x = as.Date(b), y = 0, colour = a)) +
  geom_point() +
  scale_colour_brewer(clab,palette = 'Set1') +
  ylab(ylab) +
  xlab(xlab) +
  ggtitle(title) +
  theme_minimal() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
g
}

#' gg_pointline_ver_CaDa.: title.
#' pointlines
#' @name gg_pointline_ver_CaDa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pointline_ver_CaDa. <- function(data,title = "",ylab = NULL, clab = NULL){
  f <- fringe(data)
  nms <- getCnames(f)
  clab <- clab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d
  g <- ggplot(data, aes(x = as.Date(b), y = 0, colour = a)) +
    geom_point() +
    scale_colour_brewer(clab,palette = 'Set1') +
    ylab(xlab) +
    xlab(ylab) +
    coord_flip() +
    ggtitle(title) +
    theme_minimal() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  g
}



#' gg_histogram_CaDa.: title.
#' histogram
#' @name gg_histogram_CaDa.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca-Ye-Nu,Ca-Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_histogram_CaDa. <- function(data,title = "",xlab = NULL, clab = NULL){
  f <- fringe(data)
  nms <- getCnames(f)
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
    theme_minimal()
  g
}


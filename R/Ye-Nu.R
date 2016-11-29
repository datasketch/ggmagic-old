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
    scale_x_continuous(limits=c(min(data$a),max(data$a))) +
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
    scale_x_continuous(limits=c(min(data$a),max(data$a))) +
    labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
}

#' gg_waterfall_YeNu.
#' Waterfall
#' @name gg_waterfall_YeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_waterfall_YeNu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel =  NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d

  graph <- ggplot_waterfall(data,'a','b') + theme_ds() + theme(legend.position="none") +
           scale_color_manual(breaks = c("+",  "-", ""), values = c("#E5007D", "#009EE3", "black")) +
           scale_x_continuous(limits=c(min(data$a),max(data$a))) +
           labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab)

     return(graph)
}


#' gg_bar_coloured_x_ver_YeNu.
#' vertical bar
#' @name gg_bar_coloured_x_ver_YeNu.
#' @param x A category.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu, Ye-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_x_ver_YeNu.<- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,text = TRUE, type = 'percent', size_text = 3,
                                       yLabel = NULL, fillLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  flab <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = sum(b)) %>%
    dplyr::mutate(percent = 100 * round(count/sum(count), 4))

  graph <- ggplot(data_graph, aes(x = a, y = count, fill = factor(a))) + geom_bar(stat = "identity") +
           theme_ds() +
           scale_fill_manual(values = getPalette()) +
           theme(legend.position=leg_pos) +
           guides(fill = FALSE) +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab, fill = flab)


  if(text == TRUE & type == 'count'){
    return(graph + geom_text(aes(x = a, y = count + 0.05, label = round(count,2)), size = size_text, position = position_dodge(0.9), vjust = 0))
  }else{
    if(text == TRUE & type == 'percent'){
      return(graph + geom_text(aes(x = a, y = count + 0.05, label = paste(percent, "%", sep = "")), size = size_text, position = position_dodge(0.9), vjust = 0))
    }else{
      graph
    }
  }

}


#' gg_area_YeNu.:
#' Area
#' @name gg_area_YeNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_YeNu. <- function(data, title = "", subtitle = "", caption = "", xlab = NULL, ylab = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = b, group=1)) +
           geom_area(fill = "#009EE3") + theme_ds() +
           scale_x_continuous(limits = c(min(data$a), max(data$a))) +
           labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  return(graph)
}


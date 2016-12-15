#' gg_horizon_DaNu.
#' Horizon
#' @name gg_horizon_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)

gg_horizon_DaNu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel =  NULL, leg_pos = "right", reverse = FALSE,
                             angle_x = 0,...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d
  data$a <- lubridate::as_date(data$a)
  graph <- ggplot_horizon(data, 'a', 'b')
  graph <- graph + theme_ds() +
    labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }
  return(graph)
}

#' gg_waterfall_DaNu.
#' Waterfall
#' @name gg_waterfall_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_waterfall_DaNu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel =  NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d
  data$a <- lubridate::as_date(data$a)
  graph <- ggplot_waterfall(data,'a','b') + theme_ds() + theme(legend.position="none") +
           scale_color_manual(breaks = c("+",  "-", ""), values = getPalette()) +
           labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

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
gg_lines_DaNu. <- function(data, title = "", subtitle = "", caption = "",
                           xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  data$a <- lubridate::as_date(data$a)
  graph <- ggplot(data, aes(x = a, y = b, group=1)) +
           geom_line(stat = "identity", aes(color = ""), show.legend = FALSE) +  theme_ds() +
    scale_color_manual(values =  getPalette()) +
           labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  return(graph)
}

#' gg_lines_points_DaNu.
#' Lines
#' @name gg_lines_points_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_lines_points_DaNu. <- function(data, title = "", subtitle = "", caption = "", xLabel = NULL,
                                  yLabel = NULL, type = 1, hline = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  data$a <- lubridate::as_date(data$a)
  graph <- ggplot(data, aes(x = a, y = b, group=1)) +
    geom_line(stat = "identity", aes(color = ""), show.legend = FALSE) +  theme_ds() +
    geom_point(aes(color = ""), shape = type, show.legend = FALSE) +
    scale_color_manual(values =  getPalette()) +
    labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(!is.null(hline)){
    graph <- graph + geom_hline(data = data.frame(valores = hline),
                                aes(yintercept = valores), linetype="dotted")
  }

  return(graph)
}

#' gg_point_DaNu.
#' Point scatter plot
#' @name gg_point_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_point_DaNu. <- function(data, title = "", subtitle = "", caption = "",
                           xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  data$a <- lubridate::as_date(data$a)

  graph <- ggplot(data, aes(x = a, y = b)) +
           geom_point(aes(color = ""), show.legend = FALSE) + theme_ds() +
           scale_y_continuous(labels = comma) + scale_color_manual(values = getPalette()) +
           labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))


  return(graph)
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
gg_box_DaNu. <- function(data, title = "", subtitle = "", caption = "",
                         xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  data$a <- lubridate::as_date(data$a)

  graph <- ggplot(data) + geom_boxplot(aes(y = b,x = reorder(format(a,'%B'), a), fill=format(a,'%Y'))) +
           theme_ds() + scale_fill_manual(values = getPalette()) +
           scale_y_continuous(labels = comma) +
           labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

   return(graph)
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
gg_violin_DaNu. <- function(data, title = "", subtitle = "", caption = "",
                            xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  data$a <- lubridate::as_date(data$a)

  graph <- ggplot(data) + geom_violin(aes(y = b,x = reorder(format(a,'%B'), a), fill=format(a,'%Y'))) +
           theme_ds() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
           scale_fill_manual(values = getPalette()) +
           scale_y_continuous(labels = comma) +
           labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  return(graph)
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
gg_area_DaNu. <- function(data, title = "", subtitle = "", caption = "",
                          xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  data$a <- lubridate::as_date(data$a)

  graph <- ggplot(data, aes(x = a, y = b, group=1)) +
           geom_area(aes(fill = ""), show.legend = FALSE) + theme_ds() +
           scale_y_continuous(labels = comma) +
           labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  return(graph)
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
gg_kagi_DaNu. <- function(data, title = "", subtitle = "", caption = "", xlab = NULL,
                          ylab = NULL, hline = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xlab %||% nms[1]
  ylab <- ylab %||% nms[2]
  data <- f$d
  data$a <- lubridate::as_date(data$a)

 graph <- ggplot(data, aes(x = a, y = b)) +
          geom_line(aes(color=ifelse(c(diff(b), NA) > 0, "Gain", "Loss"), group = NA)) +
          scale_color_manual(guide="none",values = getPalette()) +
          theme_ds() +
          labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
   theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

   if(!is.null(hline)){
     graph <- graph + geom_hline(data = data.frame(valores = hline),
                                 aes(yintercept = valores), linetype="dotted")
   }

 return(graph)
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
gg_smooth_DaNu. <- function(data, title = "", subtitle = "", caption = "",
                            xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  data$a <- lubridate::as_date(data$a)

 graph <- ggplot(data, aes(x = a, y = b)) + geom_point(aes(color = ""), show.legend = FALSE) +
          scale_x_date() + geom_smooth(aes(color = "*"), show.legend = FALSE) + theme_ds() +
          labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
   scale_color_manual(values = getPalette()) +
   theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
 return(graph)
}

#' gg_points_facet_DaNu.
#' Points facet by years
#' @name gg_points_facet_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_points_facet_DaNu. <- function(data, title = "", subtitle = "", caption = "",
                                     xLabel = NULL, yLabel = NULL, angle_x = 0, ...){


  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  data$a <- lubridate::as_date(data$a)
  data$Year <- format(data$a, "%Y")
  data$Month <- format(data$a, "%b")
  data$Day <- format(data$a, "%d")

  data$MonthDay <- format(data$a, "%d-%b")

  data$CommonDate <- as.Date(paste0("2000-",format(data$a, "%j")), "%Y-%j")

  graph <- ggplot(data = data, mapping = aes(x = a, y = b, shape = Year, colour = Year)) +
           geom_point() + scale_color_manual(values = getPalette()) + theme_ds() +
           facet_grid(facets = Year ~ .) +
           scale_x_date(labels = function(x) format(x, "%d-%b")) +
           labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  return(graph)
}

#' gg_line_points_facet_DaNu.
#' Line Points facet by years
#' @name gg_line_points_facet_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_points_facet_DaNu. <- function(data, title = "", subtitle = "", caption = "",
                                  xLabel = NULL, yLabel = NULL, angle_x = 0, ...){


  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  data$a <- lubridate::as_date(data$a)
  data$Year <- format(data$a, "%Y")
  data$Month <- format(data$a, "%b")
  data$Day <- format(data$a, "%d")

  data$MonthDay <- format(data$a, "%d-%b")

  data$CommonDate <- as.Date(paste0("2000-",format(data$a, "%j")), "%Y-%j")

  graph <- ggplot(data = data, mapping = aes(x = a, y = b, shape = Year, colour = Year)) +
    geom_point() + geom_line() + scale_color_manual(values = getPalette()) + theme_ds() +
    facet_grid(facets = Year ~ .) +
    scale_x_date(labels = function(x) format(x, "%d-%b")) +
    labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  return(graph)
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
gg_bar_DaNu. <- function(data, title = "", subtitle = "", caption = "",
                         xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  data$a <- lubridate::as_date(data$a)

  graph <- ggplot(data = data, aes(x = a, y = b)) +
           geom_bar(stat="identity", na.rm = TRUE, aes(color = ""), show.legend = FALSE)  +
           scale_x_date(labels = date_format("%b %y")) +  theme_ds() +
    scale_color_manual(values = getPalette()) +
           labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  return(graph)
}


#' gg_bubbles_DaNu.
#' bubbles
#' @name gg_bubbles_DaNu.
#' @param x A data.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubbles_DaNu. <- function(data, title = "", subtitle = "", caption = "",
                             xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  data$a <- lubridate::as_date(data$a)

  graph <- ggplot(data, aes(x = a, y = b, size = b) )+
           geom_point(shape = 21, aes(fill = ""), show.legend = FALSE) +
           theme_ds() + scale_fill_manual(values = getPalette()) +
           theme(legend.position="none") +
           labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

    return(graph)
}


#' gg_lollipop_DaNu.
#' lollipop
#' @name gg_lollipop_DaNu.
#' @param x A data.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_lollipop_DaNu. <- function(data, title = "", subtitle = "",
                              caption = "", xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  data$a <- lubridate::as_date(data$a)

  graph <-  ggplot(data, aes(x = a, y = b)) +
            geom_segment(aes(xend=a, yend=0)) + geom_point(aes(color = ""), show.legend = FALSE) +
            theme_ds() + scale_color_manual(values = getPalette())
            labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  return(graph)
}


#' gg_area_stepped_stacked_DaNu.
#' stepped stacked area.
#' @name gg_area_stepped_stacked_DaNu.
#' @param x A data.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_area_stepped_stacked_DaNu. <- function(data, title = "", subtitle = "", caption = "",
                                          xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  data$a <- lubridate::as_date(data$a)

  graph <-  ggplot(data) +
            geom_step(aes(x = seq_along(a), y = b, color = ""), show.legend = FALSE) +
            theme_ds() + scale_color_manual(values = getPalette()) +
            labs(title = title, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  return(graph)
}





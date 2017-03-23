#' Horizon
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
gg_horizon_DaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel =  NULL, leg_pos = "right", reverse = FALSE,
                             angle_x = 0,...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d
  graph <- ggplot_horizon(data, 'a', 'b')
  graph <- graph + theme_ds() +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(reverse){
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[2],
                                         high = getPalette(type = "sequential")[1])
  }else{
    graph <- graph + scale_fill_gradient(low = getPalette(type = "sequential")[1],
                                         high = getPalette(type = "sequential")[2])
  }
  graph
}

#' Waterfall
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
gg_waterfall_DaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                             yLabel =  NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  ylab <- yLabel %||% nms[2]
  xlab <- xLabel %||% nms[1]
  data <- f$d
  graph <- ggplot_waterfall(data,'a','b') + theme_ds() + theme(legend.position="none") +
           scale_color_manual(breaks = c("+",  "-", ""), values = getPalette()) +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}


#' Line
#' Lines
#' @name gg_line_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_DaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                           xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data, aes(x = a, y = b, group=1)) +
           geom_line(stat = "identity", aes(color = ""), show.legend = FALSE) +  theme_ds() +
    scale_color_manual(values =  getPalette()) +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph
}

#' Line + point
#' Lines
#' @name gg_line_points_DaNu.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_points_DaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                  yLabel = NULL, shape_type = 19, hline = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  graph <- ggplot(data, aes(x = a, y = b, group=1)) +
    geom_line(stat = "identity", aes(color = ""), show.legend = FALSE) +  theme_ds() +
    geom_point(aes(color = ""), shape = shape_type, show.legend = FALSE) +
    scale_color_manual(values =  getPalette()) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  if(!is.null(hline)){
    graph <- graph + geom_hline(data = data.frame(valores = hline),
                                aes(yintercept = valores), linetype="dotted")
  }

  graph
}

#' Scatter
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
gg_point_DaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                           xLabel = NULL, yLabel = NULL, angle_x = 0, shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = b)) +
           geom_point(aes(color = ""), show.legend = FALSE, shape = shape_type) + theme_ds() +
           scale_y_continuous(labels = comma) + scale_color_manual(values = getPalette()) +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))


  graph
}

#' Boxplot
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
gg_box_DaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                         xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data) + geom_boxplot(aes(y = b,x = reorder(format(a,'%B'), a), fill=format(a,'%Y'))) +
           theme_ds() + scale_fill_manual(values = getPalette()) +
           scale_y_continuous(labels = comma) +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

   graph
}

#' Violin
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
gg_violin_DaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                            xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data) + geom_violin(aes(y = b,x = reorder(format(a,'%B'), a), fill=format(a,'%Y'))) +
           theme_ds() + theme(axis.text.x = element_text(angle = angle_x, hjust = 1)) +
           scale_fill_manual(values = getPalette()) +
           scale_y_continuous(labels = comma) +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab)
  graph
}

#' Area
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
gg_area_DaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                          xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = b, group=1)) +
           geom_area(aes(fill = ""), show.legend = FALSE) + theme_ds() +
    scale_fill_manual(values = getPalette()) +
           scale_y_continuous(labels = comma) +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Kagi
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
gg_kagi_DaNu. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                          yLabel = NULL, hline = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

 graph <- ggplot(data, aes(x = a, y = b)) +
          geom_line(aes(color=ifelse(c(diff(b), NA) > 0, "Gain", "Loss"), group = NA)) +
          scale_color_manual(guide="none",values = getPalette()) +
          theme_ds() +
          labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
   theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

   if(!is.null(hline)){
     graph <- graph + geom_hline(data = data.frame(valores = hline),
                                 aes(yintercept = valores), linetype="dotted")
   }

 return(graph)
}


#' Smooth
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
gg_smooth_DaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                            xLabel = NULL, yLabel = NULL, angle_x = 0, shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

 graph <- ggplot(data, aes(x = a, y = b)) + geom_point(aes(color = ""), show.legend = FALSE, shape = shape_type) +
          scale_x_date() + geom_smooth(aes(color = "*"), show.legend = FALSE) + theme_ds() +
          labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
   scale_color_manual(values = getPalette()) +
   theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
 return(graph)
}

#' Points facet by years
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
gg_points_facet_DaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                     xLabel = NULL, yLabel = NULL, angle_x = 0, shape_type = 19, ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  data$Year <- format(data$a, "%Y")
  data$Month <- format(data$a, "%b")
  data$Day <- format(data$a, "%d")

  data$MonthDay <- format(data$a, "%d-%b")

  #data$CommonDate <- as.Date(paste0("2000-",format(data$a, "%j")), "%Y-%j")

  graph <- ggplot(data = data, mapping = aes(x = a, y = b, colour = Year)) +
           geom_point(shape = shape_type, show.legend = FALSE) + scale_color_manual(values = getPalette()) + theme_ds() +
           facet_wrap(~Year, scales = "free") +
           #scale_x_date(labels = function(x) format(x, "%d-%b")) +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Line + point facet by years
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
gg_line_points_facet_DaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                  xLabel = NULL, yLabel = NULL, angle_x = 0, shape_type = 19, ...){


  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d
  data$Year <- format(data$a, "%Y")
  data$Month <- format(data$a, "%b")
  data$Day <- format(data$a, "%d")

  data$MonthDay <- format(data$a, "%d-%b")

  #data$CommonDate <- as.Date(paste0("2000-",format(data$a, "%j")), "%Y-%j")

  graph <- ggplot(data = data, mapping = aes(x = a, y = b, shape = Year, colour = Year)) +
    geom_line(show.legend = FALSE) + geom_point(shape = shape_type, show.legend = FALSE) + scale_color_manual(values = getPalette()) + theme_ds() +
    facet_wrap(~Year, scales = "free") +
    #scale_x_date(labels = function(x) format(x, "%d-%b")) +
    labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Vertical bar
#' bar
#' @name gg_bar_ver_DaNu.
#' @param x A data.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ver_DaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                         xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data = data, aes(x = a, y = b)) +
           geom_bar(stat="identity", aes(fill = ""), show.legend = FALSE)  +
           scale_x_date(labels = date_format("%b %y")) +  theme_ds() +
    scale_fill_manual(values = getPalette()) +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

  graph
}

#' Horizontal bar
#' bar
#' @name gg_bar_hor_DaNu.
#' @param x A data.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Da-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_hor_DaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                         xLabel = NULL, yLabel = NULL, angle_x = 0, ...){


  graph <- gg_bar_ver_DaNu.(data, titleLabel, subtitle, caption, xLabel, yLabel, angle_x, ...)

  graph <- graph + coord_flip()

  graph
}

#' Bubbles
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
gg_bubbles_DaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                             xLabel = NULL, yLabel = NULL, angle_x = 0, shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <- ggplot(data, aes(x = a, y = b, size = b) )+
           geom_point(shape = shape_type, aes(fill = ""), show.legend = FALSE) +
           theme_ds() + scale_fill_manual(values = getPalette()) +
           theme(legend.position="none") +
           labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))

    graph
}


#' Lollipop
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
gg_lollipop_DaNu. <- function(data, titleLabel = "", subtitle = "",
                              caption = "", xLabel = NULL, yLabel = NULL, angle_x = 0,
                              shape_type = 19, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <-  ggplot(data, aes(x = a, y = b)) +
            geom_segment(aes(xend=a, yend=0)) + geom_point(aes(color = ""), show.legend = FALSE, shape = shape_type) +
            theme_ds() + scale_color_manual(values = getPalette()) +
            labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph
}


#' Stepped area
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
gg_area_stepped_DaNu. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                          xLabel = NULL, yLabel = NULL, angle_x = 0, ...){

  f <- fringe(data)
  nms <- getClabels(f)
  xlab <- xLabel %||% nms[1]
  ylab <- yLabel %||% nms[2]
  data <- f$d

  graph <-  ggplot(data) +
            geom_step(aes(x = a, y = b, color = ""), show.legend = FALSE) +
            theme_ds() + scale_color_manual(values = getPalette()) +
            labs(title = titleLabel, subtitle = subtitle, caption = caption, x = xlab, y = ylab) +
    theme(axis.text.x = element_text(angle = angle_x, hjust = 1))
  graph
}





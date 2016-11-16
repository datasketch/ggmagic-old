
#' gg_waffle_Ca.: title.
#' Waffle
#' @name gg_waffle_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_waffle_Ca. <- function(data, square_size = 1, rows_number = 5, titleLabel = "",
                          subtitle = "", caption = "", leg_pos = "right", ...){
  f <- fringe(data)
  data <- f$d
  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  parts <- as.vector(data_graph$count)
  graph <- waffle(parts / square_size, rows = rows_number, colors=getPalette()) + theme_ds() +
                  theme(legend.position = leg_pos) + theme_ds_clean() +
                  labs(title = titleLabel, subtitle = subtitle, caption = caption
    )
  return(graph)
}

#' gg_bar_coloured_ver_Ca.
#' Vertical coloured bars
#' @name gg_bar_coloured_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_ver_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                    xLabel = NULL, yLabel = 'Count', fillLabel = NULL,
                                    text = TRUE, size_text = 3, leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data = data, aes(x = a, fill = factor(a))) + geom_bar()
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, subtitle = subtitle, caption = caption)

  ifelse(text == TRUE,
         return(graph +
                 geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust=1.5, colour="black",
                           position=position_dodge(.9), size = size_text) +
                 theme(legend.position = leg_pos) + guides(fill  = FALSE) +
                 theme_ds() + scale_fill_manual(values = getPalette())
                 ), return(graph + theme(legend.position = leg_pos) + guides(fill  = FALSE) +
                             theme_ds() + scale_fill_manual(values = getPalette())))
}


#' gg_bar_coloured_hor_Ca.
#' Horizontal coloured Bars
#' @name gg_bar_coloured_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_hor_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                            yLabel = 'Count', fillLabel = NULL, text = TRUE, size_text = 3,
                            leg_pos = "right", ...){

  graph <- gg_bar_coloured_ver_Ca.(data, titleLabel, subtitle, caption, xLabel,
                                   yLabel, fillLabel, leg_pos, text)

  ifelse(text == TRUE,
         return(graph +
                  geom_text(stat = "count", aes(label = ..count.., y = ..count..), vjust= 1.5, colour="black", size = size_text) + coord_flip()),
        return(graph +  coord_flip()))

}

#' gg_bar_coloured_parameter_ver_Ca.
#' Vertical coloured by parameter bars
#' @name gg_bar_coloured_parameter_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_parameter_ver_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                              xLabel = NULL, yLabel = 'Count', parameter = NULL,
                                              leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  p <-  parameter %||% sample(unique(data[,nms[1]]), 1)
  data <- f$d
  data_graph <- data %>% dplyr::group_by(a) %>% dplyr::summarise(count = n())
  graph <- ggplot(data_graph, aes(x = a, y = count)) +
    geom_bar(stat="identity", aes(fill = a %in% p ))
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, subtitle = subtitle, caption = caption)
  graph <- graph + guides(fill = FALSE) + theme(legend.position = leg_pos) +
    theme_ds() + scale_fill_manual(values = getPalette())
  return(graph)
}

#' gg_bar_coloured_parameter_hor_Ca.
#' Horizontal coloured by parameter Bars
#' @name gg_bar_coloured_parameter_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_coloured_parameter_hor_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                              xLabel = NULL, yLabel = 'Count', parameter = NULL,
                                              leg_pos = "right", ...){

  graph <- gg_bar_coloured_parameter_ver_Ca.(data, titleLabel, xLabel, yLabel, parameter, leg_pos) +
           labs(subtitle = subtitle, caption = caption)

  graph <- graph + coord_flip()
  return(graph)
}

#' gg_bar_ver_Ca.
#' Vertical bars
#' @name gg_bar_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ver_Ca. <- function(data, titleLabel = "", xLabel = NULL, subtitle = "", caption = "",
                                    yLabel = 'Count', leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data = data, aes(x = factor(a), fill = "")) + geom_bar()
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, subtitle = subtitle, caption = caption)
  graph <- graph + theme(legend.position=leg_pos) + guides(fill = FALSE) +
    theme_ds() + scale_fill_manual(values = getPalette())
  return(graph)
}

#' gg_bar_hor_Ca.
#' Horizontal Bars
#' @name gg_bar_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_hor_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                    yLabel = 'Count', leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  graph <- gg_bar_ver_Ca.(data, titleLabel, xlab, yLabel, leg_pos) + labs(subtitle = subtitle, caption = caption)

  graph <- graph + coord_flip()
  return(graph)
}

#' gg_bar_ordered_ver_Ca.
#' Ordered vertical Bars
#' @name gg_bar_ordered_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ordered_ver_Ca. <- function(data, titleLabel = "", xLabel = NULL, subtitle = "",
                                   caption = "", yLabel =  'Count', leg_pos = "right", ...){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d
  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  graph <- ggplot(data_graph, aes(x = reorder(a, count), y = count, fill = "")) +
            geom_bar(stat = "identity")
  graph <- graph + labs(title = titleLabel, x = yLabel, y = xLabel, subtitle = subtitle, caption = caption)
  graph <- graph + theme(legend.position=leg_pos) +
    theme_ds() + scale_fill_manual(values = getPalette()) + guides(fill = FALSE)

  return(graph)
}

#' gg_bar_ordered_hor_Ca.
#' Ordered horizontal Bars
#' @name gg_bar_ordered_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_ordered_hor_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                                   yLabel =  'Count', leg_pos = "right", ...){

  graph <- gg_bar_ordered_ver_Ca.(data, titleLabel, xLabel, yLabel, leg_pos)

  graph <- graph + coord_flip() + labs(subtitle = subtitle, caption = caption)

  return(graph)
}

#' gg_pie_Ca.
#' Pie
#' @name gg_pie_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_pie_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "",
                       fillLabel = NULL, leg_pos="right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>% dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(pos = cumsum(count) - count/2,
                  percent = 100 * round(count/sum(count), 4))

  graph <- ggplot(data=data_graph, aes(x = factor(1), y = count, fill = a)) +
    geom_bar(stat = "identity", width = 1) + coord_polar(theta = "y") +
    geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")))
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel,  subtitle = subtitle, caption = caption) + guides(text = FALSE)
  graph <- graph + theme_ds() + theme_ds_clean() + scale_fill_manual(values = getPalette())
    #scale_y_continuous(breaks=data_graph$pos, labels=data_graph$percent)

  return(graph)
}

#Width debe de ser un parámetro.  0 < width < 1.
#' gg_donut_Ca.
#' Donut
#' @name gg_donut_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_donut_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "", fillLabel = NULL,
                         width = 0.3, leg_pos="right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  data_graph <- data %>% dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(pos = cumsum(count) - count/2,
                  percent = 100 * round(count/sum(count), 4))

  graph <- ggplot(data=data_graph, aes(x = factor(1), y = count, fill = a)) +
    geom_bar(stat = "identity", width = width) + coord_polar(theta = "y") +
    geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")))
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel, subtitle = subtitle, caption = caption)
  graph <- graph + theme(legend.position=leg_pos) +
    theme_ds() + theme_ds_clean() + scale_fill_manual(values = getPalette())

  return(graph)
}

#' gg_dot_bar_ver_Ca.
#' Vertical Dot Bar
#' @name gg_dot_bar_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_ver_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL,
                               yLabel = 'Count', fillLabel = NULL, leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  data_graph <- data_graph %>%
                mutate(order = c(1:nrow(data_graph)))

  graph <- ggplot(data = merge(x = data, y = data_graph, by = "a", all.x = TRUE),
                  aes(x = order, fill = factor(a))) + geom_dotplot(method="histodot")

  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel,  fill = flabel, subtitle = subtitle, caption = caption)
  graph <- graph + theme_minimal() + scale_y_continuous(breaks = NULL) +
            theme(legend.position=leg_pos) + theme_ds() +
    scale_fill_manual(values = getPalette())


  return(graph)
}

#' gg_dot_bar_hor_Ca.
#' Horizontal Dot Bar
#' @name gg_dot_bar_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_dot_bar_hor_Ca. <- function(data, titleLabel = "", xLabel = NULL, yLabel = 'Count',
                               subtitle = "", caption = "", fillLabel = NULL, leg_pos = "right", ...){

  graph <- gg_dot_bar_ver_Ca.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip() + labs(subtitle = subtitle, caption = caption)

  return(graph)
}

#' gg_line_hor_Ca.
#' Horizontal Line
#' @name gg_line_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_hor_Ca. <- function(data, titleLabel = '', xLabel = NULL, subtitle = "",
                            caption = "", yLabel = 'Count', leg_pos = "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(order = c(1:nrow(.)))

  graph <- ggplot(data = data_graph, aes(x = a, y = count, group=1, colour = "")) + geom_line()
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel, subtitle = subtitle, caption = caption)

  graph <- graph + theme_minimal() + theme(legend.position=leg_pos) + theme_ds() +
    scale_color_manual(values = getPalette()) + guides(colour = FALSE)

  return(graph)
}

#' gg_line_ver_Ca.
#' Vertical Line
#' @name gg_line_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_ver_Ca. <- function(data, titleLabel = '', xLabel = NULL, subtitle = "",
                            caption = "", yLabel = 'Count', leg_pos = "right", ...){

  graph <- gg_line_hor_Ca.(data, titleLabel, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip() + labs(subtitle = subtitle, caption = caption)

  return(graph)
}

#' gg_line_point_hor_Ca.
#' Horizontal Line Point
#' @name gg_line_point_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_hor_Ca. <- function(data, titleLabel = '', xLabel = NULL, subtitle = "",
                                  caption = "", yLabel = 'Count', leg_pos = "right", ...){

  graph <- gg_line_hor_Ca.(data, titleLabel, xLabel, yLabel, leg_pos)
  graph <- graph + geom_point()+ labs(subtitle = subtitle, caption = caption)

  return(graph)
}

#' gg_line_point_ver_Ca.
#' Vertical Line Point
#' @name gg_line_point_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_line_point_ver_Ca. <- function(data, titleLabel = '', xLabel = NULL, subtitle = "",
                                  caption = "", yLabel = 'Count', leg_pos = "right", ...){

  graph <- gg_line_ver_Ca.(data, titleLabel, xLabel, yLabel, leg_pos)
  graph <- graph + geom_point()+ labs(subtitle = subtitle, caption = caption)

  return(graph)
}

#' gg_gauge_Ca.
#' Gauge
#' @name gg_gauge_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_gauge_Ca. <- function(data, titleLabel = '', subtitle = '', caption = '', ncol = 2, ...){

  gg.gauge <- function(pos, breaks=c(0,30,70,100)) {
    require(ggplot2)
    get.poly <- function(a,b,r1=0.5,r2=1.0) {
      th.start <- pi*(1-a/100)
      th.end   <- pi*(1-b/100)
      th       <- seq(th.start,th.end,length=100)
      x        <- c(r1*cos(th),rev(r2*cos(th)))
      y        <- c(r1*sin(th),rev(r2*sin(th)))
      return(data.frame(x,y))
    }
    graph <- ggplot()+
      geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y), fill = getPalette()[1])+
      geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill = getPalette()[2])+
      geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill = getPalette()[3])+
      geom_polygon(data=get.poly(as.numeric(pos[[1]])-1,as.numeric(pos[[1]])+1,0.2),aes(x,y))+
      geom_text(data=as.data.frame(breaks), size=5, fontface="bold", vjust=0,
                aes(x=1.1*cos(pi*(1-breaks/100)),y=1.1*sin(pi*(1-breaks/100)),label=paste0(breaks,"%")))+
      annotate("text",x=0,y=0,label=as.character(pos[[2]]),vjust=0,size=8,fontface="bold")+
      coord_fixed()+
      theme_bw()+
      theme(axis.text=element_blank(),
            axis.title=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank()) +
      labs(title = titleLabel, subtitle = subtitle, caption = caption)
    return(graph)
  }

  f <- fringe(data)
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(order = 1:nrow(.), prop = count/(sum(count)))


  newList <- mapply(c, round(as.numeric(data_graph$prop)*100, 1),
                    data_graph$a, SIMPLIFY=FALSE)

  graphList <- lapply(newList, gg.gauge)
  grid.newpage()
  grid.draw(arrangeGrob(grobs = graphList,ncol= ncol))
}

#' gg_gauge_dial_Ca.
#' Gauge
#' @name gg_gauge_dial_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_gauge_dial_Ca. <- function(data, ...){

  gg.gauge <- function(pos, breaks=c(0,50,100)) {
    require(ggplot2)
    get.poly <- function(a,b,r1=0.5,r2=1.0) {
      th.start <- pi*(1-a/100)
      th.end   <- pi*(1-b/100)
      th       <- seq(th.start,th.end,length=100)
      x        <- c(r1*cos(th),rev(r2*cos(th)))
      y        <- c(r1*sin(th),rev(r2*sin(th)))
      return(data.frame(x,y))
    }
    graph <- ggplot()+
      geom_polygon(data=get.poly(breaks[1],as.numeric(pos[[1]])),aes(x,y),fill = getPalette()[1])+
      geom_polygon(data=get.poly(as.numeric(pos[[1]]),breaks[3]),aes(x,y),fill = getPalette()[2])+
      annotate("text",x=0,y=0,label=as.character(pos[[2]]),vjust=0,size=8,fontface="bold")+
      coord_fixed()+
      theme_bw()+
      theme(axis.text=element_blank(),
            axis.title=element_blank(),
            axis.ticks=element_blank(),
            panel.grid=element_blank(),
            panel.border=element_blank())
    return(graph)
  }

  f <- fringe(data)
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count)) %>%
    dplyr::mutate(order = 1:nrow(.), prop = count/(sum(count)))


  newList <- mapply(c, round(as.numeric(data_graph$prop)*100, 1),
                    data_graph$a, SIMPLIFY=FALSE)

  graphList <- lapply(newList, gg.gauge)
  grid.newpage()
  grid.draw(arrangeGrob(grobs = graphList,ncol=2))
}

#' gg_bar_single_stacked_hor_Ca.
#' Single Horizontal Stacked Bar
#' @name gg_bar_single_stacked_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_single_stacked_hor_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                          fillLabel = NULL, leg_pos="right", width = 0.3, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>% dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(pos = cumsum(count) - count/2,
                  percent = 100 * round(count/sum(count), 4))

  graph <- ggplot(data = data_graph, aes(x = factor(1), y = count, fill = a)) +
    geom_bar(stat = "identity", width = width) +
    geom_text(data = data_graph, aes(y = pos, label = paste(percent, "%", sep = "")))
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = fillLabel, subtitle = subtitle, caption = caption)
  graph <- graph + theme_ds() + theme_ds_clean() + scale_fill_manual(values = getPalette())
  graph <- graph + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_bar_single_stacked_ver_Ca.
#' Single Vertical Stacked Bar
#' @name gg_bar_single_stacked_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_single_stacked_ver_Ca. <- function(data, titleLabel = "", subtitle = "", caption = "",
                                   fillLabel = NULL, leg_pos="right", width = 0.3, ...){

  graph <- gg_bar_single_stacked_hor_Ca.(data, titleLabel, fillLabel, leg_pos, width, subtitle, caption)
  graph <- graph + coord_flip()

  return(graph)
}

#' gg_bubble_Ca.
#' Bubble
#' @name gg_bubble_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bubble_Ca.  <- function(data, titleLabel = "", subtitle = "", caption = "", xLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data_graph, aes(x = a, y = 0, size = count, color = ""))
  graph <- graph + geom_point()
  graph <- graph + labs(title = titleLabel, x = xlab, y = "", subtitle = subtitle, caption = caption) +
    scale_color_manual(values = getPalette())

  graph <- graph + theme_ds() + theme(legend.position="none") +
    theme(axis.line=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      panel.grid.major=element_blank())

  return(graph)
}

#' gg_coloured_bubble_Ca.
#' Coloured Bubble
#' @name gg_coloured_bubble_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_coloured_bubble_Ca.  <- function(data, titleLabel = "", xLabel = NULL, fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  flab <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data_graph, aes(x = a, y = 0, size = count))
  graph <- graph + geom_point(aes(color = a))
  graph <- graph + labs(title = titleLabel, x = xlab, y = "", fill = flab)

  graph <- graph + scale_color_manual(values = getPalette())

  graph <- graph + theme_ds() + theme(legend.position="none") +
    theme(axis.line=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major=element_blank())

  return(graph)
}

#' gg_bar_polar_Ca.
#' Polar Bar
#' @name gg_bar_polar_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_polar_Ca. <- function(data, width = 0.95, titleLabel = "",
                             fillLabel = NULL, leg_pos= "right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data=data_graph, aes(a, fill = a, weight = count)) +
            geom_bar(width = width) + coord_polar()
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel)
  graph <- graph + scale_fill_manual(values = getPalette()) + theme_ds() + theme_ds_clean()
  graph <- graph + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_bullseye_Ca.
#' Bullseye
#' @name gg_bullseye_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bullseye_Ca. <- function(data, titleLabel = "", fillLabel = NULL,
                          leg_pos="right", ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data=data, aes(x = factor(1), fill = a)) +
    geom_bar(width = 1) + coord_polar(theta = "x")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel)

  graph <- graph + scale_fill_manual(values = getPalette()) + theme_ds() + theme_ds_clean()
  graph <- graph + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_bar_circular_Ca.
#' Circular Bar
#' @name gg_bar_circular_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bar_circular_Ca. <- function(data, titleLabel = "", fillLabel = NULL,
                                leg_pos="right", width = 0.85, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data_graph, aes(x = a, y = count , fill = a )) +
    geom_bar(width = width, stat="identity") + coord_polar(theta = "y")

  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel)
  graph <- graph + scale_fill_manual(values = getPalette()) + theme_ds() + theme_ds_clean()
  graph <- graph + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_treemap_Ca.
#' Treemap fill by first Ca
#' @name gg_treemap_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_treemap_Ca. <- function(data, titleLabel = "", fillLabel = NULL, ...){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  data_graph$a <- as.factor(data_graph$a)

  graph <- ggplotify(treemapify(data_graph, area = "count", fill = 'a', group = "a"),
                     group.label.colour = "black") + guides(fill=FALSE) +
    scale_fill_manual(values = getPalette()) + labs(title = titleLabel)

  return(graph)
}


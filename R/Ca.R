
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
gg_waffle_Ca. <- function(data, filas = 5, titulo = "Report"){
  f <- fringe(data)
  data <- f$d
  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  parts <- as.vector(data_graph$count)
  graph <- waffle(parts, rows = filas, title = titulo)

  return(graph)
}

#' gg_coloured_bar_ver_Ca.
#' Vertical coloured bars
#' @name gg_coloured_bar_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_coloured_bar_ver_Ca. <- function(data, titleLabel = "Report", xLabel = NULL,
                            yLabel = 'Count', fillLabel = NULL,
                            leg_pos = "right"){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data = data, aes(x = a, fill = factor(a))) + geom_bar()
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel,  fill = flabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)
  return(graph)
}

#' gg_coloured_bar_hor_Ca.
#' Horizontal coloured Bars
#' @name gg_coloured_bar_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_coloured_bar_hor_Ca. <- function(data, titleLabel = "Report", xLabel = NULL,
                            yLabel = 'Count', fillLabel = NULL,
                            leg_pos = "right"){

  graph <- gg_coloured_bar_ver_Ca.(data, titleLabel, xLabel,
                                   yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()
  return(graph)
}

#' gg_coloured_parameter_bar_ver_Ca.
#' Vertical coloured by parameter bars
#' @name gg_coloured_parameter_bar_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_coloured_parameter_bar_ver_Ca. <- function(data, titleLabel = "Report", xLabel = NULL,
                                              yLabel = 'Count', parameter = NULL,
                                              leg_pos = "right"){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  p <-  parameter %||% sample(unique(data[,nms[1]]), 1)
  data <- f$d
  data_graph <- data %>% dplyr::group_by(a) %>% dplyr::summarise(count = n())
  graph <- ggplot(data_graph, aes(x = a, y = count)) +
    geom_bar(stat="identity", aes(fill = a == p ))
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel)
  graph <- graph + scale_fill_manual(values = c('red', 'black') ) + guides(fill=FALSE)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)
  return(graph)
}

#' gg_coloured_parameter_bar_hor_Ca.
#' Horizontal coloured by parameter Bars
#' @name gg_coloured_parameter_bar_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_coloured_parameter_bar_hor_Ca. <- function(data, titleLabel = "Report", xLabel = NULL,
                                              yLabel = 'Count', parameter = NULL,
                                              leg_pos = "right"){

  graph <- gg_coloured_parameter_bar_ver_Ca.(data, titleLabel, xLabel,
                                             yLabel, parameter, leg_pos)

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
gg_bar_ver_Ca. <- function(data, titleLabel = "Report", xLabel = NULL,
                                    yLabel = 'Count', leg_pos = "right"){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data = data, aes(x = factor(a))) + geom_bar()
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)
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
gg_bar_hor_Ca. <- function(data, titleLabel = "Report", xLabel = NULL,
                                    yLabel = 'Count', leg_pos = "right"){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  graph <- gg_bar_ver_Ca.(data, titleLabel, xlab, yLabel, leg_pos)

  graph <- graph + coord_flip()
  return(graph)
}

#' gg_ordered_bar_ver_Ca.
#' Ordered vertical Bars
#' @name gg_ordered_bar_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_ordered_bar_ver_Ca. <- function(data, titleLabel = "Report", xLabel = NULL,
                                   yLabel =  'Count', leg_pos = "right"){
  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d
  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  graph <- ggplot(data_graph, aes(x = reorder(a, count), y = count)) +
            geom_bar(stat = "identity")
  graph <- graph + labs(title = titleLabel, x = yLabel, y = xLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_ordered_bar_hor_Ca.
#' Ordered horizontal Bars
#' @name gg_ordered_bar_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_ordered_bar_hor_Ca. <- function(data, titleLabel = "Report", xLabel = NULL,
                                   yLabel =  'Count', leg_pos = "right"){

  graph <- gg_ordered_bar_ver_Ca.(data, titleLabel, xLabel, yLabel, leg_pos)

  graph <- graph + coord_flip()

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
gg_pie_Ca. <- function(data, titleLabel = "Report", fillLabel = NULL, leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data=data, aes(x = factor(1), fill = a)) +
           geom_bar(width = 1) + coord_polar(theta = "y")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
           theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos)

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
gg_donut_Ca. <- function(data, titleLabel = "Report", fillLabel = NULL,
                         width = 0.3, leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d
  graph <- ggplot(data=data, aes(x = factor(1), fill = factor(a))) +
    geom_bar(width = width) + coord_polar(theta = "y")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
    theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos)

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
gg_dot_bar_ver_Ca. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                               fillLabel = NULL, leg_pos = "right"){

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

  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel,  fill = flabel)
  graph <- graph + theme_minimal() + scale_y_continuous(breaks = NULL) +
            theme(legend.position=leg_pos)


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
gg_dot_bar_hor_Ca. <- function(data, titleLabel = "Report", xLabel = NULL, yLabel = 'Count',
                               fillLabel = NULL, leg_pos = "right"){

  graph <- gg_dot_bar_ver_Ca.(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

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
gg_line_hor_Ca. <- function(data, titleLabel = 'Report', xLabel = NULL,
                            yLabel = 'Count', leg_pos = "right"){

  f <- fringe(data)
  nms <- getCnames(f)
  xlab <- xLabel %||% nms[1]
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(order = c(1:nrow(.)))

  graph <- ggplot(data = data_graph, aes(x = a, y = count, group=1)) + geom_line()
  graph <- graph + labs(title = titleLabel, x = xlab, y = yLabel)

  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

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
gg_line_ver_Ca. <- function(data, titleLabel = 'Report', xLabel = NULL,
                            yLabel = 'Count', leg_pos = "right"){

  graph <- gg_line_hor_Ca.(data, titleLabel, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

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
gg_line_point_hor_Ca. <- function(data, titleLabel = 'Report', xLabel = NULL,
                                  yLabel = 'Count', leg_pos = "right"){

  graph <- gg_line_hor_Ca.(data, titleLabel, xLabel, yLabel, leg_pos)
  graph <- graph + geom_point()

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
gg_line_point_ver_Ca. <- function(data, titleLabel = 'Report', xLabel = NULL,
                                  yLabel = 'Count', leg_pos = "right"){

  graph <- gg_line_ver_Ca.(data, titleLabel, xLabel, yLabel, leg_pos)
  graph <- graph + geom_point()

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
gg_gauge_Ca. <- function(data){

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
      geom_polygon(data=get.poly(breaks[1],breaks[2]),aes(x,y),fill="red")+
      geom_polygon(data=get.poly(breaks[2],breaks[3]),aes(x,y),fill="gold")+
      geom_polygon(data=get.poly(breaks[3],breaks[4]),aes(x,y),fill="forestgreen")+
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
gg_gauge_dial_Ca. <- function(data){

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
      geom_polygon(data=get.poly(breaks[1],as.numeric(pos[[1]])),aes(x,y),fill="skyblue")+
      geom_polygon(data=get.poly(as.numeric(pos[[1]]),breaks[3]),aes(x,y),fill="gold")+
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

#' gg_single_stacked_bar_hor_Ca.
#' Single Horizontal Stacked Bar
#' @name gg_single_stacked_bar_hor_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_single_stacked_bar_hor_Ca. <- function(data, titleLabel = "Report", yLabel = "Count",
                                          fillLabel = "Types", leg_pos="right", width = 0.3){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data=data, aes(x = factor(1), fill = a)) + geom_bar(width = width)
  graph <- graph + labs(title = titleLabel, x = "", y = yLabel, fill = fillLabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
            theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos)

  return(graph)
}

#' gg_single_stacked_bar_ver_Ca.
#' Single Vertical Stacked Bar
#' @name gg_single_stacked_bar_ver_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_single_stacked_bar_ver_Ca. <- function(data, titleLabel = "Report", yLabel = "Frequency",
                                   fillLabel = "Types", leg_pos="right", width = 0.3){

  graph <- gg_single_stacked_bar_hor_Ca.(data, titleLabel, yLabel, fillLabel, leg_pos, width)
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
gg_bubble_Ca.  <- function(data, titleLabel = "Report", xLabel = "Categories",
                           yLabel = "Count"){

  f <- fringe(data)
  data <- f$d

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data_graph, aes(x = a, y = 0, size = count))
  graph <- graph + geom_point()
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel)

  graph <- graph + theme_minimal() + theme(legend.position="none")

  return(graph)
}

#' gg_polar_bar_Ca.
#' Polar Bar
#' @name gg_polar_bar_Ca.
#' @param x A number.
#' @param y A number.
#' @export
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_polar_bar_Ca. <- function(data, width = 0.95, titleLabel = "Report",
                             fillLabel = NULL, leg_pos= "right"){

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
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
            theme(panel.grid=element_blank())
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
gg_bullseye_Ca. <- function(data, titleLabel = "Report", fillLabel = NULL,
                          leg_pos="right"){

  f <- fringe(data)
  nms <- getCnames(f)
  flabel <- fillLabel %||% nms[1]
  data <- f$d

  graph <- ggplot(data=data, aes(x = factor(1), fill = a)) +
    geom_bar(width = 1) + coord_polar(theta = "x")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = flabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
    theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos)

  return(graph)
}

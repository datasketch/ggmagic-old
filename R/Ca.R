
#' gg_waffle.: title.
#' Hola esta es la descripción
#' Tiene múltiples líneas
#' @name gg_waffle.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_waffle. <- function(data, filas = 5, titulo = "Report"){
  f <- fringe(data)
  data <- f$d
  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  parts <- as.vector(data_graph$count)
  print(parts)
  graph <- waffle(parts, rows = filas, title = titulo)

  return(graph)
}

#' gg_bars_ver.
#'
#' Hola esta es la descripción
#' Tiene múltiples líneas
#' @name gg_bars_ver.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bars_ver. <- function(data, titleLabel = "Report", xLabel = "Frequency",
                              yLabel =  "Categories", fillLabel = "Types",
                              leg_pos = "right"){
  f <- fringe(data)
  data <- f$d
  graph <- ggplot(data = data, aes(x = factor(a), fill = factor(a))) + geom_bar()
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel,  fill = fillLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)
  return(graph)
}

#' gg_bars_ver_ord.
#'
#' Hola esta es la descripción
#' Tiene múltiples líneas
#' @name gg_bars_ver_ord.
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @section ftypes: Ca,Ca-Nu
#' @examples
#' add(1, 1)
#' add(10, 1)
gg_bars_ver_ord. <- function(data, titleLabel = "Report", xLabel = "Frequency",
                                      yLabel =  "Categories", fillLabel = "Types",
                                      leg_pos = "right"){
  f <- fringe(data)
  data <- f$d
  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n())

  graph <- ggplot(data_graph, aes(x = factor(reorder(a, count)), y = factor(count),
                                  fill = a)) +

            geom_bar(stat = "identity")

  graph <- graph + labs(title = titleLabel, x = yLabel, y = xLabel,  fill = fillLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

ordered_horizontal_bargraph <- function(data, titleLabel = "Report", xLabel = "Frequency",
                                      yLabel =  "Categories", fillLabel = "Types",
                                      leg_pos = "right"){
  f <- fringe(data)
  data <- f$data
  graph <- ordered_vertical_bargraph(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

horizontal_bargraph <- function(data, titleLabel = "Report", xLabel = "Frequency",
                                yLabel =  "Categories", fillLabel = "Types",
                                leg_pos="top"){
  f <- fringe(data)
  data <- f$data
  graph <- vertical_bargraph(data, titleLabel, xLabel,
                             yLabel, fillLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}



#Pie chart (falta agregar labels)
#Restricciones: no m?s de 8 categor?as.
piegraph <- function(data, titleLabel = "Report", fillLabel = "Types", leg_pos="right"){
  f <- fringe(data)
  data <- f$data
  graph <- ggplot(data=data, aes(x = factor(1), fill = factor(a))) +
           geom_bar(width = 1) + coord_polar(theta = "y")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = fillLabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
           theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos)

  return(graph)
}

#Width debe de ser un parámetro.  0 < width < 1.
#Si es 0 no sale nada. Si es 1 es un pie chart.
donutgraph <- function(data, width = 0.5, titleLabel = "Report",
                       fillLabel = "Types", leg_pos= "right"){
  f <- fringe(data)
  data <- f$data
  graph <- ggplot(data=data, aes(x = factor(1), fill = factor(a))) +
    geom_bar(width = width) + coord_polar(theta = "y")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = fillLabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
    theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos)

  return(graph)
}

vertical_dotgraph <- function(data, titleLabel = "Report", xLabel = "Categories", yLabel = "Frequency",
                     fillLabel = "Types", leg_pos = "right"){

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  data_graph <- data_graph %>%
                mutate(order = c(1:nrow(data_graph)))

  graph <- ggplot(data = merge(x = dataCat, y = data_graph, by = "a", all.x = TRUE),
                  aes(x = order, fill = factor(a))) + geom_dotplot(method="histodot")

  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel,  fill = fillLabel)
  graph <- graph + theme_minimal() + scale_y_continuous(breaks = NULL) +
            theme(legend.position=leg_pos)


  return(graph)
}

horizontal_dotgraph <- function(data, titleLabel = "Report", xLabel = "Categories", yLabel = "Frequency",
                                fillLabel = "Types", leg_pos = "top"){

  graph <- vertical_dotgraph(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

horizontal_linegraph <- function(data, titleLabel = "Report", xLabel = "Types",
                                 yLabel = "Frequency", leg_pos = "right"){

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  data_graph <- data_graph %>%
    dplyr::mutate(order = c(1:nrow(data_graph)))

  graph <- ggplot(data = data_graph, aes(x = a, y = count, group=1)) + geom_line() +
            geom_point()
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel)

  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

vertical_linegraph <- function(data, titleLabel = "Report", xLabel = "Types",
                               yLabel = "Frequency", leg_pos = "top"){

  graph <- horizontal_linegraph(data, titleLabel, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

#Gauge chart
#Restricciones: ninguna hasta el momento.
gaugeGraph <- function(info){
  # Based on: http://stackoverflow.com/questions/24900903/how-to-draw-gauge-chart-in-r
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
    ggplot()+
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
  }



  filter1 <- group_by(dataCat, a)
  a <- summarise(filter1, count = n())
  a <- arrange(a, desc(count))
  a$order <- c(1:nrow(a))
  inputData <- merge(x = dataCat, y = a, by = "a", all.x = TRUE)
  a$prop <- a$count/sum(a$count)

  newList <- mapply(c, round(as.numeric(a$prop)*100, 1), a$a, SIMPLIFY=FALSE)
  graphList <- lapply(newList, gg.gauge)
  grid.newpage()
  grid.draw(arrangeGrob(grobs = graphList,ncol=2))

}

horizontal_barStackGraph <- function(data, titleLabel = "Report", yLabel = "Frequency",
                                     fillLabel = "Types", leg_pos="right"){

  graph <- ggplot(data=data, aes(x = factor(1), fill = factor(a))) + geom_bar(width = 1)
  graph <- graph + labs(title = titleLabel, x = "", y = yLabel, fill = fillLabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
            theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos)

  return(graph)
}

vertical_barStackGraph <- function(data, titleLabel = "Report", yLabel = "Frequency",
                                   fillLabel = "Types", leg_pos="right"){

  graph <- horizontal_barStackGraph(data, titleLabel, yLabel, fillLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

horizontal_circleAreaPlot  <- function(data, titleLabel = "Report", xLabel = "Categories",
                                       yLabel = "Frequency", fillLabel = "Count",
                                       leg_pos="right"){

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data_graph, aes(x=factor(a), y=0, size=count))
  graph <- graph + geom_point()
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel, fill = fillLabel)

  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

vertical_circleAreaPlot  <- function(data, titleLabel = "Report", xLabel = "Categories",
                                     yLabel = "Frequency", fillLabel = "Count",
                                     leg_pos="right"){

  graph <- horizontal_circleAreaPlot(data, titleLabel, xLabel, yLabel,
                                     fillLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}


#Bar Polar Plot
# Width es un parámetro. 0 < width < 1.
# Si es 0, no muestra nada. Si es 1 es parecido al pie chart.
barPolarGraph <- function(data, width = 0.95, titleLabel = "Report",
                          fillLabel = "Types", leg_pos= "right"){

  data_graph <- data %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::arrange(desc(count))

  graph <- ggplot(data=data_graph, aes(a, fill = factor(a), weight = count)) +
            geom_bar(width = width) + coord_polar()
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = fillLabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
            theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos)

  return(graph)
}

bullsEyeGraph <- function(data, titleLabel = "Report", fillLabel = "Types",
                          leg_pos="right"){

  graph <- ggplot(data=data, aes(x = factor(1), fill = factor(a))) +
    geom_bar(width = 1) + coord_polar(theta = "x")
  graph <- graph + labs(title = titleLabel, x = "", y = "", fill = fillLabel)
  graph <- graph + theme_minimal() + theme(axis.text=element_blank()) +
    theme(panel.grid=element_blank())
  graph <- graph + theme(legend.position=leg_pos)

  return(graph)
}

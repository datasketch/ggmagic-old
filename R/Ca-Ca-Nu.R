library(reshape2)
library(ggplot2)
library(waffle)
library(extrafont)
library(dplyr)
library(grid)
library(gridExtra)
library(RColorBrewer)

circleAreaPlotCCN  <- function(data, titleLabel = "Report", xLabel = "Category",
                              yLabel = "Category", leg_pos = "right"){

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(sum = sum(c)) %>%
    dplyr::arrange(desc(sum))
  graph <- ggplot(data_graph, aes(x=factor(a), y=factor(b), size=sum))
  graph <- graph + geom_point()
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

flip_circleAreaPlotCCN  <- function(data, titleLabel = "Report", xLabel = "Category",
                                   yLabel = "Category", leg_pos = "top"){

  graph <- circleAreaPlotCCN(data, titleLabel, xLabel, yLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

vertical_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Category",
                                yLabel = "Frequency", fillLabel = "Types", leg_pos = "top"){

  graph <- ggplot(data, aes(a, fill=b, weights = c)) + geom_bar()
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel, fill=fillLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

ordered_vertical_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Frequency",
                                        yLabel =  "Categories", fillLabel = "Types",
                                        leg_pos = "right"){

  graph <- ggplot(data, aes(x=reorder(data$b,rep(1,length(data$b)),sum),fill=a, weights = c)) +
    geom_bar()

  graph <- graph + labs(title = titleLabel, x = yLabel, y = xLabel,  fill = fillLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

ordered_horizontal_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Frequency",
                                          yLabel =  "Categories", fillLabel = "Types",
                                          leg_pos = "right"){

  graph <- ordered_vertical_bargraphCCN(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

horizontal_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Category",
                                  yLabel = "Category", fillLabel = "Types", leg_pos = "top"){

  graph <- vertical_bargraphCCN(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

vertical_dotgraphCCN <- function(data, titleLabel = "Report", xLabel = "Categories", yLabel = "Frequency",
                                fillLabel = "Types", leg_pos = "right"){

  graph <- ggplot(data = data, aes(a, fill = factor(b), weights = c)) +
    geom_dotplot(stackgroups = TRUE, binpositions = "all")

  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel,  fill = fillLabel)
  graph <- graph + theme_minimal() + scale_y_continuous(breaks = NULL) +
    theme(legend.position=leg_pos)

  return(graph)
}

horizontal_dotgraphCCN <- function(data, titleLabel = "Report", xLabel = "Categories", yLabel = "Frequency",
                                  fillLabel = "Types", leg_pos = "top"){

  graph <- vertical_dotgraphCCN(data, titleLabel, xLabel, yLabel, fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}

vertical_unstacked_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Category",
                                          yLabel = "Frequency", fillLabel = "Types",
                                          leg_pos = "top"){

  graph <- ggplot(data, aes(a, weights = c)) + geom_bar(aes(fill=data$b), position = "dodge")
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel, fill=fillLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

horizontal_unstacked_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Category",
                                            yLabel = "Frequency", fillLabel = "Types",
                                            leg_pos = "top"){
  graph <- vertical_unstacked_bargraphCCN(data, titleLabel, xLabel, yLabel,
                                         fillLabel, leg_pos)

  graph <- graph + coord_flip()

  return(graph)
}


horizontal_linegraphCCN <- function(data, titleLabel = "Report", xLabel = "Types",
                                   yLabel = "Frequency"){

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(sum = sum(c)) %>%
    dplyr::arrange(desc(sum))

  graph <- ggplot(data = data_graph, aes(x = a, y = sum, group=b)) + geom_line() +
    geom_point() + facet_grid(. ~b)
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel)

  graph <- graph + theme_minimal()

  return(graph)
}

vertical_linegraphCCN <- function(data, titleLabel = "Report", xLabel = "Types",
                                 yLabel = "Frequency"){

  graph <- horizontal_linegraphCCN(data, titleLabel, xLabel, yLabel)
  graph <- graph + coord_flip()

  return(graph)
}

vertical_stacked_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Category",
                                        yLabel = "Frequency", fillLabel = "Types",
                                        leg_pos = "top"){

  graph <- ggplot(data, aes(a, fill=b, weights = c)) + geom_bar(position = "fill")
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel, fill=fillLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

horizontal_stacked_bargraphCCN <- function(data, titleLabel = "Report", xLabel = "Category",
                                          yLabel = "Frequency", fillLabel = "Types",
                                          leg_pos = "top"){


  graph <- vertical_stacked_bargraphCCN(data, titleLabel, xLabel, yLabel,
                                       fillLabel, leg_pos)
  graph <- graph + coord_flip()

  return(graph)
}

horizontal_area_bargraphCC <- function(data, titleLabel = "Report", xLabel = "Category",
                                       yLabel = "Frequency", fillLabel = "Types",
                                       leg_pos = "top"){

  data_graph <- data %>%
    dplyr::group_by(a, b) %>%
    dplyr::summarise(sum = sum(c)) %>%
    dplyr::arrange(desc(sum))
  graph <- ggplot(data = data_graph,
                  aes(x=a, y=sum, group=b)) + geom_area(aes(fill = b), position = "stack")
  graph <- graph + labs(title = titleLabel, x = xLabel, y = yLabel, fill=fillLabel)
  graph <- graph + theme_minimal() + theme(legend.position=leg_pos)

  return(graph)
}

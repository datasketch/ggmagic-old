library(reshape2)
library(ggplot2)
library(waffle)
library(extrafont)
library(dplyr)
library(grid)
library(gridExtra)
library(RColorBrewer)

dens2D_Plot <- function(data, titl="", xLabel="", yLabel="", labelText = ""){

  graph <- ggplot(data, aes(x=a, y=b)) + geom_point() +
    stat_density2d(geom = "tile", aes(fill = ..density..), contour = F) +
    theme_bw() + labs(title = titl, x = xLabel, y = yLabel, fill = labelText)

  return(graph)

}

flip_dens2D_Plot <- function(data, titl="", xLabel="", yLabel="", labelText = ""){

  graph <- dens2D_Plot(data, titl, xLabel, yLabel, labelText)
  graph <- graph + coord_flip()

  return(graph)
}



hist2D_Plot <- function(data, titl="", xLabel="", yLabel="", labelText = ""){

  binNumber <- floor(sqrt(nrow(data)))
  graph <- ggplot(data, aes(x=a, y=b)) + stat_bin2d(bins=binNumber) +
    scale_fill_gradient(low="aliceblue", high="coral2") +
    theme_bw() + labs(title = titl, x = xLabel, y = yLabel, fill = labelText)

  return(graph)
}

flip_hist2D_Plot <- function(data, titl="", xLabel="", yLabel="", labelText = ""){
  graph <- hist2D_Plot(data, titl, xLabel, yLabel, labelText)
  graph <- graph + coord_flip()

  return(graph)
}


mult_Line_Plot <- function(data, titl="", xLabel="", yLabel="", labelText = ""){

  label <- names(data)
  dataNumNum$idx <- seq(nrow(data))
  variables <- labels
  df <- melt(dataNumNum, id.vars = "idx")
  graph <- ggplot(df, aes(x = idx, y = value, colour = variable)) +
    geom_line() + labs(title = titl, x = xLabel, y = yLabel, fill = labelText) + theme_bw()

  return(graph)

}

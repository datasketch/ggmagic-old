library(reshape2)
library(ggplot2)
library(waffle)
library(extrafont)
library(dplyr)
library(grid)
library(gridExtra)
library(RColorBrewer)

dens2dPlot <- function(titl="", xLabel="", yLabel="", labelText = "", voltear=FALSE){
  q <- ggplot(dataNumNum, aes(x=a, y=b)) + geom_point()

  q <- ggplot(dataNumNum, aes(x=a, y=b))
  q <- q  + stat_density2d(geom = "tile", aes(fill = ..density..),contour = F)
  q <- q + scale_fill_gradient(low="coral3", high="snow1")
  q <- q + theme_bw()
  q <- q + labs(title = titl, x = xLabel, y = yLabel, fill = labelText)

  if (voltear){

    q <- q + coord_flip()

  }

  return(q)

}


hist2dPlot <- function(titl="", xLabel="", yLabel="", labelText = "", voltear=FALSE){



  binNumber <- floor(sqrt(nrow(dataNumNum)))
  q <- ggplot(dataNumNum, aes(x=a, y=b))
  q <- q  + stat_bin2d(bins=binNumber)
  q <- q + scale_fill_gradient(low="aliceblue", high="coral2")
  # q <- q  + stat_bin2d() + scale_fill_gradient(low="azure", high="coral2")
  q <- q + theme_bw()
  q <- q + labs(title = titl, x = xLabel, y = yLabel, fill = labelText)
  q


  if (voltear){

    q <- q + coord_flip()

  }


  return(q)

}


multLinePlot <- function(titl="", xLabel="", yLabel="", labelText = "", voltear=FALSE){

  label <- names(dataNumNum)
  dataNumNum$idx <- seq(nrow(dataNumNum))
  colors <- c("#54B4E9","coral2")
  variables <- labels
  df <- melt(dataNumNum, id.vars = "idx")
  print(df)
  q <- ggplot(df, aes(x=idx,y = value, colour = variable))
  q <- q + geom_line()
  q <- q + scale_colour_manual(values = colors, labels = labels)
  q <- q + labs(title = titl, x = xLabel, y = yLabel, fill = labelText)
  q <- q + theme_bw()
  return(q)

}

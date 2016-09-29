#barras en coordenadas polares - por variable categÃ³rica

library(ggplot2)
library(waffle)
library(extrafont)
library(dplyr)
library(plyr)
library(grid)
library(gridExtra)
library(RColorBrewer)

flowerGraph <- function(data, titleLabel = "", fillLabel = ""){
  graph <- ggplot(data = data, aes(x = b, weight = b, fill = a)) + geom_bar() +
    coord_polar() + ylab("Total Runs")
  graph <- graph + labs(title = titleLabel, fill = fillLabel, x = "", y = "") + theme_bw()

  return(graph)

}

#barras en coordenadas polares - por variable numÃ©rica

flowerNumGraph <- function(data, titleLabel = "", fillLabel = ""){
  graph <- ggplot(data = data, aes(x = a, weight = b, fill = a)) + geom_bar() +
    coord_polar() + ylab("Total Runs")
  graph <- graph + labs(title = titleLabel, fill = fillLabel, x = "", y = "") + theme_bw()
  return(graph)
}


#barras stacked
barStackedGraph <- function(data, titleLabel = "", xLabel = "", yLabel = "", fillLabel = "", voltear = TRUE){
  hist <- ggplot(data, aes(b))
  hist <- hist + geom_histogram(aes(fill = a), binwidth = 10) + geom_density() + labs(title = titleLabel, x = xLabel, y = yLabel, fill = fillLabel) + theme_bw()
  if(voltear){
    hist <- hist + coord_flip()

  }



  return(hist)
}


#multiple density, single plot
multDensSingPlot <- function(data, titleLabel = "", xLabel = "", yLabel = "", fillLabel = "", voltear = TRUE){
  density <- ggplot(data, aes(b))
  density <- density + geom_density(aes(colour = a)) + labs(title = titleLabel, x = xLabel, y = yLabel, fill = fillLabel) + theme_bw()

  if(voltear){
    density <- density + coord_flip()

  }


  return(density)
}

#multiple density, split plots
multDensSpltPlot <- function(data, titleLabel = "", xLabel = "", yLabel = "", fillLabel = "", voltear = TRUE){
  density <- ggplot(data, aes(b))
  density <- density + geom_density(aes(colour = a)) + labs(title = titleLabel, x = xLabel, y = yLabel, fill = fillLabel) + theme_bw()
  density <- density + facet_grid(a ~ ., scales = "free")


  if(voltear){
    density <- density + coord_flip()

  }
  return(density)
}

#multiple histogram, split plots
densHistSpltPlot <- function(data, titleLabel = "", xLabel = "", yLabel = "", fillLabel = "", voltear = TRUE){
  a <- ggplot(data, aes(x = b)) + geom_histogram(binwidth = 0.5, colour = "black",
                                                       fill = "white") + facet_grid(a ~ .) + geom_vline(data = data,
                                                                                                        aes(xintercept = mean(b)), linetype = "dashed", size = 1, colour = "blue")
  a <- a + labs(title = titleLabel, x = xLabel, y = yLabel, fill = fillLabel) + theme_bw()


  if(voltear){
    a <- a + coord_flip()

  }

  return(a)
}



#boxplots
boxSpltPlot <- function(data, titleLabel = "", xLabel = "", yLabel = "", fillLabel = "", voltear = TRUE){
  boxplot <- ggplot(data, mapping = aes(x = a, y = b, fill = a))
  boxPlot <- boxplot + geom_boxplot() + theme_bw()+ labs(title = titleLabel, x = xLabel, y = yLabel, fill = fillLabel)
  if(voltear){
    boxPlot <- boxPlot + coord_flip()

  }


  return(boxPlot)
}

#violin plots

ViolinMultPlot <- function(data, titleLabel = "", xLabel = "", yLabel = "", fillLabel = "", voltear = TRUE){
  violin <- ggplot(data, mapping = aes(x = a, y = b, fill = a))
  violin <- violin + geom_violin() + theme_bw()+ labs(title = titleLabel, x = xLabel, y = yLabel, fill = fillLabel)


  if(voltear){
    violin <- violin + coord_flip()

  }
  return(violin)
}

#violin plots + obs dots

ViolinDotMultPlot <- function(data, titleLabel = "", xLabel = "", yLabel = "", fillLabel = "", voltear = TRUE){
  violin <- ggplot(data, mapping = aes(x = a, y = b, fill = a))
  violin <- violin  + geom_jitter(aes(alpha = 1), height = 1, size = 1) + geom_violin() + theme_bw()+ labs(title = titleLabel, x = xLabel, y = yLabel, fill = fillLabel)

  if(voltear){
    violin <- violin + coord_flip()

  }
  return(violin)
}



dataNumNum <- data.frame(runif(100, min = 0, max = 50),
                         runif(100, min = 0, max = 50))

names(dataNumNum) <- c("a", "b")

#density - 2D
dens2dPlot ()

#histogram - 2D
hist2dPlot ()



#MultiLine plot

multLinePlot ()

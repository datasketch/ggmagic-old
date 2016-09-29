
dataNumNum <- data.frame(runif(100, min = 0, max = 50),
                         runif(100, min = 0, max = 50))

names(dataNumNum) <- c("a", "b")

#density - 2D
dens2D_Plot(dataNumNum)
flip_dens2D_Plot(dataNumNum)

#histogram - 2D
hist2D_Plot(dataNumNum)
flip_hist2D_Plot(dataNumNum)



#MultiLine plot

mult_Line_Plot(dataNumNum)

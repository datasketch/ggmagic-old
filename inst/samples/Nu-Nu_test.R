load_all()

dataNuNu <- data.frame(x = 1:100, y = cumsum(rnorm(100)))

names(dataNuNu) <- c("a", "b")

#density - 2D
dens2D_Plot(dataNuNu)
flip_dens2D_Plot(dataNuNu)

#histogram - 2D
hist2D_Plot(dataNuNu)
flip_hist2D_Plot(dataNuNu)

# Waterfall
gg_waterfall_NuNu.(dataNuNu)

# Horizon
gg_horizon_NuNu.(dataNuNu)

#MultiLine plot
mult_Line_Plot(dataNuNu)

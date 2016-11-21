load_all()

dataNuNu <- data.frame(x = 1:100, y = cumsum(rnorm(100)))

names(dataNuNu) <- c("a", "b")

#density - 2D
gg_dens2D_NuNu.(dataNuNu)
gg_flip_dens2D_NuNu.(dataNuNu)

#histogram - 2D
gg_hist2D_NuNu.(dataNuNu)
gg_flip_hist2D_NuNu.(dataNuNu)

# Waterfall
gg_waterfall_NuNu.(dataNuNu)

# Horizon
gg_horizon_NuNu.(dataNuNu)

#MultiLine plot
gg_mult_line_NuNu.(dataNuNu)

gg_scatter_NuNu.(dataNuNu)

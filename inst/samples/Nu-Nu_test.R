library(devtools)
load_all()
document()

dataNuNu <- data.frame(x = 1:100, y = cumsum(rnorm(100)))

names(dataNuNu) <- c("agsrhg", "bhtgstr")

#density - 2D
gg_dens_NuNu.(dataNuNu)
gg_dens_flip_NuNu.(dataNuNu)

#histogram - 2D
gg_hist_NuNu.(dataNuNu)
gg_hist_flip_NuNu.(dataNuNu)

# Waterfall
gg_waterfall_NuNu.(dataNuNu)

# Horizon
gg_horizon_NuNu.(dataNuNu)

#MultiLine plot
gg_line_multi_NuNu.(dataNuNu)

gg_point_NuNu.(dataNuNu, size = 1)
gg_line_point_NuNu.(dataNuNu, size = 1, shape = 6)


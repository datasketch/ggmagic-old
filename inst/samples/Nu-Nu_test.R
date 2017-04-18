library(devtools)
load_all()
document()

dataNumNum <- data.frame(x = 1:100, y = cumsum(rnorm(100)))

names(dataNumNum) <- c("agsrhg", "bhtgstr")

#density - 2D
gg_dens_NumNum.(dataNumNum)
gg_dens_flip_NumNum.(dataNumNum)

#histogram - 2D
gg_hist_NumNum.(dataNumNum)
gg_hist_flip_NumNum.(dataNumNum)

# Waterfall
gg_waterfall_NumNum.(dataNumNum)

# Horizon
gg_horizon_NumNum.(dataNumNum)

#MultiLine plot
gg_line_multi_NumNum.(dataNumNum)

gg_point_NumNum.(dataNumNum, size = 1)
gg_line_point_NumNum.(dataNumNum, size = 1, shape = 6)


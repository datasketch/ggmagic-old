# Ye-Nu

library(devtools)
load_all()
document()
install()

library(ggmagic)
data <- data.frame(año = 2001:2016, datos = rnorm(16))

gg_lines_hor_YeNu.(data)

gg_lollipop_YeNu.(data, size = 2)

gg_waterfall_YeNu.(data)

gg_bar_coloured_x_ver_YeNu.(data)

gg_area_YeNu.(data)

gg_horizon_YeNu.(data)

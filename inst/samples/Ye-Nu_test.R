# Yea-Num

library(devtools)
load_all()
document()
install()

library(ggmagic)
data <- data.frame(año = 2001:2016, datos = rnorm(16))

gg_line_hor_YeaNum.(data)

gg_lollipop_YeaNum.(data, size = 2)

gg_waterfall_YeaNum.(data)

gg_bar_coloured_x_ver_YeaNum.(data)

gg_bar_ver_YeaNum.(data)
gg_bar_hor_YeaNum.(data)

gg_area_YeaNum.(data)

gg_horizon_YeaNum.(data)

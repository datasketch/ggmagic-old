# Ye-Nu

library(devtools)
load_all()
document()
install()

library(ggmagic)
data <- data.frame(año = 2001:2016, datos = rnorm(16))

gg_lines_hor_YeNu.(data)

gg_lollipop_YeNu.(data)


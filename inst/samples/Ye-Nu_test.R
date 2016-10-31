# Ye-Nu

library(devtools)
load_all()
document()
install()

library(ggmagic)
data <- sampleData("YeNu")

gg_lines_hor_YeNu.(data)

gg_lollipop_YeNu.(data)


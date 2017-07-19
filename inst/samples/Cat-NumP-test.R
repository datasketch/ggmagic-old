library(devtools)
load_all()
document()
install()
library(ggmagic)


data <- sampleData("Cat-NumP", nrow = 4, gt0 = TRUE)
data
gg_radar_CatNumP.(data, rescale = TRUE)



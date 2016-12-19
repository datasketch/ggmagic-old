library(devtools)
load_all()
document()
install()
library(ggmagic)


data <- sampleData("Ca-NuP", nrow = 4, gt0 = TRUE)
data
gg_radar_CaNuP.(data, rescale = TRUE)



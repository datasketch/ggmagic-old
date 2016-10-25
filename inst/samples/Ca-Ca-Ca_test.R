library(devtools)
load_all()
library(ggmagic)

data("G20")
dataCaCaCa <- G20 %>% select(Region, Country, Economic.classification)

# Treemap
gg_treemap_x_CaCaCa.(dataCaCaCa)
gg_treemap_y_CaCaCa.(dataCaCaCa)
gg_treemap_z_CaCaCa.(dataCaCaCa)

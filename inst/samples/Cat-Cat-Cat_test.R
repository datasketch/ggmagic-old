library(devtools)
load_all()
library(ggmagic)
document()
install()

data("G20")
dataCatCatCat <- G20 %>% select(Region, Country, Econ.classification)

# Treemap
gg_treemap_x_CatCatCat.(dataCatCatCat)
gg_treemap_y_CatCatCat.(dataCatCatCat)
gg_treemap_z_CatCatCat.(dataCatCatCat)

# Point
gg_point_CatCatCat.(dataCatCatCat)

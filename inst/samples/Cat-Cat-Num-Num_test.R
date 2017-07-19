library(devtools)
load_all()
document()
install()
library(ggmagic)

data("G20")
dataCatCatNumNum <- G20 %>% select(Region, Country, GDP.mil.USD, HDI)

# Treemap
gg_treemap_density_x_CatCatNumNum.(dataCatCatNumNum, reverse = TRUE)
gg_treemap_density_y_CatCatNumNum.(dataCatCatNumNum)

library(devtools)
load_all()
document()
install()
library(ggmagic)

data("G20")
dataCaCaCaNu <- G20 %>% select(Region, Country, Economic.classification, HDI)

# Treemap
gg_treemap_x_CaCaCaNu.(dataCaCaCaNu)
gg_treemap_y_CaCaCaNu.(dataCaCaCaNu)
gg_treemap_z_CaCaCaNu.(dataCaCaCaNu)

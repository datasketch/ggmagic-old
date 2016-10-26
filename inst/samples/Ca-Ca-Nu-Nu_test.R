library(devtools)
load_all()
document()
install()
library(ggmagic)

data("G20")
dataCaCaNuNu <- G20 %>% select(Region, Country, Nom.GDP.mil.USD, HDI)

# Treemap
gg_treemap_density_x_CaCaNuNu.(dataCaCaNuNu)
gg_treemap_density_y_CaCaNuNu.(dataCaCaNuNu)

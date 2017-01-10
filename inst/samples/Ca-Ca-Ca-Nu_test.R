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
gg_bar_stacked_100_hor_CaCaCaNu.(data = sampleData('Ca-Ca-Ca-Nu',addNA = FALSE))


lev1 <- c(rep('colombia',5), rep('usa',5))
lev2 <- c(rep('amarillo',3),'rojo', 'azul', rep('azul', 3), rep('rojo',2 ))
lev3 <- c(rep('blanco',4),rep(NA,6))
val <- runif(10, min = 130, max = 500)

set.seed(1)
data <- data.frame(lev1, lev2, lev3, val)


gg_sunburst_CaCaCaNu.(data)

library(devtools)
load_all()
document()
install()
library(ggmagic)

data("G20")
dataCatCatCatNum <- G20 %>% select(Region, Country, Econ.classification, HDI)

# Treemap
gg_treemap_x_CatCatCatNum.(dataCatCatCatNum)
gg_treemap_y_CatCatCatNum.(dataCatCatCatNum)
gg_treemap_z_CatCatCatNum.(dataCatCatCatNum)
gg_bar_stacked_100_hor_CatCatCatNum.(data = sampleData('Cat-Cat-Cat-Num',addNA = FALSE))


lev1 <- c(rep('colombia',5), rep('usa',5))
lev2 <- c(rep('amarillo',3),'rojo', 'azul', rep('azul', 3), rep('rojo',2 ))
lev3 <- c(rep('blanco',4),rep(NA,6))
val <- runif(10, min = 130, max = 500)

set.seed(1)
data <- data.frame(lev1, lev2, lev3, val)


gg_sunburst_CatCatCatNum.(data)


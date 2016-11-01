library(devtools)

load_all()
document()
install()
library(ggmagic)


catA <- rep("Callejero", round(runif(1, 10, 20), digits = 0))
catB <- rep("De raza", round(runif(1, 10, 20), digits = 0))
catC <- rep("Mixto", round(runif(1, 10, 20), digits = 0))

catD <- rep("A", length(catA)*0.8)
catE <- rep("B", length(catA)*0.9)
catF <- rep("C", - length(catD) - length(catE) + length(catA) + length(catB) + length(catC))

dataCaCaNu <- data.frame(c(catA, catB, catC), c(catD, catE, catF))
names(dataCaCaNu)[1] <- "RSGSH"; names(dataCaCaNu)[2] <- "GREHSEZGH"
dataCaCaNu[] <- sapply(dataCaCaNu, as.character)


dataCaCaNu$c <- runif(nrow(dataCaCaNu), min = 0, max = 50)

# Coloured Bubble
gg_coloured_x_bubble_CaCaNu.(dataCaCaNu)
gg_coloured_y_bubble_CaCaNu.(dataCaCaNu)

gg_pyramid_CaCaNu.(dataCaCaNu)

# Bubble
gg_bubble_CaCaNu.(dataCaCaNu)

vertical_bargraphCCN(dataCaCaNu)
ordered_vertical_bargraphCCN(dataCaCaNu)
ordered_horizontal_bargraphCCN(dataCaCaNu)
horizontal_bargraphCCN(dataCaCaNu)

vertical_dotgraphCCN(dataCaCaNu)
horizontal_dotgraphCCN(dataCaCaNu)

vertical_unstacked_bargraphCCN(dataCaCaNu)
horizontal_unstacked_bargraphCCN(dataCaCaNu)

horizontal_linegraphCCN(dataCaCaNu)
vertical_linegraphCCN(dataCaCaNu)

# Stacked Bar
gg_bar_stacked_hor_CaCaNu.(dataCaCaNu)
gg_bar_stacked_ver_CaCaNu.(dataCaCaNu)

# Stacked Bar 100%
gg_bar_stacked_100_hor_CaCaNu.(dataCaCaNu)
gg_bar_stacked_100_ver_CaCaNu.(dataCaCaNu)

horizontal_area_bargraphCC(dataCaCaNu)

# Facet Circular
gg_bar_facet_circular_CaCaNu.(dataCaCaNu)

# Facet Pie
gg_facet_pie_CaCaNu.(dataCaCaNu)

# Facet Donut
gg_donut_CaCaNu.(dataCaCaNu)

# Facet Bullseye
gg_bullseye_CaCaNu.(dataCaCaNu)

# Stacked Polar Bar
gg_bar_stacked_polar_CaCaNu.(dataCaCaNu)

# Stacked 100% Polar Bar
gg_bar_stacked_polar_100_CaCaNu.(dataCaCaNu)

# Facet Coloured Bars
gg_bar_facet_coloured_x_ver_CaCaNu.(dataCaCaNu)
gg_bar_facet_coloured_x_hor_CaCaNu.(dataCaCaNu)
gg_bar_facet_coloured_y_ver_CaCaNu.(dataCaCaNu)
gg_bar_facet_coloured_y_hor_CaCaNu.(dataCaCaNu)
gg_bar_facet_coloured_z_ver_CaCaNu.(dataCaCaNu)
gg_bar_facet_coloured_z_hor_CaCaNu.(dataCaCaNu)

# Facet Coloured parameter Bar
gg_bar_facet_coloured_parameter_ver_ver_CaCaNu.(dataCaCaNu,
                                          parameter1 = c("Callejero"),
                                          parameter2 = c("A"))
gg_bar_facet_coloured_parameter_ver_hor_CaCaNu.(dataCaCaNu,
                                          parameter1 = c("Callejero", "De raza", "Mixto"),
                                          parameter2 = c("A","B","C"))

# Treemap
gg_treemap_x_CaCaNu.(dataCaCaNu)
gg_treemap_y_CaCaNu.(dataCaCaNu)
gg_treemap_density_z_CaCaNu.(dataCaCaNu)

# pyramid

catA <- rep("Callejero", round(runif(1, 10, 20), digits = 0))
catB <- rep("De raza", round(runif(1, 10, 20), digits = 0))
catD <- rep("A", length(catA)*0.8)
catE <- rep("B", length(catA)*0.9)
catF <- rep("C", - length(catD) - length(catE) + length(catA) + length(catB))

dataCaCaNu <- data.frame(c(catA, catB), c(catD, catE, catF))
names(dataCaCaNu)[1] <- "RSGSH"; names(dataCaCaNu)[2] <- "GREHSEZGH"
dataCaCaNu[] <- sapply(dataCaCaNu, as.character)

dataCaCaNu$c <- runif(nrow(dataCaCaNu), min = 0, max = 50)
gg_pyramid_CaCaNu.(dataCaCaNu)


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
gg_bubble_coloured_x_CaCaNu.(dataCaCaNu)
gg_bubble_coloured_y_CaCaNu.(dataCaCaNu)

gg_pyramid_CaCaNu.(dataCaCaNu)

# Bubble
gg_bubble_CaCaNu.(dataCaCaNu)

# Unstacked Bar
gg_bar_unstacked_ver_CaCaNu.(dataCaCaNu)
gg_bar_unstacked_hor_CaCaNu.(dataCaCaNu)

# Line
gg_line_hor_facet_CaCaNu.(dataCaCaNu)
gg_line_ver_facet_CaCaNu.(dataCaCaNu)
d <- data.frame(type = c("hola", "hola", "casa", "casa"), x = c("12", "16", "12", "16"), y = c(15,57,124,44))
gg_multi_line_point_CaCaNu.(d, xLabel = "DEf")

# Stacked Bar
gg_bar_stacked_hor_CaCaNu.(dataCaCaNu)
gg_bar_stacked_ver_CaCaNu.(dataCaCaNu)

# Stacked Bar 100%
gg_bar_stacked_100_hor_CaCaNu.(dataCaCaNu)
gg_bar_stacked_100_ver_CaCaNu.(dataCaCaNu)

# Stacked Area
gg_area_stacked_hor_CaCaNu.(dataCaCaNu)
gg_area_stacked_ver_CaCaNu.(dataCaCaNu)

# 100 Stacked Area
gg_area_stacked_100_hor_CaCaNu.(dataCaCaNu)
gg_area_stacked_100_ver_CaCaNu.(dataCaCaNu)

# Facet Circular
gg_bar_circular_facet_CaCaNu.(dataCaCaNu)

# Facet Pie
gg_pie_facet_CaCaNu.(dataCaCaNu)

# Facet Donut
gg_donut_facet_CaCaNu.(dataCaCaNu)

# Facet Bullseye
gg_bullseye_facet_CaCaNu.(dataCaCaNu)

# Stacked Polar Bar
gg_bar_stacked_polar_CaCaNu.(dataCaCaNu)

# Stacked 100% Polar Bar
gg_bar_stacked_polar_100_CaCaNu.(dataCaCaNu)

# Facet Coloured Bars
gg_bar_coloured_ver_x_facet_CaCaNu.(dataCaCaNu)
gg_bar_coloured_hor_x_facet_CaCaNu.(dataCaCaNu)
gg_bar_coloured_ver_y_facet_CaCaNu.(dataCaCaNu)
gg_bar_coloured_hor_y_facet_CaCaNu.(dataCaCaNu)
gg_bar_coloured_ver_z_facet_CaCaNu.(dataCaCaNu)
gg_bar_coloured_hor_z_facet_CaCaNu.(dataCaCaNu)

# Facet Coloured parameter Bar
gg_bar_coloured_parameter_ver_ver_facet_CaCaNu.(dataCaCaNu,
                                          parameter1 = c("Callejero"),
                                          parameter2 = c("A"))
gg_bar_coloured_parameter_ver_hor_facet_CaCaNu.(dataCaCaNu,
                                          parameter1 = c("Callejero", "De raza", "Mixto"),
                                          parameter2 = c("A","B","C"))

# Treemap
gg_treemap_x_CaCaNu.(dataCaCaNu)
gg_treemap_y_CaCaNu.(dataCaCaNu)
gg_treemap_density_z_CaCaNu.(dataCaCaNu, reverse = FALSE)

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


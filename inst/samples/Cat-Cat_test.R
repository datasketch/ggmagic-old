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

dataCatCat <- data.frame(c(catA, catB, catC), c(catD, catE, catF))
names(dataCatCat)[1] <- "RSGSH"; names(dataCatCat)[2] <- "GREHSEZGH"
dataCatCat[] <- sapply(dataCatCat, as.character)


# Bubble
gg_bubble_CatCat.(dataCatCat)


# Facet Dot Bar
gg_dot_bar_ver_facet_CatCat.(dataCatCat)
gg_dot_bar_hor_facet_CatCat.(dataCatCat)

# Facet Pie
gg_pie_facet_CatCat.(dataCatCat)

# Facet Donut
gg_donut_facet_CatCat.(dataCatCat)

# Facet Bullseye
gg_bullseye_facet_CatCat.(dataCatCat)

# Facet coloured Bar
gg_bar_coloured_x_ver_facet_CatCat.(dataCatCat)
gg_bar_coloured_x_hor_facet_CatCat.(dataCatCat)
gg_bar_coloured_y_ver_facet_CatCat.(dataCatCat)
gg_bar_coloured_y_hor_facet_CatCat.(dataCatCat)

# Facet coloured by parameter
gg_bar_coloured_parameter_ver_facet_CatCat.(dataCatCat,
                                          parameter1 = c("Catllejero"),
                                          parameter2 = c("A"))
gg_bar_coloured_parameter_hor_facet_CatCat.(dataCatCat,
                                          parameter1 = c("Catllejero", "De raza", "Mixto"),
                                          parameter2 = c("A","B","C"))



# Stacked Bar
gg_bar_stacked_ver_CatCat.(dataCatCat, l_ncol = 3)
gg_bar_stacked_hor_CatCat.(dataCatCat)


gg_bar_ordered_stacked_hor_CatCat.(dataCatCat)
gg_bar_ordered_stacked_ver_CatCat.(dataCatCat)

# Stacked dot Bar
gg_stacked_dot_bar_hor_CatCat.(dataCatCat)
gg_stacked_dot_bar_ver_CatCat.(dataCatCat, subtitle = 'ssd', caption = 'sdfs')

# Unstacked Coloured Bar
gg_bar_grouped_coloured_hor_CatCat.(dataCatCat)
gg_bar_grouped_coloured_ver_CatCat.(dataCatCat)

d2 <- data.frame(a=sample(letters[1:7],100,replace=TRUE),
                b=sample(letters[8:13],100,replace=TRUE))

# Facet Line
gg_line_hor_facet_CatCat.(d2)
gg_line_ver_facet_CatCat.(d2)

# Facet Line Point
gg_line_point_hor_facet_CatCat.(d2)
gg_line_point_hor_facet_CatCat.(d2)

# 100% Stacked Bar
gg_bar_stacked_100_ver_CatCat.(dataCatCat)
gg_bar_stacked_100_hor_CatCat.(dataCatCat)

# Stacked Area
gg_area_stacked_hor_CatCat.(d2, l_ncol = 2)
gg_area_stacked_ver_CatCat.(dataCatCat, l_ncol = 2)

# Stacked Area 100%
gg_area_stacked_100_hor_CatCat.(d2, l_ncol = 3)
gg_area_stacked_100_ver_CatCat.(d2, l_ncol = 2)

# Marimekko
gg_marimekko_ver_CatCat.(dataCatCat)
gg_marimekko_hor_CatCat.(dataCatCat)

# Polar Stacked Bar
gg_bar_polar_stacked_CatCat.(dataCatCat)

# Polar Stacked 100% Bar
gg_bar_polar_stacked_100_CatCat.(dataCatCat)

# Circular Bar
gg_bar_circular_facet_CatCat.(dataCatCat)

# Treemap
gg_treemap_x_CatCat.(dataCatCat)
gg_treemap_y_CatCat.(dataCatCat)

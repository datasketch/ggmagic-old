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

dataCaCa <- data.frame(c(catA, catB, catC), c(catD, catE, catF))
names(dataCaCa)[1] <- "RSGSH"; names(dataCaCa)[2] <- "GREHSEZGH"
dataCaCa[] <- sapply(dataCaCa, as.character)


# Bubble
gg_bubble_CaCa.(dataCaCa)

# Coloured Bubble
gg_coloured_bubble_CaCa.(dataCaCa)

# Facet Dot Bar
gg_facet_dot_bar_ver_CaCa.(dataCaCa)
gg_facet_dot_bar_hor_CaCa.(dataCaCa)

# Facet Pie
gg_facet_pie_CaCa.(dataCaCa)

# Facet Donut
gg_facet_donut_CaCa.(dataCaCa)

# Facet Bullseye
gg_facet_bullseye_CaCa.(dataCaCa)

# Facet coloured Bar
gg_bar_facet_coloured_x_ver_CaCa.(dataCaCa)
gg_bar_facet_coloured_x_hor_CaCa.(dataCaCa)
gg_bar_facet_coloured_y_ver_CaCa.(dataCaCa)
gg_bar_facet_coloured_y_hor_CaCa.(dataCaCa)

# Facet coloured by parameter
gg_bar_facet_coloured_parameter_ver_CaCa.(dataCaCa,
                                          parameter1 = c("Callejero"),
                                          parameter2 = c("A"))
gg_bar_facet_coloured_parameter_hor_CaCa.(dataCaCa,
                                          parameter1 = c("Callejero", "De raza", "Mixto"),
                                          parameter2 = c("A","B","C"))



# Stacked Bar
gg_bar_stacked_ver_CaCa.(dataCaCa)
gg_bar_stacked_hor_CaCa.(dataCaCa)


gg_bar_ordered_stacked_hor_CaCa.(dataCaCa)
gg_bar_ordered_stacked_ver_CaCa.(dataCaCa)

# Stacked dot Bar
gg_stacked_dot_bar_hor_CaCa.(dataCaCa)
gg_stacked_dot_bar_ver_CaCa.(dataCaCa)

# Unstacked Coloured Bar
gg_bar_unstacked_coloured_hor_CaCa.(dataCaCa)
gg_bar_unstacked_coloured_ver_CaCa.(dataCaCa)

d2 <- data.frame(a=sample(letters[1:7],100,replace=TRUE),
                b=sample(letters[8:13],100,replace=TRUE))

# Facet Line
gg_facet_line_hor_CaCa.(d2)
gg_facet_line_ver_CaCa.(d2)

# Facet Line Point
gg_facet_line_point_hor_CaCa.(d2)
gg_facet_line_point_ver_CaCa.(d2)

# 100% Stacked Bar
gg_bar_stacked_100_ver_CaCa.(dataCaCa)
gg_bar_stacked_100_hor_CaCa.(dataCaCa)

# Stacked Area
gg_area_stacked_hor_CaCa.(d2)
gg_area_stacked_ver_CaCa.(dataCaCa)

# Stacked Area 100%
gg_area_stacked_100_hor_CaCa.(d2)
gg_area_stacked_100_ver_CaCa.(d2)

# Marimekko
gg_marimekko_ver_CaCa.(dataCaCa)
gg_marimekko_hor_CaCa.(dataCaCa)

# Polar Stacked Bar
gg_bar_stacked_polar_CaCa.(dataCaCa)

# Polar Stacked 100% Bar
gg_bar_stacked_polar_100_CaCa.(dataCaCa)

# Circular Bar
gg_bar_facet_circular_CaCa.(dataCaCa)

# Treemap
gg_treemap_x_CaCa.(dataCaCa)
gg_treemap_y_CaCa.(dataCaCa)

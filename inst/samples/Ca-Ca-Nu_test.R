library(devtools)

load_all()
document()
install()
library(ggmagic)

data <- sampleData("Ca-Ca-Nu", nrow = 100, gt0 = TRUE)

# Bars

## Grouped Bar
gg_bar_grouped_ver_CaCaNu.(data)
gg_bar_grouped_ver_CaCaNu.(data, aggregation = "sum")
gg_bar_grouped_ver_CaCaNu.(data, aggregation = "mean")
gg_bar_grouped_hor_CaCaNu.(data)

gg_bar_grouped2_ver_CaCaNu.(data)
gg_bar_grouped2_hor_CaCaNu.(data)

## Stacked Bar
gg_bar_stacked_ver_CaCaNu.(data)
gg_bar_stacked_hor_CaCaNu.(data)

gg_bar_stacked2_ver_CaCaNu.(data)
gg_bar_stacked2_hor_CaCaNu.(data)


# Stacked Bar 100%
gg_bar_stacked_100_hor_CaCaNu.(data)
gg_bar_stacked_100_ver_CaCaNu.(data)

gg_bar_stacked2_100_hor_CaCaNu.(data)
gg_bar_stacked2_100_ver_CaCaNu.(data)

## Facet
data <- sampleData("Ca-Ca-Nu", nrow = 15)

gg_bar_facet_ver_CaCaNu.(data)
gg_bar_facet_ver_CaCaNu.(data, groupLabelPosition = "bottom")
gg_bar_facet_ver_CaCaNu.(data, aggregation = "median",fillMissing = FALSE)
gg_bar_facet_ver_CaCaNu.(data,  fillMissingValue = 500)

gg_bar_facet_hor_CaCaNu.(data, aggregation = "sum", fillMissing = FALSE)
gg_bar_facet_hor_CaCaNu.(data, groupLabelPosition = "left",fillMissing = FALSE)
gg_bar_facet_hor_CaCaNu.(data[c(2,1,3)], fillMissing = FALSE)







# Coloured Bubble
gg_bubble_coloured_x_CaCaNu.(data)
gg_bubble_coloured_y_CaCaNu.(data)

gg_pyramid_CaCaNu.(data)

# Bubble
gg_bubble_CaCaNu.(data)


# Line
gg_line_hor_facet_CaCaNu.(data)
gg_line_ver_facet_CaCaNu.(data)
d <- data.frame(type = c("hola", "hola", "casa", "casa"), x = c("12", "16", "12", "16"), y = c(15,57,124,44))
gg_multi_line_point_CaCaNu.(d, xLabel = "DEf")


# Stacked Area
gg_area_stacked_hor_CaCaNu.(data)
gg_area_stacked_ver_CaCaNu.(data)

# 100 Stacked Area
gg_area_stacked_100_hor_CaCaNu.(data)
gg_area_stacked_100_ver_CaCaNu.(data)

# Facet Circular
gg_bar_circular_facet_CaCaNu.(data)

# Facet Pie
gg_pie_facet_CaCaNu.(data)

# Facet Donut
gg_donut_facet_CaCaNu.(data)

# Facet Bullseye
gg_bullseye_facet_CaCaNu.(data)

# Stacked Polar Bar
gg_bar_stacked_polar_CaCaNu.(data)

# Stacked 100% Polar Bar
gg_bar_stacked_polar_100_CaCaNu.(data)

# Facet Coloured Bars
gg_bar_coloured_ver_x_facet_CaCaNu.(data)
gg_bar_coloured_hor_x_facet_CaCaNu.(data)
gg_bar_coloured_ver_y_facet_CaCaNu.(data)
gg_bar_coloured_hor_y_facet_CaCaNu.(data)
gg_bar_coloured_ver_z_facet_CaCaNu.(data)
gg_bar_coloured_hor_z_facet_CaCaNu.(data)

# Facet Coloured parameter Bar
gg_bar_coloured_parameter_ver_facet_CaCaNu.(data,
                                          parameter1 = c("Callejero"),
                                          parameter2 = c("A"))
gg_bar_coloured_parameter_hor_facet_CaCaNu.(data,
                                          parameter1 = c("Callejero", "De raza", "Mixto"),
                                          parameter2 = c("A","B","C"))

# Treemap
gg_treemap_x_CaCaNu.(data)
gg_treemap_y_CaCaNu.(data)
gg_treemap_density_z_CaCaNu.(data, reverse = FALSE)

# pyramid

catA <- rep("Callejero", round(runif(1, 10, 20), digits = 0))
catB <- rep("De raza", round(runif(1, 10, 20), digits = 0))
catD <- rep("A", length(catA)*0.8)
catE <- rep("B", length(catA)*0.9)
catF <- rep("C", - length(catD) - length(catE) + length(catA) + length(catB))

data <- data.frame(c(catA, catB), c(catD, catE, catF))
names(data)[1] <- "RSGSH"; names(data)[2] <- "GREHSEZGH"
data[] <- sapply(data, as.character)

data$c <- runif(nrow(data), min = 0, max = 50)
gg_pyramid_CaCaNu.(data)


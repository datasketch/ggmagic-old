library(devtools)

load_all()
document()
install()
library(ggmagic)

data <- sampleData("Cat-Cat-Num", nrow = 100, gt0 = TRUE)

# Bars

## Grouped Bar
gg_bar_grouped_ver_CatCatNum.(data)
gg_bar_grouped_ver_CatCatNum.(data, aggregation = "sum")
gg_bar_grouped_ver_CatCatNum.(data, aggregation = "mean")
gg_bar_grouped_hor_CatCatNum.(data)

gg_bar_grouped2_ver_CatCatNum.(data)
gg_bar_grouped2_hor_CatCatNum.(data)

## Stacked Bar
gg_bar_stacked_ver_CatCatNum.(data)
gg_bar_stacked_hor_CatCatNum.(data)

gg_bar_stacked2_ver_CatCatNum.(data)
gg_bar_stacked2_hor_CatCatNum.(data)


# Stacked Bar 100%
gg_bar_stacked_100_ver_CatCatNum.(data)
gg_bar_stacked_100_hor_CatCatNum.(data)

gg_bar_stacked2_100_hor_CatCatNum.(data)
gg_bar_stacked2_100_ver_CatCatNum.(data)

## Facet
data <- sampleData("Cat-Cat-Num", nrow = 15)

gg_bar_facet_ver_CatCatNum.(data)
gg_bar_facet_ver_CatCatNum.(data, groupLabelPosition = "bottom")
gg_bar_facet_ver_CatCatNum.(data, aggregation = "median",fillMissing = FALSE)
gg_bar_facet_ver_CatCatNum.(data,  fillMissingValue = 500)

gg_bar_facet_hor_CatCatNum.(data, aggregation = "sum", fillMissing = FALSE)
gg_bar_facet_hor_CatCatNum.(data, groupLabelPosition = "left",fillMissing = FALSE)
gg_bar_facet_hor_CatCatNum.(data[c(2,1,3)], fillMissing = FALSE)







# Coloured Bubble
gg_bubble_coloured_x_CatCatNum.(data)
gg_bubble_coloured_y_CatCatNum.(data)

gg_pyramid_CatCatNum.(data)

# Bubble
gg_bubble_CatCatNum.(data)


# Line
gg_line_hor_facet_CatCatNum.(data)
gg_line_ver_facet_CatCatNum.(data)
d <- data.frame(type = c("hola", "hola", "casa", "casa"), x = c("12", "16", "12", "16"), y = c(15,57,124,44))
gg_multi_line_point_CatCatNum.(d, xLabel = "DEf")


# Stacked Area
gg_area_stacked_hor_CatCatNum.(data)
gg_area_stacked_ver_CatCatNum.(data)

# 100 Stacked Area
gg_area_stacked_100_hor_CatCatNum.(data)
gg_area_stacked_100_ver_CatCatNum.(data)

# Facet Circular
gg_bar_circular_facet_CatCatNum.(data)

# Facet Pie
gg_pie_facet_CatCatNum.(data)

# Facet Donut
gg_donut_facet_CatCatNum.(data)

# Facet Bullseye
gg_bullseye_facet_CatCatNum.(data)

# Stacked Polar Bar
gg_bar_stacked_polar_CatCatNum.(data)

# Stacked 100% Polar Bar
gg_bar_stacked_polar_100_CatCatNum.(data)

# Facet Coloured Bars
gg_bar_coloured_ver_x_facet_CatCatNum.(data)
gg_bar_coloured_hor_x_facet_CatCatNum.(data)
gg_bar_coloured_ver_y_facet_CatCatNum.(data)
gg_bar_coloured_hor_y_facet_CatCatNum.(data)
gg_bar_coloured_ver_z_facet_CatCatNum.(data)
gg_bar_coloured_hor_z_facet_CatCatNum.(data)

# Facet Coloured parameter Bar
gg_bar_coloured_parameter_ver_facet_CatCatNum.(data,
                                          parameter1 = c("Catllejero"),
                                          parameter2 = c("A"))
gg_bar_coloured_parameter_hor_facet_CatCatNum.(data,
                                          parameter1 = c("Catllejero", "De raza", "Mixto"),
                                          parameter2 = c("A","B","C"))

# Treemap
gg_treemap_x_CatCatNum.(data)
gg_treemap_y_CatCatNum.(data)
gg_treemap_density_z_CatCatNum.(data, reverse = FALSE)

# pyramid

catA <- rep("Catllejero", round(runif(1, 10, 20), digits = 0))
catB <- rep("De raza", round(runif(1, 10, 20), digits = 0))
catD <- rep("A", length(catA)*0.8)
catE <- rep("B", length(catA)*0.9)
catF <- rep("C", - length(catD) - length(catE) + length(catA) + length(catB))

data <- data.frame(c(catA, catB), c(catD, catE, catF))
names(data)[1] <- "RSGSH"; names(data)[2] <- "GREHSEZGH"
data[] <- sapply(data, as.character)

data$c <- runif(nrow(data), min = 0, max = 50)
gg_pyramid_CatCatNum.(data)



# sunburst

firsnev <- c(rep('colombia',5), rep('usa',5))
twonev <- c(rep('amarillo',3),'verde', 'azul', rep('azul', 3), rep('rojo',2 ))
vals <- runif(10, min = 100, max = 500)

set.seed(1)
data <- data.frame(firsnev, twonev, vals)
data$firsnev <- as.factor(data$firsnev)
data$twonev <- as.factor(data$twonev)

gg_sunburst_CatCatNum.(data)



library(devtools)

load_all()
document()
install()
library(ggmagic)

catA <- rep("Catllejero", round(runif(1, 10, 20), digits = 0))
catB <- rep("De raza", round(runif(1, 10, 20), digits = 0))
catC <- rep("Mixto", round(runif(1, 10, 20), digits = 0))
dataCat <- data.frame(c(catA, catB, catC))
names(dataCat) <- "SGRWHSR"
data <- dataCat
# Waffle plot
gg_waffle_Cat.(dataCat)

d <- data.frame(a=sample(letters[1:7],10000,replace=TRUE))

# min.freq=1 por defecto
# Parámetros:
# minScale y maxScale

# Coloured Bar
gg_bar_coloured_ver_Cat.(dataCat, leg_pos = "right")
gg_bar_coloured_hor_Cat.(dataCat,leg_pos = "bottom", text = TRUE, text_size = 4)

# Bar
gg_bar_ver_Cat.(dataCat)
gg_bar_hor_Cat.(d)

# Coloured by parameter Bar
gg_bar_coloured_parameter_ver_Cat.(dataCat)
gg_bar_coloured_parameter_hor_Cat.(dataCat, parameter = c("Mixto", "De raza"))

# Ordered Bar
gg_bar_ordered_ver_Cat.(dataCat, type = 'percent')
gg_bar_ordered_hor_Cat.(dataCat, type = 'count')

# Pie
gg_pie_Cat.(dataCat, leg_pos = "right")

# Donut
gg_donut_Cat.(dataCat, "Tipos", width = 0.2)

# PILE CHART
gg_dot_bar_ver_Cat.(dataCat)
gg_dot_bar_hor_Cat.(dataCat)

# LINE
gg_line_hor_Cat.(d)
gg_line_ver_Cat.(d)

gg_line_point_hor_Cat.(dataCat)
gg_line_point_ver_Cat.(dataCat)

# GAUGE
gg_gauge_Cat.(dataCat, ncol = 1)
gg_gauge_dial_Cat.(dataCat)

#CIRCLE AREA PLOT
gg_bubble_Cat.(d)
gg_bubble_Cat.(dataCat)

# Coloured bubble
gg_bubble_coloured_Cat.(d)
gg_bubble_coloured_Cat.(dataCat)

# Polar Bar
gg_bar_polar_Cat.(dataCat, width = 1)

# Single Stacked Bar
gg_bar_single_stacked_hor_Cat.(dataCat)
gg_bar_single_stacked_ver_Cat.(dataCat)

# Bullseye
gg_bullseye_Cat.(dataCat)

# Circular Bar
gg_bar_circular_Cat.(dataCat)

# Treemap
gg_treemap_Cat.(dataCat)

# Bubbles 2
gg_bubble_Cat2.(dataCat)

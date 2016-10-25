library(devtools)

load_all()
document()
install()
library(ggmagic)

catA <- rep("Callejero", round(runif(1, 10, 20), digits = 0))
catB <- rep("De raza", round(runif(1, 10, 20), digits = 0))
catC <- rep("Mixto", round(runif(1, 10, 20), digits = 0))
dataCat <- data.frame(c(catA, catB, catC))
names(dataCat) <- "SGRWHSR"
data <- dataCat
# Waffle plot
gg_waffle_Ca.(d, 50, "Reporte \n de conteo de perros")

d <- data.frame(a=sample(letters[1:7],10000,replace=TRUE))

# min.freq=1 por defecto
# Parámetros:
# minScale y maxScale

# Coloured Bar
gg_coloured_bar_ver_Ca.(dataCat, leg_pos = "top")
gg_coloured_bar_hor_Ca.(d,leg_pos = "bottom")

# Bar
gg_bar_ver_Ca.(dataCat)
gg_bar_hor_Ca.(d)

# Coloured by parameter Bar
gg_coloured_parameter_bar_ver_Ca.(dataCat)
gg_coloured_parameter_bar_hor_Ca.(dataCat, parameter = c("Mixto", "De raza"))

# Ordered Bar
gg_ordered_bar_ver_Ca.(dataCat)
gg_ordered_bar_hor_Ca.(dataCat)

# Pie
gg_pie_Ca.(dataCat, leg_pos = "right")

# Donut
gg_donut_Ca.(dataCat, "Tipos", width = 0.2)

# PILE CHART
gg_dot_bar_ver_Ca.(dataCat)
gg_dot_bar_hor_Ca.(dataCat)

# LINE
gg_line_hor_Ca.(d)
gg_line_ver_Ca.(d)

gg_line_point_hor_Ca.(dataCat)
gg_line_point_ver_Ca.(dataCat)

# GAUGE
gg_gauge_Ca.(dataCat)
gg_gauge_dial_Ca.(dataCat)

#CIRCLE AREA PLOT
gg_bubble_Ca.(d)
gg_bubble_Ca.(dataCat)

# Coloured bubble
gg_coloured_bubble_Ca.(d)
gg_coloured_bubble_Ca.(dataCat)

# Polar Bar
gg_polar_bar_Ca.(dataCat, width = 1)

# Single Stacked Bar
gg_single_stacked_bar_hor_Ca.(dataCat)
gg_single_stacked_bar_ver_Ca.(dataCat)

# Bullseye
gg_bullseye_Ca.(dataCat)

# Circular Bar
gg_circular_bar_Ca.(dataCat)

# Treemap

library(devtools)

load_all()
document()
install()
library(ggmagic)

catA <- rep("Callejero", round(runif(1, 10, 20), digits = 0))
catB <- rep("De raza", round(runif(1, 10, 20), digits = 0))
catC <- rep("Mixto", round(runif(1, 10, 20), digits = 0))
dataCa <- data.frame(c(catA, catB, catC))
names(dataCa) <- "SGRWHSR"
data <- dataCa
# Waffle plot
gg_waffle_Ca.(dataCa)

d <- data.frame(a=sample(letters[1:7],10000,replace=TRUE))

# min.freq=1 por defecto
# Parámetros:
# minScale y maxScale

# Coloured Bar
gg_bar_coloured_ver_Ca.(dataCa, leg_pos = "right")
gg_bar_coloured_hor_Ca.(dataCa,leg_pos = "bottom", text = TRUE, size_text = 4)

# Bar
gg_bar_ver_Ca.(dataCa)
gg_bar_hor_Ca.(d)

# Coloured by parameter Bar
gg_bar_coloured_parameter_ver_Ca.(dataCa)
gg_bar_coloured_parameter_hor_Ca.(dataCa, parameter = c("Mixto", "De raza"))

# Ordered Bar
gg_bar_ordered_ver_Ca.(dataCa)
gg_bar_ordered_hor_Ca.(dataCa)

# Pie
gg_pie_Ca.(dataCa, leg_pos = "right")

# Donut
gg_donut_Ca.(dataCa, "Tipos", width = 0.2)

# PILE CHART
gg_dot_bar_ver_Ca.(dataCa)
gg_dot_bar_hor_Ca.(dataCa)

# LINE
gg_line_hor_Ca.(d)
gg_line_ver_Ca.(d)

gg_line_point_hor_Ca.(dataCa)
gg_line_point_ver_Ca.(dataCa)

# GAUGE
gg_gauge_Ca.(dataCa)
gg_gauge_dial_Ca.(dataCa)

#CIRCLE AREA PLOT
gg_bubble_Ca.(d)
gg_bubble_Ca.(dataCa)

# Coloured bubble
gg_coloured_bubble_Ca.(d)
gg_coloured_bubble_Ca.(dataCa)

# Polar Bar
gg_bar_polar_Ca.(dataCa, width = 1)

# Single Stacked Bar
gg_bar_single_stacked_hor_Ca.(dataCa)
gg_bar_single_stacked_ver_Ca.(dataCa)

# Bullseye
gg_bullseye_Ca.(dataCa)

# Circular Bar
gg_bar_circular_Ca.(dataCa)

# Treemap
gg_treemap_Ca.(dataCa)


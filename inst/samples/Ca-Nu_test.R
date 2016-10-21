library(devtools)
document()
install()
load_all()

catA <- rep("Callejero", round(runif(1, 10, 20), digits = 0))
catB <- rep("De raza", round(runif(1, 10, 20), digits = 0))
catC <- rep("Mixto", round(runif(1, 10, 20), digits = 0))
dataCaNu <- data.frame(c(catA, catB, catC))
names(dataCaNu)[1] <- "GSRG"
dataCaNu$GSRG <- as.character(dataCaNu$GSRG)
dataCaNu$SRHTD <- runif(nrow(dataCaNu), min = 0, max = 100)


# Pie
gg_pie_CaNu.(dataCaNu)

# Bubble
gg_bubble_CaNu.(dataCaNu)

# Coloured Bubble
gg_coloured_bubble_CaNu.(dataCaNu)

# Coloured Bar
gg_coloured_bar_x_ver_CaNu.(dataCaNu)
gg_coloured_bar_x_hor_CaNu.(dataCaNu)
gg_coloured_bar_y_ver_CaNu.(dataCaNu)
gg_coloured_bar_y_hor_CaNu.(dataCaNu)

# Coloured Parameter Bar
gg_coloured_parameter_bar_ver_CaNu.(dataCaNu)
gg_coloured_parameter_bar_hor_CaNu.(dataCaNu, parameter = "Callejero")

#Ordered Bar
gg_ordered_bar_ver_CaNu.(dataCaNu)
gg_ordered_bar_hor_CaNu.(dataCaNu)

# Polar Bar
gg_polar_bar_CaNu.(dataCaNu)

# Circular Bar
gg_circular_bar_CaNu.(dataCaNu)

#Stacked histogram
gg_stacked_hist_ver_CaNu.(dataCaNu)


#multiple density, single plot
gg_coloured_multi_density_dist_CaNu.(dataCaNu)
gg_area_multi_density_dist_CaNu.(dataCaNu)


#Facet Density
gg_facet_dist_ver_CaNu.(dataCaNu)
gg_facet_dist_hor_CaNu.(dataCaNu)

#Facet Histogram + Combinations
gg_facet_hist_hor_CaNu.(dataCaNu)
gg_facet_hist_ver_CaNu.(dataCaNu)
gg_facet_hist_mean_hor_CaNu.(dataCaNu)
gg_facet_hist_mean_ver_CaNu.(dataCaNu)
gg_facet_dist_hist_hor_CaNu.(dataCaNu)
gg_facet_dist_hist_ver_CaNu.(dataCaNu)
gg_facet_dist_hist_mean_hor_CaNu.(dataCaNu)
gg_facet_dist_hist_mean_hor_CaNu.(dataCaNu)
gg_facet_dot_dist_ver_CaNu.(dataCaNu)
gg_facet_dot_dist_hor_CaNu.(dataCaNu)
gg_facet_dot_hist_ver_CaNu.(dataCaNu)
gg_facet_dot_hist_hor_CaNu.(dataCaNu)
gg_facet_dot_hist_mean_ver_CaNu.(dataCaNu)
gg_facet_dot_hist_mean_hor_CaNu.(dataCaNu)
gg_facet_dot_dist_hist_ver_CaNu.(dataCaNu)
gg_facet_dot_dist_hist_hor_CaNu.(dataCaNu)
gg_facet_dot_dist_hist_mean_ver_CaNu.(dataCaNu)
gg_facet_dot_dist_hist_mean_ver_CaNu.(dataCaNu)

# Point
gg_facet_point_CaNu.(dataCaNu)
gg_grouped_point_CaNu.(dataCaNu)
gg_facet_point_trend_line_CaNu.(dataCaNu)
gg_facet_trend_ribbon_CaNu.(dataCaNu)

# Donut
gg_donut_CaNu.(dataCaNu)

# Bullseye
gg_bullseye_CaNu.(dataCaNu)

# Dot Bar
gg_dot_bar_ver_CaNu.(dataCaNu)
gg_dot_bar_hor_CaNu.(dataCaNu)

# Single Satcked Bar
gg_single_stacked_bar_ver_CaNu.(dataCaNu)
gg_single_stacked_bar_hor_CaNu.(dataCaNu)

# Gauge
gg_gauge_CaNu.(dataCaNu)
gg_gauge_dial_Ca.(dataCaNu)

#Grouped Line Point
gg_multi_line_point_CaNu.(dataCaNu)
gg_multi_line_CaNu.(dataCaNu)

# Facet Line
gg_facet_line_point_CaNu.(dataCaNu)
gg_facet_line_CaNu.(dataCaNu)

# Area
gg_facet_area_hor_CaNu.(dataCaNu)
gg_facet_area_ver_CaNu.(dataCaNu)
gg_stacked_area_100_ver_CaNu.(dataCaNu)
gg_stacked_area_100_hor_CaNu.(dataCaNu)
gg_stacked_area_hor_CaNu.(dataCaNu)
gg_stacked_area_ver_CaNu.(dataCaNu)

gg_facet_stepped_area_ver_CaNu.(dataCaNu)

# Facet Horizon
gg_facet_horizon_CaNu.(dataCaNu)

# Stream
gg_stream_CaNu.(dataCaNu)

#boxplots

boxSpltPlot(dataCaNu, titleLabel, xLabel, yLabel, fillLabel, voltear)

#violin plots

ViolinMultPlot(dataCaNu, titleLabel, xLabel, yLabel, fillLabel, voltear)


#violin plots + obs. dots
ViolinDotMultPlot(dataCaNu, titleLabel, xLabel, yLabel, fillLabel, voltear)

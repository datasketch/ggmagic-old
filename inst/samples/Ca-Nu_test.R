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

dataCaNu <- rename(dataCaNu, c('GSRG' = 'a', 'SRHTD' = 'b'))

d <- sampleData("Ca-Nu", 1000)
gg_bubble_CaNu2.(d)
gg_bullseye_CaNu.(d)
data_graph <- d %>% dplyr::group_by(a) %>%
  dplyr::summarise(count = sum(b)) %>% dplyr::arrange(desc(count))

# Pie
gg_pie_CaNu.(dataCaNu, text = FALSE)
gg_pie_CaNu.(dataCaNu, type = 'count')


# Bubble
gg_bubble_CaNu.(dataCaNu)

# Bubble2
gg_bubble_CaNu2.(dataCaNu)

# Coloured Bubble
gg_coloured_bubble_CaNu.(dataCaNu)

# Coloured Bar
gg_bar_coloured_x_ver_CaNu.(dataCaNu)



gg_bar_coloured_x_hor_CaNu.(dataCaNu, type = 'count')



gg_bar_coloured_y_ver_CaNu.(dataCaNu, reverse = TRUE, type = 'count')
gg_bar_coloured_y_hor_CaNu.(dataCaNu)

# Coloured Parameter Bar
gg_bar_coloured_parameter_ver_CaNu.(dataCaNu)
gg_bar_coloured_parameter_hor_CaNu.(dataCaNu, parameter = "Callejero")

#Ordered Bar
gg_bar_ordered_ver_CaNu.(dataCaNu)
gg_bar_ordered_hor_CaNu.(dataCaNu)

# Polar Bar
gg_bar_polar_CaNu.(dataCaNu, width = 1)

# Circular Bar
gg_bar_circular_CaNu.(dataCaNu)

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
gg_bar_single_stacked_ver_CaNu.(dataCaNu)
gg_bar_single_stacked_hor_CaNu.(dataCaNu)

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
gg_area_stacked_100_ver_CaNu.(dataCaNu)
gg_area_stacked_100_hor_CaNu.(dataCaNu)
gg_area_stacked_hor_CaNu.(dataCaNu)
gg_area_stacked_ver_CaNu.(dataCaNu)

gg_facet_stepped_area_ver_CaNu.(dataCaNu)

# Facet Horizon
gg_facet_horizon_CaNu.(dataCaNu)

# Steam
gg_steam_CaNu.(dataCaNu)

# Treemap
gg_treemap_x_CaNu.(dataCaNu)
gg_treemap_density_y_CaNu.(dataCaNu, reverse = TRUE)

data <- data.frame(hechos = c("secuestro", "secuestro", "delito", "delito", "ex", "ex"),
                   year = c(5000,160,5923,21563,952, 4565))

gg_slope_CaNu.(data, size_point = 1)

#boxplots

boxSpltPlot(dataCaNu, titleLabel, xLabel, yLabel, fillLabel, voltear)

#violin plots

ViolinMultPlot(dataCaNu, titleLabel, xLabel, yLabel, fillLabel, voltear)


#violin plots + obs. dots
ViolinDotMultPlot(dataCaNu, titleLabel, xLabel, yLabel, fillLabel, voltear)

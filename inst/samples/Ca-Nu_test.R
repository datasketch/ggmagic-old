library(devtools)
document()
install()


load_all()

data <- sampleData("Ca-Nu", 1000)

# Pie
gg_pie_CaNu.(data, type = 'count', aggregation = "sum")

# Bullseye
gg_bullseye_CaNu.(data)

# Bubble
gg_bubble_CaNu.(data)

# Bubble2
gg_bubble_CaNu2.(data)
gg_bubble_coloured_CaNu.(data)


# Bars
gg_bar_ver_CaNu.(data, aggregation = "sum", line_mean = TRUE)
gg_bar_ver_CaNu.(data, aggregation = "sum", title = "HOLA",xLabel = "YEAH", line_mean = TRUE)

gg_bar_ver_CaNu.(data, aggregation = "mean", angle = 45)
gg_bar_hor_CaNu.(data, aggregation = "mean")
gg_bar_hor_CaNu.(data, aggregation = "sum", title = "XXX", yLabel ="Y rules", line_mean = TRUE)
gg_bar_hor_CaNu.(data, aggregation = "sum")


# Coloured Bar
gg_bar_coloured_x_ver_CaNu.(data, line_mean = TRUE, text = TRUE, aggregation = 'sum')
gg_bar_coloured_x_hor_CaNu.(data, line_mean = TRUE, type = 'count')
gg_bar_coloured_y_ver_CaNu.(data, reverse = TRUE, line_mean = TRUE, type = 'count')
gg_bar_coloured_y_hor_CaNu.(data, line_mean = TRUE)


# Coloured Parameter Bar
gg_bar_coloured_parameter_ver_CaNu.(data, order = TRUE, parameter = 'max')
gg_bar_coloured_parameter_ver_CaNu.(data, parameter = 'TypeA')
gg_bar_coloured_parameter_hor_CaNu.(data, order = TRUE, parameter = 'TypeD', line_mean = TRUE, aggregation = 'sum')

#Ordered Bar
gg_bar_ordered_ver_CaNu.(data)
gg_bar_ordered_hor_CaNu.(data)

# Polar Bar
gg_bar_polar_CaNu.(data, width = 1)

# Circular Bar
gg_bar_circular_CaNu.(data)

#Stacked histogram
gg_hist_stacked_ver_CaNu.(data)


#multiple density, single plot
gg_density_multi_dist_coloured_CaNu.(data)
gg_area_multi_density_dist_CaNu.(data)


#Facet Density
gg_dist_ver_facet_CaNu.(data)
gg_dist_hor_facet_CaNu.(data)

#Facet Histogram + Combinations
gg_hist_hor_facet_CaNu.(data)
gg_hist_ver_facet_CaNu.(data)
gg_hist_hor_mean_facet_CaNu.(data)
gg_hist_ver_mean_facet_CaNu.(data)
gg_dist_hist_hor_facet_CaNu.(data)
gg_dist_hist_ver_facet_CaNu.(data)
gg_dist_hist_hor_mean_facet_CaNu.(data)
gg_dist_hist_ver_mean_facet_CaNu.(data)
gg_dot_dist_ver_facet_CaNu.(data)
gg_dot_dist_hor_facet_CaNu.(data)
gg_dot_hist_ver_facet_CaNu.(data)
gg_dot_hist_hor_facet_CaNu.(data)
gg_dot_hist_ver_mean_facet_CaNu.(data)
gg_dot_hist_hor_mean_facet_CaNu.(data)
gg_dot_dist_hist_ver_facet_CaNu.(data)
gg_dot_dist_hist_hor_facet_CaNu.(data)
gg_dot_dist_hist_ver_mean_facet_CaNu.(data)
gg_dot_dist_hist_hor_mean_facet_CaNu.(data)

# Point
gg_point_facet_CaNu.(data)
gg_line_point_facet_CaNu.(data)
gg_line_facet_CaNu.(data)
gg_point_grouped_CaNu.(data)
gg_point_trend_line_facet_CaNu.(data)
gg_trend_ribbon_facet_CaNu.(data)

# Donut
gg_donut_CaNu.(data)

# Bullseye
gg_bullseye_CaNu.(data)

# Dot Bar
gg_dot_bar_ver_CaNu.(data)
gg_dot_bar_hor_CaNu.(data)

# Single Satcked Bar
gg_bar_single_stacked_ver_CaNu.(data)
gg_bar_single_stacked_hor_CaNu.(data)

# Gauge
gg_gauge_CaNu.(data)
gg_gauge_dial_CaNu.(data)

#Grouped Line Point
gg_line_point_multi_CaNu.(data, type = 4)
gg_line_multi_CaNu.(data)

# Facet Line
gg_line_point_facet_CaNu.(data)
gg_line_facet_CaNu.(data)

# Area
gg_area_ver_facet_CaNu.(data)
gg_area_hor_facet_CaNu.(data)
gg_area_stacked_100_ver_CaNu.(data)
gg_area_stacked_100_hor_CaNu.(data)
gg_area_stacked_hor_CaNu.(data)
gg_area_stacked_ver_CaNu.(data)

# stream
gg_steam_CaNu.(data)

# Treemap
gg_treemap_x_CaNu.(data)
gg_treemap_density_y_CaNu.(dataata, reverse = TRUE)

data <- data.frame(hechos = c("secuestro", "secuestro", "delito", "delito", "ex", "ex"),
                   year = c(5000,160,5923,21563,952, 4565))

gg_slope_CaNu.(dataata, size_point = 1)


#boxplots

gg_boxplot_CaNu.(data)
gg_boxplot_flip_CaNu.(data)

gg_boxplot_dot_CaNu.(data)
gg_boxplot_dot_flip_CaNu.(data)

#violin plots

gg_violin_mult_CaNu.(data)
gg_violin_mult_flip_CaNu.(data)

#violin plots + obs. dots
gg_violin_dot_mult_CaNu.(data)
gg_violin_dot_mult_flip_CaNu.(data)

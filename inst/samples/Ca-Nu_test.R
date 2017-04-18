library(devtools)
document()
install()


load_all()

data <- sampleData("Cat-Num", 1000)

# Pie
gg_pie_CatNum.(data, type = 'count', aggregation = "sum")

# Bullseye
gg_bullseye_CatNum.(data)

# Bubble
gg_bubble_CatNum.(data)

# Bubble2
gg_bubble_CatNum2.(data)
gg_bubble_coloured_x_CatNum.(data)


# Bars
gg_bar_ver_CatNum.(data, aggregation = "sum", line_mean = TRUE)
gg_bar_ver_CatNum.(data, aggregation = "sum", title = "HOLA",xLabel = "YEAH", line_mean = TRUE)

gg_bar_ver_CatNum.(data, aggregation = "mean", angle = 45)
gg_bar_hor_CatNum.(data, aggregation = "mean")
gg_bar_hor_CatNum.(data, aggregation = "sum", title = "XXX", yLabel ="Y rules", line_mean = TRUE)
gg_bar_hor_CatNum.(data, aggregation = "sum")


# Coloured Bar
gg_bar_coloured_x_ver_CatNum.(data, line_mean = TRUE, text = TRUE, aggregation = 'sum')
gg_bar_coloured_x_hor_CatNum.(data, line_mean = TRUE, type = 'count')
gg_bar_coloured_y_ver_CatNum.(data, reverse = TRUE, line_mean = TRUE, type = 'count')
gg_bar_coloured_y_hor_CatNum.(data, line_mean = TRUE)


# Coloured Parameter Bar
gg_bar_coloured_parameter_ver_CatNum.(data, order = TRUE, parameter = 'max')
gg_bar_coloured_parameter_ver_CatNum.(data, parameter = 'TypeA')
gg_bar_coloured_parameter_hor_CatNum.(data, order = TRUE, parameter = 'TypeD', line_mean = TRUE, aggregation = 'sum')

#Ordered Bar
gg_bar_ordered_ver_CatNum.(data)
gg_bar_ordered_hor_CatNum.(data)

# Polar Bar
gg_bar_polar_CatNum.(data, width = 1)

# Circular Bar
gg_bar_circular_CatNum.(data)

#Stacked histogram
gg_hist_stacked_ver_CatNum.(data)


#multiple density, single plot
gg_density_multi_dist_coloured_CatNum.(data)
gg_area_multi_density_dist_CatNum.(data)


#Facet Density
gg_dist_ver_facet_CatNum.(data)
gg_dist_hor_facet_CatNum.(data)

#Facet Histogram + Combinations
gg_hist_hor_facet_CatNum.(data)
gg_hist_ver_facet_CatNum.(data)
gg_hist_hor_mean_facet_CatNum.(data)
gg_hist_ver_mean_facet_CatNum.(data)
gg_dist_hist_hor_facet_CatNum.(data)
gg_dist_hist_ver_facet_CatNum.(data)
gg_dist_hist_hor_mean_facet_CatNum.(data)
gg_dist_hist_ver_mean_facet_CatNum.(data)
gg_dot_dist_ver_facet_CatNum.(data)
gg_dot_dist_hor_facet_CatNum.(data)
gg_dot_hist_ver_facet_CatNum.(data)
gg_dot_hist_hor_facet_CatNum.(data)
gg_dot_hist_ver_mean_facet_CatNum.(data)
gg_dot_hist_hor_mean_facet_CatNum.(data)
gg_dot_dist_hist_ver_facet_CatNum.(data)
gg_dot_dist_hist_hor_facet_CatNum.(data)
gg_dot_dist_hist_ver_mean_facet_CatNum.(data)
gg_dot_dist_hist_hor_mean_facet_CatNum.(data)

# Point
gg_point_facet_CatNum.(data)
gg_line_point_facet_CatNum.(data)
gg_line_facet_CatNum.(data)
gg_point_grouped_CatNum.(data)
gg_point_trend_line_facet_CatNum.(data)
gg_trend_ribbon_facet_CatNum.(data)

# Donut
gg_donut_CatNum.(data)

# Bullseye
gg_bullseye_CatNum.(data)

# Dot Bar
gg_dot_bar_ver_CatNum.(data)
gg_dot_bar_hor_CatNum.(data)

# Single Satcked Bar
gg_bar_single_stacked_ver_CatNum.(data)
gg_bar_single_stacked_hor_CatNum.(data)

# Gauge
gg_gauge_CatNum.(data)
gg_gauge_dial_CatNum.(data)

#Grouped Line Point
gg_line_point_multi_CatNum.(data, type = 4)
gg_line_multi_CatNum.(data)

# Facet Line
gg_line_point_facet_CatNum.(data)
gg_line_facet_CatNum.(data)

# Area
gg_area_ver_facet_CatNum.(data)
gg_area_hor_facet_CatNum.(data)
gg_area_stacked_100_ver_CatNum.(data)
gg_area_stacked_100_hor_CatNum.(data)
gg_area_stacked_hor_CatNum.(data)
gg_area_stacked_ver_CatNum.(data)

# stream
gg_steam_CatNum.(data)

# Treemap
gg_treemap_x_CatNum.(data)
gg_treemap_density_y_CatNum.(data, reverse = TRUE)

data <- data.frame(hechos = c("secuestro", "secuestro", "delito", "delito", "ex", "ex"),
                   year = c(5000,160,5923,21563,952, 4565))

gg_slope_CatNum.(data, size_point = 1)


#boxplots

gg_boxplot_CatNum.(data)
gg_boxplot_flip_CatNum.(data)

gg_boxplot_dot_CatNum.(data)
gg_boxplot_dot_flip_CatNum.(data)

#violin plots


gg_violin_mult_CatNum.(data)
gg_violin_mult_flip_CatNum.(data)

#violin plots + obs. dots
gg_violin_dot_mult_CatNum.(data)
gg_violin_dot_mult_flip_CatNum.(data)


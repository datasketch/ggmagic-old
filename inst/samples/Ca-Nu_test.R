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

names(dataCaNu) <- c('GSRG', 'SRHTD')

d <- sampleData("Ca-Nu", 1000)
gg_bubble_CaNu2.(dataCaNu)
gg_bullseye_CaNu.(dataCaNu)
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
gg_bubble_coloured_CaNu.(dataCaNu)

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
gg_hist_stacked_ver_CaNu.(dataCaNu)


#multiple density, single plot
gg_density_multi_dist_coloured_CaNu.(dataCaNu)
gg_area_multi_density_dist_CaNu.(dataCaNu)


#Facet Density
gg_dist_ver_facet_CaNu.(dataCaNu)
gg_dist_hor_facet_CaNu.(dataCaNu)

#Facet Histogram + Combinations
gg_hist_hor_facet_CaNu.(dataCaNu)
gg_hist_ver_facet_CaNu.(dataCaNu)
gg_hist_hor_mean_facet_CaNu.(dataCaNu)
gg_hist_ver_mean_facet_CaNu.(dataCaNu)
gg_dist_hist_hor_facet_CaNu.(dataCaNu)
gg_dist_hist_ver_facet_CaNu.(dataCaNu)
gg_dist_hist_hor_mean_facet_CaNu.(dataCaNu)
gg_dist_hist_ver_mean_facet_CaNu.(dataCaNu)
gg_dot_dist_ver_facet_CaNu.(dataCaNu)
gg_dot_dist_hor_facet_CaNu.(dataCaNu)
gg_dot_hist_ver_facet_CaNu.(dataCaNu)
gg_dot_hist_hor_facet_CaNu.(dataCaNu)
gg_dot_hist_ver_mean_facet_CaNu.(dataCaNu)
gg_dot_hist_hor_mean_facet_CaNu.(dataCaNu)
gg_dot_dist_hist_ver_facet_CaNu.(dataCaNu)
gg_dot_dist_hist_hor_facet_CaNu.(dataCaNu)
gg_dot_dist_hist_ver_mean_facet_CaNu.(dataCaNu)
gg_dot_dist_hist_hor_mean_facet_CaNu.(dataCaNu)

# Point
gg_point_facet_CaNu.(dataCaNu)
gg_line_point_facet_CaNu.(dataCaNu)
gg_line_facet_CaNu.(dataCaNu)
gg_point_grouped_CaNu.(dataCaNu)
gg_point_trend_line_facet_CaNu.(dataCaNu)
gg_trend_ribbon_facet_CaNu.(dataCaNu)

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
gg_gauge_dial_CaNu.(dataCaNu)

#Grouped Line Point
gg_line_point_multi_CaNu.(dataCaNu, type = 4)
gg_line_multi_CaNu.(dataCaNu)

# Facet Line
gg_line_point_facet_CaNu.(dataCaNu)
gg_line_facet_CaNu.(dataCaNu)

# Area
gg_area_ver_facet_CaNu.(dataCaNu)
gg_area_hor_facet_CaNu.(dataCaNu)
gg_area_stacked_100_ver_CaNu.(dataCaNu)
gg_area_stacked_100_hor_CaNu.(dataCaNu)
gg_area_stacked_hor_CaNu.(dataCaNu)
gg_area_stacked_ver_CaNu.(dataCaNu)

# stream
gg_steam_CaNu.(dataCaNu)

# Treemap
gg_treemap_x_CaNu.(dataCaNu)
gg_treemap_density_y_CaNu.(dataCaNu, reverse = TRUE)

data <- data.frame(hechos = c("secuestro", "secuestro", "delito", "delito", "ex", "ex"),
                   year = c(5000,160,5923,21563,952, 4565))

gg_slope_CaNu.(data, size_point = 1)


#boxplots

gg_boxplot_CaNu.(dataCaNu)
gg_boxplot_flip_CaNu.(dataCaNu)

gg_boxplot_dot_CaNu.(dataCaNu)
gg_boxplot_dot_flip_CaNu.(dataCaNu)

#violin plots

gg_violin_mult_CaNu.(dataCaNu)
gg_violin_mult_flip_CaNu.(dataCaNu)

#violin plots + obs. dots
gg_violin_dot_mult_CaNu.(dataCaNu)
gg_violin_dot_mult_flip_CaNu.(dataCaNu)

library(devtools)
document()
install()
load_all()


data <- sampleData("Ca-Nu-Nu")

# Scatter

gg_scatter_CaNuNu.(data) # TODO use ggrepel for labels

gg_scatter_trend_CaNuNu.(data) # TODO use ggrepel for labels
gg_scatter_trend_CaNuNu.(data,se = TRUE) # TODO use ggrepel for labels



####

gg_steam_CaNuNu.(data)
gg_line_CaNuNu.(data)
gg_point_CaNuNu.(data, shape_point = 8)

gg_point_line_CaNuNu.(data)
gg_circle_CaNuNu.(data)

library(devtools)
document()
install()
load_all()


data <- sampleData("Cat-Num-Num")

# Scatter

gg_scatter_CatNumNum.(data) # TODO use ggrepel for labels

gg_scatter_trend_CatNumNum.(data) # TODO use ggrepel for labels
gg_scatter_trend_CatNumNum.(data,se = TRUE) # TODO use ggrepel for labels



####

gg_steam_CatNumNum.(data)
gg_line_CatNumNum.(data)
gg_point_line_CatNumNum.(data, shape_point = 8)

gg_point_line_CatNumNum.(data)
#gg_circle_CatNumNum.(data)

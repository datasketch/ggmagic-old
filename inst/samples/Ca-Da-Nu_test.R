# Ca-Da

library(devtools)
load_all()
document()
install()
library(ggmagic)

dataCaDaNu <- sampleData('Ca-Da-Nu')
gg_scatter_hor_CaDaNu.(dataCaDaNu)


dataCaDaNu <- sampleData('Ca-Da-Nu', nrow = 400)
gg_steam_CaDaNu.(dataCaDaNu)

dfData = data.frame(
  time = 1:1000,
  value = abs(
    c(
      cumsum(rnorm(1000, 0, 3)),
      cumsum(rnorm(1000, 0, 4)),
      cumsum(rnorm(1000, 0, 1)),
      cumsum(rnorm(1000, 0, 2))
    )
  ),
  cat = c(rep('Class A', 1000), rep('Class B', 1000), rep('Class C', 1000), rep('Class D', 1000))
)

dfData <- dfData %>% select(cat, time, value)
gg_steam_CaDaNu.(dfData)

gg_area_stacked_ver_CaDaNu.(dfData)
gg_area_stacked_hor_CaDaNu.(dfData)
gg_area_stacked_100_ver_CaDaNu.(dfData)
gg_area_stacked_100_hor_CaDaNu.(dfData)

gg_multi_line_CaDaNu.(dataCaDaNu)
gg_multi_line_point_CaDaNu.(dataCaDaNu)

gg_bar_stacked_ver_CaDaNu.(dataCaDaNu, angle_x = 45, hline = c(500,1000))

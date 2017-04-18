# Cat-Dat

library(devtools)
load_all()
document()
install()
library(ggmagic)

dataCatDatNum <- sampleData('Cat-Dat-Num')
gg_scatter_hor_CatDatNum.(dataCatDatNum)


dataCatDatNum <- sampleData('Cat-Dat-Num', nrow = 400)
gg_steam_CatDatNum.(dataCatDatNum)

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
gg_steam_CatDatNum.(dfData)

gg_area_stacked_ver_CatDatNum.(dfData)
gg_area_stacked_hor_CatDatNum.(dfData)
gg_area_stacked_100_ver_CatDatNum.(dfData)
gg_area_stacked_100_hor_CatDatNum.(dfData)

gg_multi_line_CatDatNum.(dataCatDatNum)
gg_multi_line_point_CatDatNum.(dataCatDatNum)

gg_bar_stacked_ver_CatDatNum.(dataCatDatNum, angle_x = 45, hline = c(500,1000))

library(devtools)
load_all()
document()
install()

library(ggmagic)

### Area plots

df <- sampleData('Yea-Num')
gg_area_YeaNum.(df)

dfC <- sampleData('Cat-Num')
gg_area_ver_facet_CatNum.(dfC)
gg_area_hor_facet_CatNum.(dfC)
gg_area_stacked_100_ver_CatNum.(dfC)
gg_area_stacked_100_hor_CatNum.(dfC)
gg_area_stacked_ver_CatNum.(dfC)
gg_area_stacked_hor_CatNum.(dfC)
gg_area_multi_density_dist_CatNum.(dfC)

dfD <- sampleData('Dat-Num')
gg_area_DatNum.(dfD)
gg_area_stepped_DatNum.(dfD)

dfCdn <- data.frame(
  cat = c(rep('Class A', 1000), rep('Class B', 1000), rep('Class C', 1000), rep('Class D', 1000)),
  time = 1:1000,
  value = abs(
    c(
      cumsum(rnorm(1000, 0, 3)),
      cumsum(rnorm(1000, 0, 4)),
      cumsum(rnorm(1000, 0, 1)),
      cumsum(rnorm(1000, 0, 2))
    )
  )


 )

gg_area_stacked_ver_CatDatNum.(dfCdn)
gg_area_stacked_hor_CatDatNum.(dfCdn)
gg_area_stacked_100_ver_CatDatNum.(dfCdn)
gg_area_stacked_100_hor_CatDatNum.(dfCdn)

dfCc <- sampleData('Cat-Cat')
gg_area_stacked_hor_CatCat.(dfCc)
gg_area_stacked_ver_CatCat.(dfCc)
gg_area_stacked_100_ver_CatCat.(dfCc)
gg_area_stacked_100_hor_CatCat.(dfCc)

dfCcn <- sampleData('Cat-Cat-Num')
gg_area_stacked_ver_CatCatNum.(dfCcn)
gg_area_stacked_hor_CatCatNum.(dfCcn)
gg_area_stacked_100_ver_CatCatNum.(dfCcn)
gg_area_stacked_100_hor_CatCatNum.(dfCcn)


## Bar plots

df <- sampleData('Cat-Num')
gg_bar_coloured_x_ver_CatNum.(df)
gg_bar_coloured_x_hor_CatNum.(df)
gg_bar_coloured_y_ver_CatNum.(df)
gg_bar_coloured_y_hor_CatNum.(df)
gg_bar_coloured_parameter_ver_CatNum.(df)
gg_bar_coloured_parameter_hor_CatNum.(df)
gg_bar_ordered_ver_CatNum.(df)
gg_bar_ordered_hor_CatNum.(df)
gg_bar_ver_CatNum.(df)
gg_bar_hor_CatNum.(df)
gg_bar_polar_CatNum.(df)
gg_bar_circular_CatNum.(df)
gg_bar_single_stacked_ver_CatNum.(df)
gg_bar_single_stacked_hor_CatNum.(df)

dfYn <- sampleData('Yea-Num')
gg_bar_coloured_x_ver_YeaNum.(dfYn)
gg_bar_coloured_x_hor_YeaNum.(dfYn)
gg_bar_density_y_ver_YeaNum.(dfYn)
gg_bar_density_y_hor_YeaNum.(dfYn)
gg_bar_ver_YeaNum.(dfYn)
gg_bar_hor_YeaNum.(dfYn)

dfDn <- sampleData('Dat-Num')
gg_bar_ver_DatNum.(dfDn)
gg_bar_hor_DatNum.(dfDn)
gg_bar_density_y_ver_DatNum.(dfDn)
gg_bar_density_y_hor_DatNum.(dfDn)

dfCp <- sampleData('Cat-Cat-Cat-Num')
gg_bar_stacked_100_hor_CatCatCatNum.(dfCp)
gg_bar_stacked_100_ver_CatCatCatNum.(dfCp)
gg_bar_stacked_ver_CatCatCatNum.(dfCp)
gg_bar_stacked_hor_CatCatCatNum.(dfCp)

dfC <- sampleData('Cat')
gg_bar_coloured_ver_Cat.(dfC)
gg_bar_coloured_hor_Cat.(dfC)
gg_bar_coloured_parameter_ver_Cat.(dfC)
gg_bar_coloured_parameter_hor_Cat.(dfC)
gg_bar_ver_Cat.(dfC)
gg_bar_hor_Cat.(dfC)
gg_bar_ordered_ver_Cat.(dfC)
gg_bar_ordered_hor_Cat.(dfC)
gg_bar_single_stacked_ver_Cat.(dfC)
gg_bar_single_stacked_hor_Cat.(dfC)

dfCyn <- sampleData('Cat-Yea-Num')
gg_bar_facet_ver_CatYeaNum.(dfCyn)
gg_bar_facet_hor_CatYeaNum.(dfCyn)
gg_bar_grouped_ver_CatYeaNum.(dfCyn)
gg_bar_grouped_hor_CatYeaNum.(dfCyn)
gg_bar_grouped2_ver_CatYeaNum.

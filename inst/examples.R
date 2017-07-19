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

dfCdn <- sampleData('Cat-Dat-Num')
gg_area_stacked_ver_CatDatNum.(dfCdn)
gg_area_stacked_hor_CatDatNum.(dfCdn)
gg_area_stacked_100_ver_CatDatNum.(dfCdn)
gg_area_stacked_100_hor_CatDatNum.(dfCdn)

dfCc <- sampleData('CatCat')
gg_area_stacked_hor_CatCat.(dfCc)
gg_area_stacked_ver_CatCat.(dfCc)
gg_area_stacked_100_ver_CatCat.(dfCc)
gg_area_stacked_100_hor_CatCat.(dfCc)

dfCcn <- sampleData('Cat-Cat-Num')
gg_area_stacked_ver_CatCatNum.(dfCcn)
gg_area_stacked_hor_CatCatNum.(dfCcn)
gg_area_stacked_100_ver_CatCatNum.(dfCcn)
gg_area_stacked_100_hor_CatCatNum.(dfCcn)

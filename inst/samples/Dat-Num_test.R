# Dat-Num

library(devtools)
load_all()
document()
install()


library(ggmagic)

datesA <- seq.Date(as.Date("2015-01-01"),to = Sys.Date(), by = "1 day")
numB <- rnorm(length(datesA))
data <- data.frame(a=datesA, b=cumsum(rnorm(length(datesA))))

#devtools::install_github("jpmarindiaz/datafringe")
gg_horizon_DatNum.(data)

gg_line_DatNum.(data)
gg_line_points_DatNum.(data, hline = 20)

gg_point_DatNum.(data)

gg_box_DatNum.(data, angle_x = 45)

gg_violin_DatNum.(data)

gg_area_DatNum.(data)

gg_kagi_DatNum.(data)

gg_smooth_DatNum.(data)

gg_points_facet_DatNum.(data)
gg_line_points_facet_DatNum.(data)

gg_bar_ver_DatNum.(data)

gg_bubbles_DatNum.(data)

gg_lollipop_DatNum.(data)

gg_area_stepped_DatNum.(data)

gg_waterfall_DatNum.(data)



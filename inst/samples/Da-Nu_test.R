# Da-Nu

library(devtools)
load_all()
document()
install()

library(ggmagic)

datesA <- seq.Date(as.Date("2015-01-01"),to = Sys.Date(), by = "1 day")
numB <- rnorm(length(datesA))
data <- data.frame(a=datesA, b=cumsum(rnorm(length(datesA))))

#devtools::install_github("jpmarindiaz/datafringe")
gg_horizon_DaNu.(data)

gg_lines_DaNu.(data)
gg_lines_points_DaNu.(data, hline = 20)

gg_point_DaNu.(data)

gg_box_DaNu.(data, angle_x = 45)

gg_violin_DaNu.(data)

gg_area_DaNu.(data)

gg_kagi_DaNu.(data)

gg_smooth_DaNu.(data)

gg_points_facet_DaNu.(data)
gg_line_points_facet_DaNu.(data)

gg_bar_DaNu.(data)

gg_bubbles_DaNu.(data)

gg_lollipop_DaNu.(data)

gg_area_stepped_stacked_DaNu.(data)

gg_waterfall_DaNu.(data)



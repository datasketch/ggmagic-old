# Ca-Ye-Nu

library(devtools)
load_all()
document()
install()


data <- sampleData("Ca-Ye-Nu")

gg_bar_stk_ver_CaYeNu.(data)
gg_bar_stk_hor_CaYeNu.(data)

gg_bar_grp_ver_CaYeNu.(data)
gg_bar_grp_hor_CaYeNu.(data)

gg_lines_hor_CaYeNu.(data)
gg_circle_CaYeNu.(data)
gg_steam_CaYeNu.(data, leg_pos = "bottom")


data <- data.frame(hechos = c("secuestro", "secuestro", "delito", "delito", "ex", "ex"),
                   year = c(2007,2015,2007,2015,2007,2015),
                   pop = c(230, 123, 7139, 21597, 1082, 5480))

gg_slope_CaYeNu.(data)

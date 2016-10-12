# Ca-Ye-Nu

library(devtools)
load_all()
document()
install()

library(ggmagic)
data <- sampleData("CaYeNu")

gg_bars_stk_ver_CaYeNu.(data)
gg_bars_grp_ver_CaYeNu.(data)
gg_bars_stk_hor_CaYeNu.(data)
gg_lines_hor_CaYeNu.(data)
gg_circle_CaYeNu.(data)



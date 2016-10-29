
library(devtools)
load_all()
document()
install()

library(ggmagic)

#
# Pie
gg_pie_CaNu.(sampleData("Ca-Nu")) + theme_ds() +
  scale_fill_manual(values = getPalette())

gg_pie_CaNu.(sampleData("Ca-Nu")) + theme_ds_clean() +
  scale_fill_manual(values = getPalette())


data("G20")
dataCaCaCa <- G20 %>% select(Region, Country, Economic.classification)
gg_treemap_x_CaCaCa.(dataCaCaCa)+ theme_ds() + scale_fill_manual(values = getPalette())


ggList()
ggList("bar.*_CaNu\\.")

ggList(wrongNames = TRUE)

ggFtype()
ggFtype("gg_waffle.")
data <- data.frame(sample(letters[1:3],10,replace = TRUE))
ggWhich(data)

run_gg(data,"gg_waffle.")
run_gg(data,"gg_bar_ver.")
run_gg(data,"gg_bar_ver_ord.")



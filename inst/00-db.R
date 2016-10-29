
library(devtools)
load_all()
document()
install()

library(ggmagic)

dsGreen <- "#95C11E"
dsYellow <- "#FFED00"
dsMagenta <- "#E5007D"
dsBlue <- "#009EE3"
dsOrange <- "#FFA500"
dsPalette <- c(dsMagenta,dsBlue,dsGreen,dsOrange,dsYellow)

##
# Pie
gg_pie_CaNu.(sampleData("Ca-Nu")) + theme_ds() + scale_fill_manual(values = dsPalette)



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



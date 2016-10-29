
library(devtools)
load_all()
document()
install()

library(ggmagic)


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



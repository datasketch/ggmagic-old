# Da-Nu

library(devtools)
load_all()
document()
install()

library(ggmagic)

datesA <- seq.Date(as.Date("2016-01-01"),to = Sys.Date(), by = "7 days")
numB <- rnorm(length(datesA))
dataDaNu <- data.frame(a=datesA, b=numB)


data <- dataDaNu

gg_lines_DaNu.(dataDaNu)

gg_scatter_DaNu.(dataDaNu)

gg_box_DaNu.(dataDaNu)

gg_violin_DaNu.(dataDaNu)

gg_area_DaNu.(dataDaNu)

gg_kagi_DaNu.(dataDaNu)

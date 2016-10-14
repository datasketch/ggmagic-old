library(devtools)

load_all()
document()
install()
library(ggmagic)


catA <- rep("Callejero", round(runif(1, 10, 20), digits = 0))
catB <- rep("De raza", round(runif(1, 10, 20), digits = 0))
catC <- rep("Mixto", round(runif(1, 10, 20), digits = 0))

catD <- rep("A", length(catA)*0.8)
catE <- rep("B", length(catA)*0.9)
catF <- rep("C", - length(catD) - length(catE) + length(catA) + length(catB) + length(catC))

dataCaCaNu <- data.frame(c(catA, catB, catC), c(catD, catE, catF))
names(dataCaCaNu)[1] <- "RSGSH"; names(dataCaCaNu)[2] <- "GREHSEZGH"
dataCaCaNu[] <- sapply(dataCaCaNu, as.character)


dataCaCaNu$c <- runif(nrow(dataCaCaNu), min = 0, max = 50)



circleAreaPlotCCN(dataCaCaNu)
flip_circleAreaPlotCCN(dataCaCaNu)

vertical_bargraphCCN(dataCaCaNu)
ordered_vertical_bargraphCCN(dataCaCaNu)
ordered_horizontal_bargraphCCN(dataCaCaNu)
horizontal_bargraphCCN(dataCaCaNu)

vertical_dotgraphCCN(dataCaCaNu)
horizontal_dotgraphCCN(dataCaCaNu)

vertical_unstacked_bargraphCCN(dataCaCaNu)
horizontal_unstacked_bargraphCCN(dataCaCaNu)

horizontal_linegraphCCN(dataCaCaNu)
vertical_linegraphCCN(dataCaCaNu)

gg_stacked_bar_hor_CaCaNu.(dataCaCaNu)
gg_stacked_bar_ver_CaCaNu.(dataCaCaNu)
gg_stacked_bar_100_hor_CaCaNu.(dataCaCaNu)
gg_stacked_bar_100_ver_CaCaNu.(dataCaCaNu)

horizontal_area_bargraphCC(dataCaCaNu)

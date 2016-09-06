
catA <- rep("Callejero", round(runif(1, 10, 20), digits = 0))
catB <- rep("De raza", round(runif(1, 10, 20), digits = 0))
catC <- rep("Mixto", round(runif(1, 10, 20), digits = 0))

dataCat2Num <- data.frame(c(catA, catB, catC), c(catC, catB, catA))
names(dataCat2Num)[1] <- "a"; names(dataCat2Num)[2] <- "b"
dataCat2Num$a <- as.character(dataCat2Num$a); dataCat2Num$b <- as.character(dataCat2Num$b)


dataCat2Num$c <- runif(nrow(dataCat2Num), min = 0, max = 50)



circleAreaPlotCCN(dataCat2Num)
flip_circleAreaPlotCCN(dataCat2Num)

vertical_bargraphCCN(dataCat2Num)
ordered_vertical_bargraphCCN(dataCat2Num)
ordered_horizontal_bargraphCCN(dataCat2Num)
horizontal_bargraphCCN(dataCat2Num)

vertical_dotgraphCCN(dataCat2Num)
horizontal_dotgraphCCN(dataCat2Num)

vertical_unstacked_bargraphCCN(dataCat2Num)
horizontal_unstacked_bargraphCCN(dataCat2Num)

horizontal_linegraphCCN(dataCat2Num)
vertical_linegraphCCN(dataCat2Num)

vertical_stacked_bargraphCCN(dataCat2Num)
horizontal_stacked_bargraphCCN(dataCat2Num)

horizontal_area_bargraphCC(dataCat2Num)

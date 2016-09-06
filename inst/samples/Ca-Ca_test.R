
catA <- rep("Callejero", round(runif(1, 10, 20), digits = 0))
catB <- rep("De raza", round(runif(1, 10, 20), digits = 0))
catC <- rep("Mixto", round(runif(1, 10, 20), digits = 0))

dataCat2 <- data.frame(c(catA, catB, catC), c(catC, catB, catA))
names(dataCat2)[1] <- "a"; names(dataCat2)[2] <- "b"
dataCat2$a <- as.character(dataCat2$a); dataCat2$b <- as.character(dataCat2$b)



#Circle - Area Plot - 2D

circleArea2DPlot(dataCat2, "Reporte de razas mixtas")
flip_circleArea2DPlot(dataCat2)

#Bar Plot 2D
vertical_bargraph2D(dataCat2)
horizontal_bargraph2D(dataCat2)


ordered_vertical_bargraph2D(dataCat2)
ordered_horizontal_bargraph2D(dataCat2)

vertical_dotgraph2D(dataCat2)
horizontal_dotgraph2D(dataCat2)

vertical_unstacked_bargraph2D(dataCat2)
horizontal_unstacked_bargraph2D(dataCat2)

d2 <- data.frame(a=sample(letters[1:7],100,replace=TRUE),
                b=sample(letters[8:13],100,replace=TRUE))

horizontal_linegraph2D(d2)
vertical_linegraph2D(d2)

vertical_stacked_bargraph2D(dataCat2)
horizontal_stacked_bargraph2D(dataCat2)

horizontal_area_bargraph2D(d2)




catA <- rep("Callejero", round(runif(1, 10, 20), digits = 0))
catB <- rep("De raza", round(runif(1, 10, 20), digits = 0))
catC <- rep("Mixto", round(runif(1, 10, 20), digits = 0))
dataCatNum <- data.frame(c(catA, catB, catC))
names(dataCatNum)[1] <- "a"
dataCatNum$a <- as.character(dataCatNum$a)
dataCatNum$b <- runif(nrow(dataCatNum), min = 0, max = 100)


filter1 <- group_by(dataCatNum, a)
a <- summarise(filter1, sum = sum(b))
a <- arrange(a, desc(sum))

titleLabel <-  "hola"
fillLabel <-  "alo"
xLabel <- "Valor 1"
yLabel <- "Valor 2"
voltear <- TRUE
source("Ca-Nu_fun.R")


#barras en coordenadas polares - por variable categórica

flowerGraph (dataCatNum, titleLabel, fillLabel)



#barras en coordenadas polares - por variable numérica
flowerNumGraph (dataCatNum, titleLabel, fillLabel)

#Stacked histogram
barStackedGraph(dataCatNum, titleLabel, xLabel, yLabel,  fillLabel, voltear)


#multiple density, single plot
multDensSingPlot(dataCatNum, titleLabel, xLabel, yLabel, fillLabel, voltear)

#multiple density, split plots
multDensSpltPlot(dataCatNum, titleLabel, xLabel, yLabel, fillLabel, voltear)


#multiple histogram, split plots
densHistSpltPlot(dataCatNum, titleLabel, xLabel, yLabel, fillLabel, voltear)

#boxplots

boxSpltPlot(dataCatNum, titleLabel, xLabel, yLabel, fillLabel, voltear)

#violin plots

ViolinMultPlot(dataCatNum, titleLabel, xLabel, yLabel, fillLabel, voltear)


#violin plots + obs. dots
ViolinDotMultPlot(dataCatNum, titleLabel, xLabel, yLabel, fillLabel, voltear)

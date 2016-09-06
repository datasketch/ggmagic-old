

catA <- rep("Callejero", round(runif(1, 10, 20), digits = 0))
catB <- rep("De raza", round(runif(1, 10, 20), digits = 0))
catC <- rep("Mixto", round(runif(1, 10, 20), digits = 0))
dataCat <- data.frame(c(catA, catB, catC))
names(dataCat) <- "a"

# Waffle plot
wafgraph(dataCat, 5, "Reporte \n de conteo de perros")

d <- data.frame(a=sample(letters[1:7],10000,replace=TRUE))

wafgraph(dataCat)

#Wordcloud
wcgraph(dataCat)
# min.freq=1 por defecto
# Parámetros:
# minScale y maxScale

# BARS
vertical_bargraph(dataCat,leg_pos = "top")
horizontal_bargraph(d,leg_pos = "bottom")

ordered_vertical_bargraph(dataCat)
ordered_horizontal_bargraph(dataCat)

#PIE
piegraph(data=dataCat, fillLabel = "Tipo", leg_pos = "right")

#DONUT
donutgraph(dataCat, "Tipos", width = 0.2)

# PILE CHART
vertical_dotgraph(dataCat)
horizontal_dotgraph(dataCat)
#Dot chart
# add options:
# change shape
# group (bin) values <- works only for divisible bins
# theme
# remove background
# rename ticks for var names
# add legends and caption

# LINE
horizontal_linegraph(d)
vertical_linegraph(d)

# GAUGE
gaugeGraph(dataCat) #Gauge Chart
# title, caption
# breaks option


#CIRCLE AREA PLOT
horizontal_circleAreaPlot(d, leg_pos = "top")
vertical_circleAreaPlot(d, leg_pos = "left")

#BAR POLAR GRAPH
barPolarGraph(dataCat, width = 1)

# STACKED BAR GRAPH
horizontal_barStackGraph(dataCat)
vertical_barStackGraph(dataCat)

# BULL'S EYE BAR GRAPH
bullsEyeGraph(dataCat)

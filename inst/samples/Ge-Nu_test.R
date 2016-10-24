library(devtools)
load_all()
document()
install()

deptos <- c("05", "08", "11", "13", "15", "17", "18", "19", "20", "23", "25", "27", "41", "44", "47", "50")
dataGeNu <- data.frame(id = deptos, num = runif(length(deptos), 0, 1))

gg_choropleth_co_GeNu.(dataGeNu)




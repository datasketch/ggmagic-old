library(devtools)
load_all()
document()
install()

deptos <- c("05", "08", "11", "13", "15", "17", "18", "19", "20", "23", "25", "27", "41", "44", "47", "50")
dataGeNu <- data.frame(id = deptos, num = runif(length(deptos), 0, 1))

gg_choropleth_co_GeNu.(dataGeNu)

depto_ <- c("05")
mpios <- c("05045", "05051", "05055", "05059", "05079", "05086", "05088")
dataGeNu2 <- data.frame(id = mpios, num = runif(length(mpios), 0, 1))

gg_choropleth_depto_GeNu.(dataGeNu2, depto_ = depto_)

lat_max <- 6.276997
long_max <- -69.635037
lat_min <- 2.342537
long_min <- -76.937433

long <- runif(100, long_min, long_max)
lat <- runif(100, lat_min, lat_max)

dataGeNu3 <- data.frame(long = long, lat = lat) #num = round(runif(length(lat), 1, 5), 0))

gg_point_co_GeNu.(dataGeNu3)

lat_max <- 3.644543
long_max <- -76.247046
lat_min <- 3.363461
long_min <- -76.804436

depto_ <- "76"
long <- runif(25, long_min, long_max)
lat <- runif(25, lat_min, lat_max)

dataGeNu4 <- data.frame(long = long, lat = lat) #num = round(runif(length(lat), 1, 5), 0))

gg_point_depto_GeNu.(dataGeNu4, depto_ = depto_)

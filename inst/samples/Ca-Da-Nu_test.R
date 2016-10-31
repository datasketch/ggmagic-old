# Ca-Da

library(devtools)
load_all()
document()
install()
library(ggmagic)

dataCaDaNu <- data.frame(proj = paste0("p",sample(LETTERS[5:7],50, replace=TRUE)),
                   date = sample(seq(as.Date('1999-01-01'), as.Date('2002-01-01'), by="day"), 50),
                   value= 100*runif(50)
)

Time = seq.Date(as.Date("2015-01-01"),to = Sys.Date(), by = "1 day")
dfData = data.frame(
  Ca = c(rep('Almacén A', length(Time)), rep('Almacén B', length(Time)),
                    rep('Almacén C', length(Time)), rep('Almacén D', length(Time))),
  Da = Time,
  Nu = abs(
    c(
      cumsum(rnorm(length(Time), 0, 3)),
      cumsum(rnorm(length(Time), 0, 4)),
      cumsum(rnorm(length(Time), 0, 1)),
      cumsum(rnorm(length(Time), 0, 2))
    )
  )
)

gg_scatter_hor_CaDaNu.(dataCaDaNu)

gg_stream_CaDaNu.(dfData)

gg_area_stacked_ver_CaDaNu.(dfData)
gg_area_stacked_hor_CaDaNu.(dfData)
gg_area_stacked_100_ver_CaDaNu.(dfData)
gg_area_stacked_100_hor_CaDaNu.(dfData)



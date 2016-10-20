# Ca-Da

library(devtools)
load_all()
document()
#install()

dataCaDaNu <- data.frame(proj = paste0("p",sample(LETTERS[5:7],50, replace=TRUE)),
                   date = sample(seq(as.Date('1999-01-01'), as.Date('2002-01-01'), by="day"), 50),
                   value= 100*runif(50)
)

Time = seq.Date(as.Date("2015-01-01"),to = Sys.Date(), by = "1 day")
dfData = data.frame(
  FSRG = c(rep('Almacén', length(Time)), rep('Almacén B', length(Time)),
                    rep('Almacén C', length(Time)), rep('Almacén D', length(Time))),
  BRTN = Time,
  NRYN = abs(
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





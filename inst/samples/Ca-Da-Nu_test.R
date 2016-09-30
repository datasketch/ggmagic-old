# Ca-Da

library(devtools)
load_all()
document()
#install()

data <- data.frame(proj = paste0("p",sample(LETTERS[5:7],50, replace=TRUE)),
                   date = sample(seq(as.Date('1999-01-01'), as.Date('2002-01-01'), by="day"), 50),
                   value= 100*runif(50)
)
gg_scatter_hor_CaDaNu.(data)







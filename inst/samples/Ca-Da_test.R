# Ca-Da

library(devtools)
load_all()
document()
#install()

data <- data.frame(proj = paste0("p",sample(LETTERS[5:7],50, replace=TRUE)),
                 date = sample(seq(as.Date('1999-01-01'), as.Date('2002-01-01'), by="day"), 50)
                   )


gg_pointline_hor_CaDa.(data)
gg_pointline_ver_CaDa.(data)


data <- data.frame(proj = paste0("p",sample(LETTERS[5:7],50, replace=TRUE)),
                   date = sample(seq(as.Date('1999-01-01'), as.Date('1999-02-01'), by="day"), 200,replace = TRUE)
)

gg_histogram_CaDa.(data)

### OJO

# Ca-Da
# se puede transformar en Ca-Da-Nu





library(devtools)
document()
install()
load_all()


data <- data.frame(a = rep(LETTERS[1:3],10),  b = rnorm(10), c = rnorm(10))

gg_lines_CaNuNu.(data)
gg_point_CaNuNu.(data)
gg_steamgraph_CaNuNu(data)

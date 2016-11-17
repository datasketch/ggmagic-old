library(devtools)
document()
install()
load_all()


data <- data.frame(a = sample(LETTERS[1:3], replace = TRUE, 30),  b = rnorm(10), c = rnorm(10))

gg_lines_CaNuNu.(data)
gg_point_CaNuNu.(data, shape_point = 8)
gg_steamgraph_CaNuNu.(data)



library(devtools)
document()
install()
load_all()


data <- data.frame(a = sample(LETTERS[1:3], replace = TRUE, 30),  b = rnorm(10), c = rnorm(10))

gg_lines_CaNuNu.(data)
gg_point_CaNuNu.(data, shape_point = 8)
gg_steamgraph_CaNuNu.(data)

data <- data.frame(hechos = c("secuestro", "secuestro", "delito", "delito", "ex", "ex"),
                   year = c(130,160,5923,21563,952, 4565),
                   pop = c(230, 123, 7139, 21597, 1082, 5480))

gg_slope_CaNuNu.(data)

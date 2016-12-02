# Ca-Ye-Nu

library(devtools)
load_all()
document()
#install()


data <- sampleData("Ca-Ye-Nu")

gg_bar_stk_ver_CaYeNu.(data)
gg_bar_stk_hor_CaYeNu.(data)

gg_bar_grp_ver_CaYeNu.(data)
gg_bar_grp_hor_CaYeNu.(data)


data <- data.frame(hechos = c("secuestro","secuestro", "secuestro", "delito", "delito", "ex", "ex"),
                   year = c(2006,2007,2015,2007,2015,2007,2015),
                   pop = c(432,230, 123, 7139, 21597, 1082, 5480))
gg_lines_hor_CaYeNu.(data)

gg_circle_CaYeNu.(data)
gg_steam_CaYeNu.(data, leg_pos = "bottom")


data <- data.frame(hechos = c("secuestro", "secuestro", "delito", "delito", "ex", "ex"),
                   year = c(2007,2015,2007,2015,2007,2015),
                   pop = c(230, 123, 7139, 21597, 1082, 5480))

gg_slope_CaYeNu.(data)

pais <- c("Argentina", "Chile", "Colombia", "Argentina", "Chile", "Colombia")
año <- c(1970,1970,1970,2014,2014,2014)
indice <- round(c(0.4630111792, 6.072004637,1.18313905,2.704745925,7.951126245,6.829719025), 3)
data <- data.frame(`País` = pais, `Año` = año, `Porcentaje INB` = indice)
gg_slope_CaYeNu.(data, size_vjust = 0.5, size_hjust = 0.5,
                 titleLabel = "Agotamiento de recursos \n naturales para los años 1970 y 2014",
                 leg_pos = "bottom")


municipio <- c("Anolaima", "Chía", "Cajicá", "Tocancipá")
año <- c(1990, 1990, 1990, 1990,2013, 2013, 2013, 2013)
values <- c(10, 29, 17, 8, 1,5,7,5)
data <- data.frame(`Municipio` = municipio, `Año` = año, `homicidios` = values)
gg_slope_CaYeNu.(data, size_vjust = 1.5, size_hjust = 0.5,
                 titleLabel = "Homicidios en municipios Cundinamarca \n para los años 1990 y 2015",
                 leg_pos = "bottom")



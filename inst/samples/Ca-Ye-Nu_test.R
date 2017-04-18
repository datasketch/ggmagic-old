# Cat-Yea-Num

library(devtools)
load_all()
document()
install()


data <- sampleData("Cat-Yea-Num", nrow = 10, gt0 = TRUE)

# Bars

gg_bar_grouped_ver_CatYeaNum.(data)
gg_bar_grouped_hor_CatYeaNum.(data)

gg_bar_grouped2_ver_CatYeaNum.(data)
gg_bar_grouped2_hor_CatYeaNum.(data)

gg_bar_facet_ver_CatYeaNum.(data)
gg_bar_facet_hor_CatYeaNum.(data)

gg_bar_stacked_100_ver_CatYeaNum.(data)
gg_bar_stacked_100_hor_CatYeaNum.(data)

gg_bar_stacked_ver_CatYeaNum.(data)
gg_bar_stacked_hor_CatYeaNum.(data)




# Lines
data <- sampleData("Cat-Yea-Num", nrow = 20, gt0 = TRUE)
gg_line_hor_CatYeaNum.(data)
gg_line_hor_CatYeaNum.(data, symbol = 1)




# Circle

gg_circle_CatYeaNum.(data)
gg_steam_CatYeaNum.(data, leg_pos = "bottom")


data <- data.frame(hechos = c("secuestro", "secuestro", "delito", "delito", "ex", "ex"),
                   year = c(2007,2015,2007,2015,2007,2015),
                   pop = c(230, 123, 7139, 21597, 1082, 5480))

gg_slope_CatYeaNum.(data)

pais <- c("Argentina", "Chile", "Colombia", "Argentina", "Chile", "Colombia")
año <- c(1970,1970,1970,2014,2014,2014)
indice <- round(c(0.4630111792, 6.072004637,1.18313905,2.704745925,7.951126245,6.829719025), 3)
data <- data.frame(`País` = pais, `Año` = año, `Porcentaje INB` = indice)
gg_slope_CatYeaNum.(data, size_vjust = 0.5, size_hjust = 0.5,
                 titleLabel = "Agotamiento de recursos \n naturales para los años 1970 y 2014",
                 leg_pos = "bottom")


municipio <- c("Anolaima", "Chía", "Catjicá", "Tocancipá")
año <- c(1990, 1990, 1990, 1990,2013, 2013, 2013, 2013)
values <- c(10, 29, 17, 8, 1,5,7,5)
data <- data.frame(`Municipio` = municipio, `Año` = año, `homicidios` = values)
gg_slope_CatYeaNum.(data, size_vjust = 1.5, size_hjust = 0.5,
                 titleLabel = "Homicidios en municipios Cundinamarca \n para los años 1990 y 2015",
                 leg_pos = "bottom")



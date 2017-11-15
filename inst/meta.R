library(tidyverse)
library(homodatum)
# Create meta data for funs
db <- tools::Rd_db("ggmagic")
if(length(db)==0) stop("Restart session")
f <- function(rd){
  #rd <- db[[1]]
  rd <- capture.output(rd)
  con <- textConnection(rd)
  l <- Rd2roxygen::parse_file(con)
  l <- map_if(l,~length(.)==0,function(x)'')
  map(l,paste,collapse = "_")
}
funs <- map(db,f)
funsMeta <- funs %>% bind_rows()
funsMeta <- funsMeta %>% filter(grepl("^gg_",name))
meta <- funsMeta[c("name", "title", "desc", "section")]
meta$ctypes <- stringr::str_extract(meta$section,"(?<=\n).*?(?=\n)$")
meta$section <- NULL
meta$ftype <- ctypesToFtype(meta$ctypes, as_string = TRUE)
meta$group <- stringr::str_extract(meta$name,"(?<=_).*?(?=_)")
meta <- left_join(meta, metaApp, by = "name")
write_csv(meta, file.path("inst","meta.csv"))


## meta-status
metaApp <- data.frame("name" = c("gg_bar_circular_Cat.", "gg_bar_circular_CatNum.", "gg_bar_circular_facet_CatCat.", "gg_bar_polar_Cat.",
                                 "gg_bar_polar_CatNum.", "gg_bar_polar_stacked_100_CatCat.", "gg_bar_polar_stacked_CatCat.",
                                 "gg_bar_circular_facet_CatCatNum.", "gg_bar_stacked_polar_100_CatCatNum.", "gg_bar_stacked_polar_CatCatNum.",
                                 "gg_bar_dot_ver_CatNum", "gg_bar_dot_hor_CatNum", "gg_bar_coloured_facet_x_hor_CatCat", "gg_bar_coloured_facet_x_hor_CatCatNum",
                                 "gg_bar_coloured_facet_x_ver_CatCat", "gg_bar_coloured_facet_x_ver_CatCatNum", "gg_bar_coloured_facet_y_hor_CatCat",
                                 "gg_bar_coloured_facet_y_hor_CatCatNum", "gg_bar_coloured_facet_y_ver_CatCat", "gg_bar_coloured_facet_y_ver_CatCatNum",
                                 "gg_bar_coloured_hor_Cat", "gg_bar_coloured_parameter_facet_hor_CatCat", "gg_bar_coloured_parameter_facet_hor_CatCatNum",
                                 "gg_bar_coloured_parameter_facet_ver_CatCat", "gg_bar_coloured_parameter_facet_ver_CatCatNum", "gg_bar_coloured_parameter_hor_Cat",
                                 "gg_bar_coloured_parameter_hor_CatNum", "gg_bar_coloured_parameter_ver_Cat", "gg_bar_coloured_parameter_ver_CatNum",
                                 "gg_bar_coloured_ver_Cat", "gg_bar_coloured_x_hor_CatNum", "gg_bar_coloured_x_hor_YeaNum", "gg_bar_coloured_x_ver_CatNum",
                                 "gg_bar_coloured_x_ver_YeaNum", "gg_bar_density_facet_z_hor_CatCatNum", "gg_bar_density_facet_z_ver_CatCatNum",
                                 "gg_bar_density_y_hor_CatNum", "gg_bar_density_y_hor_DatNum", "gg_bar_density_y_hor_YeaNum",
                                 "gg_bar_density_y_ver_CatNum", "gg_bar_density_y_ver_DatNum", "gg_bar_density_y_ver_YeaNum",
                                 "gg_bar_dot_facet_hor_CatCat", "gg_bar_dot_facet_ver_CatCat", "gg_bar_dot_hor_Cat",
                                 "gg_bar_dot_ver_Cat", "gg_bar_facet_hor_CatCatNum", "gg_bar_facet_hor_CatYeaNum",
                                 "gg_bar_facet_ver_CatCatNum", "gg_bar_facet_ver_CatYeaNum", "gg_bar_grouped2_hor_CatCatNum",
                                 "gg_bar_grouped2_hor_CatYeaNum", "gg_bar_grouped2_ver_CatCatNum", "gg_bar_grouped2_ver_CatYeaNum",
                                 "gg_bar_grouped_coloured_hor_CatCat", "gg_bar_grouped_coloured_ver_CatCat", "gg_bar_grouped_hor_CatCatNum",
                                 "gg_bar_grouped_hor_CatYeaNum", "gg_bar_grouped_ver_CatCatNum", "gg_bar_grouped_ver_CatYeaNum",
                                 "gg_bar_hor_Cat", "gg_bar_hor_CatNum", "gg_bar_hor_DatNum", "gg_bar_hor_YeaNum", "gg_bar_ordered_hor_Cat",
                                 "gg_bar_ordered_hor_CatNum", "gg_bar_ordered_ver_Cat", "gg_bar_ordered_ver_CatNum",
                                 "gg_bar_single_stacked_hor_Cat", "gg_bar_single_stacked_hor_CatNum", "gg_bar_single_stacked_ver_Cat",
                                 "gg_bar_single_stacked_ver_CatNum", "gg_bar_stacked2_100_hor_CatCatNum", "gg_bar_stacked2_100_ver_CatCatNum",
                                 "gg_bar_stacked2_hor_CatCatNum", "gg_bar_stacked2_ver_CatCatNum", "gg_bar_stacked_100_hor_CatCat",
                                 "gg_bar_stacked_100_hor_CatCatCatNum", "gg_bar_stacked_100_hor_CatCatNum", "gg_bar_stacked_100_hor_CatCatYeaNum",
                                 "gg_bar_stacked_100_hor_CatYeaNum", "gg_bar_stacked_100_ver_CatCat", "gg_bar_stacked_100_ver_CatCatCatNum",
                                 "gg_bar_stacked_100_ver_CatCatNum", "gg_bar_stacked_100_ver_CatCatYeaNum", "gg_bar_stacked_100_ver_CatYeaNum",
                                 "gg_bar_stacked_dot_hor_CatCat", "gg_bar_stacked_dot_ver_CatCat", "gg_bar_stacked_hor_CatCat",
                                 "gg_bar_stacked_hor_CatCatCatNum", "gg_bar_stacked_hor_CatCatNum", "gg_bar_stacked_hor_CatCatYeaNum",
                                 "gg_bar_stacked_hor_CatDatNum", "gg_bar_stacked_hor_CatYeaNum", "gg_bar_stacked_ordered_hor_CatCat",
                                 "gg_bar_stacked_ordered_ver_CatCat", "gg_bar_stacked_ver_CatCat", "gg_bar_stacked_ver_CatCatCatNum",
                                 "gg_bar_stacked_ver_CatCatNum", "gg_bar_stacked_ver_CatCatYeaNum", "gg_bar_stacked_ver_CatDatNum",
                                 "gg_bar_stacked_ver_CatYeaNum", "gg_bar_ver_Cat", "gg_bar_ver_CatNum", "gg_bar_ver_DatNum", "gg_bar_ver_YeaNum"),
                      "app" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, rep("OK", 94)))
metaStatus <- data.frame("name" = c(),



                         "status" = c("OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", NA, NA,

                                      "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK",
                                      "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK",
                                      "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK",
                                      "OK", NA, NA,

                                      "OK", "OK", "OK", "OK", "OK", "OK", NA, "OK", "OK", "OK", "OK", NA,

                                      "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK", "OK",

                                      "OK", "OK", "OK", "OK", "OK", "OK", "OK",

                                      "OK", "OK", "OK", "OK", "OK"),

                         "coments" = c("Esta gráfica no sirve cuando hay un sólo dato. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       NA,
                                       "Esta gráfica no sirve cuando hay un sólo dato. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Esta gráfica no sirve cuando hay un sólo dato en alguna categoría. Se podrían agregar los marcadores cuando el número de observaciones es pequeño.
                El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Falta revisar y habilitar función.", "Falta revisar y habilitar función.",

                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas. ",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas. ",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas. ",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas. ",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas. ",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas. ",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas. ",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas. ",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "El eje 'y' debería centrarse en 0 (cuando hay valores negativos). Argumento que controle los separadores decimales y de miles en las variables numéricas.",
                                       "Sólo sirve para variables con pocos niveles categóricos",
                                       NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,

                                       "Ordenar los años", "Ordenar los años", "Ordenar los años", "Ordenar los años", "Ordenar los años", NA, NA, NA, NA, NA, NA, NA,

                                       NA, NA, NA, NA, NA, NA, NA, NA, NA,

                                       NA, NA, NA, NA, NA, NA, NA,

                                       NA, NA, NA, NA, NA)
)
#metaStatus <- left_join(select(hgchmagic::hgchMeta(), name), metaStatus, by = name)
write_csv(metaStatus, "inst/meta-status.csv")

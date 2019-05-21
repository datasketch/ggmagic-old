library(devtools)
load_all()
document()
install()

library(ggmagic)
library(hgchmagic)

# por default queda color_scale discrete...¿?
# poner función de getOptions en transformation_helpers
# colores discrete en barras "hor" salen reversados en ggmagic y hgchmagic, más
# bien el eje está reversado, pasa en area y líneas también
# ggmagic el texto de show_text que salga en bold como en
# ¿para qué es lo de los breaks del eje y?
# opts$horLine matches opts$horLine_label problemas a veces...[["horLine"]]
# ¿area CatNum siempre color_scale no?
# ¿líneas CatNum siempre color_scale no?
# en ggmagic el máximo valor del eje el mayor que en hgchmagic
# area CatCat "stacked" sale rara en ggmagic, revisar con hgchmagic
# líneas CatCat... ¿las categorías que no tienen valor se ponen en 0?
# en ggmagic parece estar así en hgchmagic no
# en CatCat de ggmagic y hgchmagic salen labels repetidos eje y
# en pie y donut la opción de que salga conteo y porcentaje ¿?
# se puede poner geom_label en vez de geom_text y queda con un recuadro del
# mismo color (se ve más, como en los judíos)
# que labelratio sirva en treemap

s0 <- sampleData("Cat")

# faltan las divisiones del eje y (calcularlas) ggplot

gg_bar_Cat(s0, opts = list(orientation = "hor"))
hgch_bar_Cat(s0, orientation = "hor") # color scale no... salen distintos
gg_bar_Cat(s0, opts = list(label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete"))
hgch_bar_Cat(s0, label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete")
gg_bar_Cat(s0, opts = list(label_ratio = 1, color_scale = "no", colors = c("blue", "red")))
hgch_bar_Cat(s0, label_ratio = 1, color_scale = "no", colors = c("blue", "red")) # ¿no funciona el color?

gg_bar_Cat(s0, opts = list(percentage = TRUE))
hgch_bar_Cat(s0, percentage = TRUE)
gg_bar_Cat(s0, opts = list(label_ratio = 0.5, percentage = TRUE, color_scale = "continuous"))
hgch_bar_Cat(s0, label_ratio = 0.5, percentage = TRUE, color_scale = "continuous") # parece que no sale escala continua
gg_bar_Cat(s0, opts = list(label_ratio = 1, percentage = TRUE, suffix ="pres"))
hgch_bar_Cat(s0, label_ratio = 1, percentage = TRUE, suffix ="pres") # no sale sufijo que reemplazaría porcentaje

gg_area_Cat(s0) # los labels salen un poco montados en los puntos, si se agranda la ventana se arregla
hgch_area_Cat(s0) # color default amarillo en hgchmagic, verde en ggmagic
gg_area_Cat(s0, opts = list(label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj"))
hgch_area_Cat(s0, label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj") # no salen sufijo ni prefixo en labels
gg_area_Cat(s0, opts = list(label_ratio = 1, color_scale = "no", colors = c("blue", "red")))
hgch_area_Cat(s0, label_ratio = 1, color_scale = "no", colors = c("blue", "red")) # ¿no funciona el color?

gg_area_Cat(s0, opts = list(percentage = TRUE, orientation = "hor"))
hgch_area_Cat(s0, percentage = TRUE) # no sale porcentaje en labels ni sufijo
gg_area_Cat(s0, opts = list(label_ratio = 0.5, percentage = TRUE, color_scale = "continuous")) # salen los puntos de colores continuous, ¿no debería pasar?
hgch_area_Cat(s0, label_ratio = 0.5, percentage = TRUE, color_scale = "continuous") # no sale sufijos en labels (en particular el porcentaje)
gg_area_Cat(s0, opts = list(label_ratio = 1, percentage = TRUE, suffix ="pres"))
hgch_area_Cat(s0, label_ratio = 1, percentage = TRUE, suffix ="pres")

gg_line_Cat(s0) # color_scale = "discrete" ¿dejarla así?
hgch_line_Cat(s0)
gg_line_Cat(s0, opts = list(label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete"))
hgch_line_Cat(s0, label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete")
gg_line_Cat(s0, opts = list(label_ratio = 1, color_scale = "no", colors = c("blue", "red")))
hgch_line_Cat(s0, label_ratio = 1, color_scale = "no", colors = c("blue", "red"))

gg_line_Cat(s0, opts = list(percentage = TRUE, orientation = "hor"))
hgch_line_Cat(s0, percentage = TRUE, orientation = "hor")
gg_line_Cat(s0, opts = list(label_ratio = 0.5, percentage = TRUE, color_scale = "continuous"))
hgch_line_Cat(s0, label_ratio = 0.5, percentage = TRUE, color_scale = "continuous")
gg_line_Cat(s0, opts = list(label_ratio = 1, percentage = TRUE, suffix = "pres"))
hgch_line_Cat(s0, label_ratio = 1, percentage = TRUE, suffix = "pres")

gg_pie_Cat(s0)
hgch_pie_Cat(s0)
gg_pie_Cat(s0, opts = list(label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete"))
hgch_pie_Cat(s0, label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete")
gg_pie_Cat(s0, opts = list(label_ratio = 1, color_scale = "no", colors = c("blue", "red")))
hgch_pie_Cat(s0, label_ratio = 1, color_scale = "no", colors = c("blue", "red"))

gg_pie_Cat(s0, opts = list(percentage = TRUE, legend_position = "top"))
hgch_pie_Cat(s0, percentage = TRUE)
gg_pie_Cat(s0, opts = list(label_ratio = 0.5, percentage = TRUE, color_scale = "continuous"))
hgch_pie_Cat(s0, label_ratio = 0.5, percentage = TRUE, color_scale = "continuous")
gg_pie_Cat(s0, opts = list(label_ratio = 1.5, percentage = TRUE, suffix = "pres"))
hgch_pie_Cat(s0, label_ratio = 1.5, percentage = TRUE, suffix = "pres")

gg_donut_Cat(s0)
hgch_donut_Cat(s0)
gg_donut_Cat(s0, opts = list(label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete"))
hgch_donut_Cat(s0, label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete")
gg_donut_Cat(s0, opts = list(label_ratio = 1, color_scale = "no", colors = c("blue", "red")))
hgch_donut_Cat(s0, label_ratio = 1, color_scale = "no", colors = c("blue", "red"))

gg_donut_Cat(s0, opts = list(percentage = TRUE, legend_position = "top"))
hgch_donut_Cat(s0, percentage = TRUE)
gg_donut_Cat(s0, opts = list(label_ratio = 0.5, percentage = TRUE, color_scale = "continuous"))
hgch_donut_Cat(s0, label_ratio = 0.5, percentage = TRUE, color_scale = "continuous")
gg_donut_Cat(s0, opts = list(label_ratio = 1.5, percentage = TRUE, suffix = "pres"))
hgch_donut_Cat(s0, label_ratio = 1.5, percentage = TRUE, suffix = "pres")

gg_treemap_Cat(s0)
hgch_treemap_Cat(s0)
gg_treemap_Cat(s0, opts = list(prefix = "dskk", suffix = " jdjdj", color_scale = "discrete"))
hgch_treemap_Cat(s0, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete")
gg_treemap_Cat(s0, opts = list(color_scale = "no", colors = c("blue", "red")))
hgch_treemap_Cat(s0, label_ratio = 1, color_scale = "no", colors = c("blue", "red"))

gg_treemap_Cat(s0, opts = list(percentage = TRUE, legend_position = "top"))
hgch_treemap_Cat(s0, percentage = TRUE)
gg_treemap_Cat(s0, opts = list(label_ratio = 0.5, percentage = TRUE, color_scale = "continuous"))
hgch_treemap_Cat(s0, label_ratio = 0.5, percentage = TRUE, color_scale = "continuous")
gg_treemap_Cat(s0, opts = list(label_ratio = 1.5, percentage = TRUE, suffix = "pres"))
hgch_treemap_Cat(s0, label_ratio = 1.5, percentage = TRUE, suffix = "pres")

s1 <- sampleData("Cat-Num")
s1 <- rbind(s1, c("tT", 2))

gg_bar_CatNum(s1)
hgch_bar_CatNum(s1)
gg_bar_CatNum(s1, opts = list(label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete"))
hgch_bar_CatNum(s1, label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete")
gg_bar_CatNum(s1, opts = list(label_ratio = 1, color_scale = "no", colors = c("blue", "red")))
hgch_bar_CatNum(s1, label_ratio = 1, color_scale = "no", colors = c("blue", "red")) # salen distintos colores y no salen los marks (punto en miles) en los labels

gg_bar_CatNum(s1, opts = list(percentage = TRUE,  orientation = "hor"))
hgch_bar_CatNum(s1, percentage = TRUE, orientation = "hor")
gg_bar_CatNum(s1, opts = list(label_ratio = 0.5, percentage = TRUE, color_scale = "continuous"))
hgch_bar_CatNum(s1, label_ratio = 0.5, percentage = TRUE, color_scale = "continuous")
gg_bar_CatNum(s1, opts = list(label_ratio = 1, percentage = TRUE, suffix ="pres"))
hgch_bar_CatNum(s1, label_ratio = 1, percentage = TRUE, suffix ="pres") # prioridad sufijo

gg_area_CatNum(s1)
hgch_area_CatNum(s1)
gg_area_CatNum(s1, opts = list(label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj"))
hgch_area_CatNum(s1, label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj")
gg_area_CatNum(s1, opts = list(label_ratio = 1, color_scale = "no", colors = c("blue", "red")))
hgch_area_CatNum(s1, label_ratio = 1, color_scale = "no", colors = c("blue", "red"))

gg_area_CatNum(s1, opts = list(percentage = TRUE, orientation = "hor"))
hgch_area_CatNum(s1, percentage = TRUE, orientation = "hor") #falta porcentaje, sufijos en labels
gg_area_CatNum(s1, opts = list(label_ratio = 0.5, percentage = TRUE, color_scale = "continuous"))
hgch_area_CatNum(s1, label_ratio = 0.5, percentage = TRUE, color_scale = "continuous")
gg_area_CatNum(s1, opts = list(label_ratio = 1, percentage = TRUE, suffix ="pres"))
hgch_area_CatNum(s1, label_ratio = 1, percentage = TRUE, suffix ="pres")

gg_line_CatNum(s1)
hgch_line_CatNum(s1)
gg_line_CatNum(s1, opts = list(label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete"))
hgch_line_CatNum(s1, label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete")
gg_line_CatNum(s1, opts = list(label_ratio = 1, color_scale = "no", colors = c("blue", "red")))
hgch_line_CatNum(s1, label_ratio = 1, color_scale = "no", colors = c("blue", "red"))

gg_line_CatNum(s1, opts = list(percentage = TRUE, orientation = "hor"))
hgch_line_CatNum(s1, percentage = TRUE)
gg_line_CatNum(s1, opts = list(label_ratio = 0.5, percentage = TRUE, color_scale = "continuous"))
hgch_line_CatNum(s1, label_ratio = 0.5, percentage = TRUE, color_scale = "continuous")
gg_line_CatNum(s1, opts = list(label_ratio = 1, percentage = TRUE, suffix ="pres"))
hgch_line_CatNum(s1, label_ratio = 1, percentage = TRUE, suffix ="pres")

gg_pie_CatNum(s1)
hgch_pie_CatNum(s1)
gg_pie_CatNum(s1, opts = list(label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete"))
hgch_pie_CatNum(s1, label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete")
gg_pie_CatNum(s1, opts = list(label_ratio = 1.5, color_scale = "no", colors = c("blue", "red")))
hgch_pie_CatNum(s1, label_ratio = 1, color_scale = "no", colors = c("blue", "red"))

gg_pie_CatNum(s1, opts = list(percentage = TRUE, legend_position = "top"))
hgch_pie_CatNum(s1, percentage = TRUE)
gg_pie_CatNum(s1, opts = list(label_ratio = 0.5, percentage = TRUE, color_scale = "continuous"))
hgch_pie_CatNum(s1, label_ratio = 0.5, percentage = TRUE, color_scale = "continuous")
gg_pie_CatNum(s1, opts = list(label_ratio = 1.5, percentage = TRUE, suffix ="pres"))
hgch_pie_CatNum(s1, label_ratio = 1, percentage = TRUE, suffix ="pres")

gg_donut_CatNum(s1)
hgch_donut_CatNum(s1)
gg_donut_CatNum(s1, opts = list(label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete"))
hgch_donut_CatNum(s1, label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete")
gg_donut_CatNum(s1, opts = list(label_ratio = 1.5, color_scale = "no", colors = c("blue", "red")))
hgch_donut_CatNum(s1, label_ratio = 1, color_scale = "no", colors = c("blue", "red"))

gg_donut_CatNum(s1, opts = list(percentage = TRUE, legend_position = "top"))
hgch_donut_CatNum(s1, percentage = TRUE)
gg_donut_CatNum(s1, opts = list(label_ratio = 0.5, percentage = TRUE, color_scale = "continuous"))
hgch_donut_CatNum(s1, label_ratio = 0.5, percentage = TRUE, color_scale = "continuous")
gg_donut_CatNum(s1, opts = list(label_ratio = 1.5, percentage = TRUE, suffix ="pres"))
hgch_donut_CatNum(s1, label_ratio = 1, percentage = TRUE, suffix ="pres")

gg_treemap_CatNum(s1)
hgch_treemap_CatNum(s1)
gg_treemap_CatNum(s1, opts = list(prefix = "dskk", suffix = " jdjdj", color_scale = "discrete"))
hgch_treemap_CatNum(s1, label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj", color_scale = "discrete")
gg_treemap_CatNum(s1, opts = list(color_scale = "no", colors = c("blue", "red")))
hgch_treemap_CatNum(s1, label_ratio = 1, color_scale = "no", colors = c("blue", "red"))

gg_treemap_CatNum(s1, opts = list(percentage = TRUE, legend_position = "top"))
hgch_treemap_CatNum(s1, percentage = TRUE)
gg_treemap_CatNum(s1, opts = list(label_ratio = 0.5, percentage = TRUE, color_scale = "continuous"))
hgch_treemap_CatNum(s1, label_ratio = 0.5, percentage = TRUE, color_scale = "continuous")
gg_treemap_CatNum(s1, opts = list(label_ratio = 1.5, percentage = TRUE, suffix ="pres"))
hgch_treemap_CatNum(s1, label_ratio = 1, percentage = TRUE, suffix ="pres")


s2 <- sampleData("Cat-Cat")

# mejorar la división de los ejes
gg_bar_CatCat(s2) # se repiten labels eje y
hgch_bar_CatCat(s2) # se repiten labels eje y
gg_bar_CatCat(s2, opts = list(label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj"))
hgch_bar_CatCat(s2, label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj")
gg_bar_CatCat(s2, opts = list(label_ratio = 1))
hgch_bar_CatCat(s2, label_ratio = 1)

# REVISAR
gg_bar_CatCat(s2, opts = list(percentage = TRUE, orientation = "hor"))
hgch_bar_CatCat(s2, percentage = TRUE, orientatio = "hor")
gg_bar_CatCat(s2, opts = list(label_ratio = 0.5, percentage = TRUE, legend_position = "top"))
hgch_bar_CatCat(s2, label_ratio = 0.5, percentage = TRUE, legend_position = "right")
gg_bar_CatCat(s2, opts = list(label_ratio = 1, percentage = TRUE, suffix ="pres"))
hgch_bar_CatCat(s2, label_ratio = 1, percentage = TRUE, suffix ="pres")


gg_bar_CatCat(s2, opts = list(graph_type = "stacked")) +
  scale_y_continuous(limits = c(0, 30))
hgch_bar_CatCat(s2, graphType = "stacked")
gg_bar_CatCat(s2, opts = list(graph_type = "stacked", label_ratio = 0.1, prefix = "dskk", suffix = " jdjdj")) +
  scale_y_continuous(limits = c(0, 20))
hgch_bar_CatCat(s2, graphType = "stacked", label_ratio = 0.1, prefix = "dskk", suffix = " jdjdj")
gg_bar_CatCat(s2, opts = list(graph_type = "stacked", label_ratio = 1)) +
  scale_y_continuous(limits = c(0, 20))
hgch_bar_CatCat(s2, graphType = "stacked", label_ratio = 1)

gg_bar_CatCat(s2, opts = list(graph_type = "stacked", percentage = TRUE, orientation = "hor")) +
  scale_y_continuous(limits = c(0, 100))
hgch_bar_CatCat(s2, graphType = "stacked", percentage = TRUE, orientation = "hor")
gg_bar_CatCat(s2, opts = list(graph_type = "stacked", label_ratio = 0.1, percentage = TRUE, legend_position = "top")) +
  scale_y_continuous(limits = c(0, 100))
hgch_bar_CatCat(s2, graphType = "stacked", label_ratio = 0.1, percentage = TRUE, legend_position = "right")
gg_bar_CatCat(s2, opts = list(graph_type = "stacked", label_ratio = 1, percentage = TRUE, suffix ="pres")) +
  scale_y_continuous(limits = c(0, 100))
hgch_bar_CatCat(s2, graphType = "stacked", label_ratio = 1, percentage = TRUE, suffix ="pres")

gg_area_CatCat(s2) # se repiten labels eje y
hgch_area_CatCat(s2) # se repiten labels eje y
gg_area_CatCat(s2, opts = list(label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj"))
hgch_area_CatCat(s2, label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj") # faltan prefijos y sufijos en labels
gg_area_CatCat(s2, labopts = list(el_ratio = 1))
hgch_area_CatCat(s2, label_ratio = 1)

# hay una diferencia entre los dos paquetes con el porcentaje, mirar fotos
gg_area_CatCat(s2, opts = list(percentage = TRUE, orientation = "hor"))
hgch_area_CatCat(s2, percentage = TRUE, orientation = "hor")
gg_area_CatCat(s2, opts = list(label_ratio = 0.5, percentage = TRUE, legend_position = "top"))
hgch_area_CatCat(s2, label_ratio = 0.5, percentage = TRUE, legend_position = "right")
gg_area_CatCat(s2, opts = list(label_ratio = 1, percentage = TRUE, suffix ="pres"))
hgch_area_CatCat(s2, label_ratio = 1, percentage = TRUE, suffix ="pres")

# la gráfica es diferente mirar fotos
gg_area_CatCat(s2, opts = list(graph_type = "stacked")) # recisar contra hgchmagic
hgch_area_CatCat(s2, graphType = "stacked")
gg_area_CatCat(s2, opts = list(label_ratio = 0.5, graph_type = "stacked", prefix = "dskk", suffix = " jdjdj")) # faltan sufijos y prefijos en el eje y
hgch_area_CatCat(s2, label_ratio = 0.5, graphType = "stacked", prefix = "dskk", suffix = " jdjdj")
gg_area_CatCat(s2, opts = list(label_ratio = 1, graph_type = "stacked"))
hgch_area_CatCat(s2, label_ratio = 1, graphType = "stacked")

# REVISAR
gg_area_CatCat(s2, opts = list(graph_type = "stacked", percentage = TRUE, orientation = "hor")) # ¿están bien?
hgch_area_CatCat(s2, graphType = "stacked", percentage = TRUE, orientation = "hor") # ¿están bien?
gg_area_CatCat(s2, opts = list(label_ratio = 0.5, graph_type = "stacked", percentage = TRUE, legend_position = "top"))
hgch_area_CatCat(s2, label_ratio = 0.5, graphType = "stacked", percentage = TRUE, legend_position = "right")
gg_area_CatCat(s2, opts = list(label_ratio = 1, graph_type = "stacked", percentage = TRUE, suffix ="pres"))
hgch_area_CatCat(s2, label_ratio = 1, graphType = "stacked", percentage = TRUE, suffix ="pres")

gg_line_CatCat(s2) # se repiten labels eje y
hgch_line_CatCat(s2) # se repiten labels eje y
gg_line_CatCat(s2, opts = list(label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj"))
hgch_line_CatCat(s2, label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj")
gg_line_CatCat(s2, opts = list(label_ratio = 1)) # se repiten labels en el eje y
hgch_line_CatCat(s2, label_ratio = 1) # se repiten labels en el eje y

gg_line_CatCat(s2, opts = list(percentage = TRUE, orientation = "hor"))
hgch_line_CatCat(s2, percentage = TRUE)
gg_line_CatCat(s2, opts = list(label_ratio = 0.5, percentage = TRUE, legend_position = "top"))
hgch_line_CatCat(s2, label_ratio = 0.5, percentage = TRUE, legend_position = "right")
gg_line_CatCat(s2, opts = list(label_ratio = 1, percentage = TRUE, suffix ="pres"))
hgch_line_CatCat(s2, label_ratio = 1, percentage = TRUE, suffix ="pres")

gg_treemap_CatCat(s2) # se repiten labels eje y
hgch_treemap_CatCat(s2) # se repiten labels eje y
gg_treemap_CatCat(s2, opts = list(prefix = "dskk", suffix = " jdjdj"))
hgch_treemap_CatCat(s2, label_ratio = 0.5, prefix = "dskk", suffix = " jdjdj")
gg_treemap_CatCat(s2, opts = list(label_ratio = 1)) # se repiten labels en el eje y
hgch_treemap_CatCat(s2, label_ratio = 1) # se repiten labels en el eje y

gg_treemap_CatCat(s2, opts = list(percentage = TRUE, orientation = "hor"))
hgch_treemap_CatCat(s2, percentage = TRUE)
gg_treemap_CatCat(s2, opts = list(label_ratio = 0.5, percentage = TRUE, legend_position = "top"))
hgch_treemap_CatCat(s2, label_ratio = 0.5, percentage = TRUE, legend_position = "right")
gg_treemap_CatCat(s2, opts = list(label_ratio = 1, percentage = TRUE, suffix ="pres"))
hgch_treemap_CatCat(s2, label_ratio = 1, percentage = TRUE, suffix ="pres")



gg_line_CatNum
function (data, title = NULL, subtitle = NULL, caption = NULL,
          horLabel = NULL, verLabel = NULL, horLine = NULL, verLine = NULL,
          agg = "sum", colors = NULL, colorText = "black", color_scale = "no",
          dropNa = FALSE, format = c("", ""), highlightValue = NULL,
          highlightValueColor = NULL, label_ratio = 1, labelWrap = 12,
          marks = c(".", ","), nDigits = 0, order = NULL, orientation = "ver",
          percentage = FALSE, shapeType = 19, sort = "no", sliceN = NULL,
          showText = TRUE, spline = FALSE, startAtZero = TRUE, theme = NULL,
          ...)
{
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  title <- title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2], paste(agg, nms[2])),
                            hor = horLabel, ver = verLabel)
  lineXY <- orientationXY(orientation,
                          0,
                          0,
                          hor = horLine,
                          ver = verLine)
  if (dropNa)
    d <- d %>% tidyr::drop_na()
  d <- d %>% tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                                    b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg,
                             b)) %>% dplyr::mutate(percent = b * 100/sum(b, na.rm = TRUE))
  d <- sortSlice(d, "b", "a", orientation, sort, sliceN)
  d <- orderCategory(d, "a", orientation, order, labelWrap)
  d <- labelPosition(d, "b", label_ratio, percentage)
  fillCol <- fillColors(d, "a", colors, color_scale, highlightValue,
                        highlightValueColor, labelWrap)
  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }
  gg <- ggplot(d, aes(x = a, y = d[[ifelse(percentage, "percent", "b")]], colour = a, group = 1)) +
    geom_line() +
    geom_point(shape = as.integer(shapeType)) +
    geom_vline(xintercept = lineXY[2], color = ifelse((orientation == "hor" & !is.null(horLine)) | (orientation == "ver" & !is.null(verLine)), "black", "transparent"), linetype = "dashed") +
    geom_hline(yintercept = lineXY[1], color = ifelse((orientation == "hor" & !is.null(verLine)) | (orientation == "ver" & !is.null(horLine)), "black", "transparent"), linetype = "dashed") +
    geom_text(aes(y = labPos, label = paste0(format[1], format(d[[ifelse(percentage, "percent", "b")]], big.mark = marks[1], decimal.mark = marks[2],
                                                               digits = nDigits, nsmall = nDigits), format[2])),
              check_overlap = TRUE, color = ifelse(showText, colorText,
                                                   "transparent")) + labs(title = title, subtitle = subtitle,
                                                                          caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_color_manual(values = fillCol) + scale_y_continuous(labels = function(x) paste0(format[1],
                                                                                          format(x, big.mark = marks[1], decimal.mark = marks[2],
                                                                                                 digits = nDigits, nsmall = nDigits), format[2]),
                                                              limits = c(ifelse(startAtZero, 0, NA), NA)) + theme_ds() +
    theme(legend.position = "none", plot.caption = element_text(hjust = 1))
  if (orientation == "hor")
    gg <- gg + coord_flip()
  gg
}

labelPosition
function (data, col, label_ratio, percentage = FALSE, zeroToNa = FALSE)
{
  col <- ifelse(percentage, "percent", col)
  half <- data[[col]] - data[[col]]/2
  small <- half < max(data[[col]] * label_ratio)
  half[small] <- data[[col]][small] + max(data[[col]])/50
  data$labPos <- half
  if (zeroToNa) {
    data$labPos[data[[col]] == 0] <- NA
  }
  data
}



gg_bar_CatNum

f0 <- function (data, title = NULL, subtitle = NULL, caption = NULL,
                horLabel = NULL, verLabel = NULL, horLine = NULL, verLine = NULL,
                agg = "sum", agg_text = NULL, colors = NULL, colorText = "#5A6B72",
                color_scale = "no", dropNa = FALSE, prefix = NULL, suffix = NULL,
                highlightValue = NULL, highlightValueColor = NULL, label_ratio = 0.1,
                labelWrap = 12, marks = c(".", ","), nDigits = NULL, order = NULL,
                orientation = "ver", percentage = FALSE, sort = "no", sliceN = NULL,
                showText = TRUE, sizeText = 3, theme = NULL, ...)
{
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  title <- title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  Lc <- length(unique(d$a))
  angleText <- ifelse(Lc >= 7 & Lc < 15, 45, ifelse(Lc >= 15,
                                                    90, 0))
  prefix_agg <- ifelse(is.null(agg_text), agg, agg_text)
  labelsXY <- orientationXY(orientation, x = nms[1], y = ifelse(nrow(d) ==
                                                                  dplyr::n_distinct(d$a), nms[2], paste(prefix_agg, nms[2])),
                            hor = horLabel, ver = verLabel)
  lineXY <- orientationXY(orientation, 0, 0, hor = horLine,
                          ver = verLine)
  if (dropNa)
    d <- d %>% tidyr::drop_na()
  if (is.null(nDigits)) {
    nDig <- 0
  }
  else {
    nDig <- nDigits
  }
  d <- d %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA), b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = round(agg(agg, b), nDig)) %>%
    dplyr::mutate(percent = round(b * 100/sum(b, na.rm = TRUE), nDig))

  d <- sortSlice(d, "b", "a", orientation, sort, sliceN)
  d <- orderCategory(d, "a", orientation, order, labelWrap)
  d1 <- labelPosition(d, "b", label_ratio, percentage)
  fillCol <- fillColors(d, "a", colors, color_scale, highlightValue,
                        highlightValueColor, labelWrap)
  if (percentage & is.null(suffix)) {
    suffix <- "%"
  }
  varP <- ifelse(percentage, "percent", "b")
  minLim <- ifelse(min(d[[varP]], na.rm = T) < 0, min(d[[varP]],
                                                      na.rm = T), 0)
  maxLim <- max(d[[varP]], na.rm = T) + 0.3 * max(d[[varP]],
                                                  na.rm = T)

  sq <- nchar(round(maxLim - minLim, 0)) - 2
  sq <- floor(seq(minLim, maxLim, round(maxLim/Lc, 0)) * 10^(-df)) * 10^df
  sq <- c(0, minLim, sq)

  gg <- ggplot(d, aes(x = a, y = d[[ifelse(percentage, "percent", "b")]], fill = a)) +
    geom_bar(stat = "identity") +
    geom_vline(xintercept = lineXY[2], color = ifelse((orientation == "hor" & !is.null(horLine)) | (orientation == "ver" & !is.null(verLine)), "#5A6B72",
                                                      "transparent"), linetype = "dashed") +
    geom_hline(yintercept = lineXY[1], color = ifelse((orientation == "hor" & !is.null(verLine)) | (orientation == "ver" & !is.null(horLine)), "#5A6B72", "transparent"), linetype = "dashed") +
    geom_text(aes(y = labPos, label = paste0(prefix, format(d[[ifelse(percentage, "percent", "b")]], big.mark = marks[1], decimal.mark = marks[2], nsmall = nDig), suffix)), check_overlap = TRUE, size = sizeText, color = ifelse(showText, colorText, "transparent")) +
    labs(title = title, subtitle = subtitle, caption = caption,
         x = labelsXY[1], y = labelsXY[2]) + scale_fill_manual(values = fillCol) +
    scale_y_continuous(labels = function(x) paste0(prefix,
                                                   format(x, big.mark = marks[1], decimal.mark = marks[2],
                                                          nsmall = nDig), suffix),
                       breaks = seq(minLim,maxLim, round(maxLim/Lc, nDig)),
                       limits = c(minLim, maxLim))
  if (orientation == "hor")
    gg <- gg + coord_flip()
  if (is.null(theme)) {
    gg <- gg + tma()
  }
  else {
    gg <- gg + theme
  }
  gg + theme(legend.position = "none", plot.caption = element_text(hjust = 1),
             axis.text.x = element_text(angle = angleText))
}



lp0 <- function(data, col, label_ratio, percentage = FALSE, zeroToNa = FALSE) {
  col <- ifelse(percentage, "percent", col)
  half <- data[[col]] - data[[col]] / 2
  small <- half < max(data[[col]] * label_ratio)
  half[small] <- data[[col]][small] + max(data[[col]]) / 50
  data$labPos <- half
  # do I want zero labels to be shown?
  if (zeroToNa) {
    data$labPos[data[[col]] == 0] <- NA
  }
  data
}




sq <- nchar(round(maxLim - minLim, 0)) - 2
sq <- floor(seq(minLim, maxLim, round(maxLim/Lc, 0)) * 10^(-df)) * 10^df
sq <- unique(c(0, minLim, sq))

d2 <- lp0(d0, "b", 1, FALSE)

ggplot(d, aes(x = a, y = d[[ifelse(FALSE, "percent", "b")]], fill = a)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = function(x) paste0("", format(x, big.mark = ".", decimal.mark = ",", nsmall = 0), ""),
                     breaks = sq, limits = c(minLim, maxLim))





f0 <- function(data,
               title = NULL,
               subtitle = NULL,
               caption = NULL,
               horLabel = NULL,
               verLabel = NULL,
               horLine = NULL,
               #horLineLabel = NULL,
               verLine = NULL,
               #verLineLabel = NULL,
               agg = "sum",
               colors = NULL,
               colorText = "black",
               color_scale = "no",
               dropNa = FALSE,
               format = c("", ""),
               highlightValue = NULL,
               highlightValueColor = NULL,
               label_ratio = 1,
               labelWrap = 12,
               marks = c(".", ","),
               nDigits = 0,
               order = NULL,
               orientation = "ver",
               percentage = FALSE,
               shapeType = 19,
               sort = "no",
               sliceN = NULL,
               showText = TRUE,
               spline = FALSE,
               startAtZero = TRUE,
               theme = NULL, ...) {
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d

  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2], paste(agg, nms[2])),
                            hor = horLabel,
                            ver = verLabel)
  lineXY <- orientationXY(orientation,
                          0,
                          0,
                          hor = horLine,
                          ver = verLine)

  if (dropNa)
    d <- d %>%
    tidyr::drop_na()

  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg, b)) %>%
    dplyr::mutate(percent = b * 100 / sum(b, na.rm = TRUE))

  d <- sortSlice(d, "b", "a", orientation, sort, sliceN)
  d <- orderCategory(d, "a", orientation, order, labelWrap)
  d <- labelPosition(d, "b", label_ratio, percentage)
  fillCol <- fillColors(d, "a", colors, color_scale, highlightValue, highlightValueColor, labelWrap)
  if (percentage & nchar(format[2]) == 0) {
    format[2] <- "%"
  }

  # if (spline) {
  #   d <- as.data.frame(spline(d))
  # }

  gg <- ggplot(d, aes(x = a, y = d[[ifelse(percentage, "percent", "b")]], colour = a, group = 1)) +
    geom_line() +
    geom_point(shape = as.integer(shapeType)) +
    geom_vline(xintercept = lineXY[2],
               color = ifelse((orientation == "hor" & !is.null(horLine)) | (orientation == "ver" & !is.null(verLine)),
                              "black",
                              "transparent"),
               linetype = "dashed") +
    geom_hline(yintercept = lineXY[1],
               color = ifelse((orientation == "hor" & !is.null(verLine)) | (orientation == "ver" & !is.null(horLine)),
                              "black",
                              "transparent"),
               linetype = "dashed") +
    geom_text(aes(y = labPos,
                  label = paste0(format[1],
                                 format(d[[ifelse(percentage, "percent", "b")]],
                                        big.mark = marks[1],
                                        decimal.mark = marks[2],
                                        digits = nDigits,
                                        nsmall = nDigits),
                                 format[2])),
              check_overlap = TRUE,
              color = ifelse(showText, colorText, "transparent")) +
    labs(title = title, subtitle = subtitle, caption = caption, x = labelsXY[1], y = labelsXY[2]) +
    scale_color_manual(values = fillCol) +
    scale_y_continuous(labels =  function(x) paste0(format[1],
                                                    format(x,
                                                           big.mark = marks[1],
                                                           decimal.mark = marks[2],
                                                           digits = nDigits,
                                                           nsmall = nDigits),
                                                    format[2]),
                       limits = c(ifelse(startAtZero, 0, NA), NA)) +
    tm() +
    theme(legend.position = "none",
          plot.caption = element_text(hjust = 1))
  ### FECHAAA
  # if (f$getCtypes()[1] == "Dat")
  #   gg <- gg +
  #   scale_x_date(labels = date_format("%Y-%m-%d"))
  if (orientation == "hor")
    gg <- gg +
    coord_flip()
  gg
}


tm <- function(...){
  type <- 'outer'
  inner <- type == 'inner'
  palette <- list(
    background = "#ffffff", # #ffffff #F0EDFF #F8EDFA #FDF8FD
    text = list(inner = "#555555", outer = "#111111"),
    line = list(inner = "#826A50", outer = "#362C21"),
    gridline = "#c9c7d3",
    swatch = c("#111111","#65ADC2","#233B43","#E84646","#C29365","#362C21","#316675","#168E7F","#109B37"),
    gradient = list(low = "#65ADC2", high = "#362C21")
  )
  spacing <- 0.5
  line_colour <- "#1d1d1d"
  text_colour <- "#555555"
  text_size <- 12
  line_weight <- 0.5
  x_title_spacing <- function(spacing)
    max(-1.2, -(spacing / 1.25) + 0.5)
  y_title_spacing <- function(spacing)
    max(0.8, min(2.4, spacing))

  theme(
    #legend.title=element_blank(),
    line = element_line(
      colour = line_colour,
      size = line_weight,
      linetype = 1,
      lineend = "butt"),
    rect = element_rect(
      fill = "white",
      colour = text_colour,
      size = 0.5,
      linetype = 1),
    text = element_text(
      debug=FALSE,
      margin=margin(),
      family = '',
      face = "plain",
      colour = text_colour,
      size = text_size,
      hjust = 0.5,
      vjust = 0.5,
      angle = 0,
      lineheight = 0.9),
    axis.text = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(0.8),
      colour = text_colour),
    strip.text = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(0.8)),
    axis.line = element_line(
      colour = line_colour),
    axis.line.x = element_line(colour = line_colour),
    axis.line.y = element_line(colour = line_colour),
    axis.text.x = element_text(
      debug=FALSE,
      margin=margin(0.1 * spacing, 0.1 * spacing, 0.1 * spacing, 0.1 * spacing, unit = 'cm'),
      vjust = 1,
      colour = text_colour,
      face='bold'),
    axis.text.y = element_text(
      debug=FALSE,
      margin=margin(0.1 * spacing, 0.1 * spacing, 0.1 * spacing, 0.1 * spacing, unit = 'cm'),
      hjust = 1,
      colour = text_colour,
      face='bold'),
    axis.ticks = element_line(colour = line_colour),
    axis.title = element_text(face='bold',colour = text_colour),
    axis.title.x = element_text(
      debug=FALSE,
      margin=margin(),
      vjust=x_title_spacing(spacing)),
    axis.title.y = element_text(
      debug=FALSE,
      margin=margin(),
      angle = 90,
      vjust=y_title_spacing(spacing)),
    axis.ticks.length = grid::unit(0.15, "cm"),
    axis.ticks.length.x.bottom = grid::unit(0.15, "cm"),
    axis.ticks.length.x.top = grid::unit(0.15, "cm"),
    axis.ticks.length.y.left = grid::unit(0.15, "cm"),
    axis.ticks.length.y.right = grid::unit(0.15, "cm"),
    legend.background = element_rect(
      colour = ifelse(inner, 'white', palette$background),
      fill = ifelse(inner, 'white', palette$background)),
    legend.margin = grid::unit(0.2 * spacing, "cm"),
    legend.key = element_rect(
      colour = ifelse(inner, 'white', palette$background),
      fill = palette$background),
    legend.key.size = grid::unit(
      1.2, "lines"),
    legend.key.height = NULL,
    legend.key.width = NULL,
    legend.text = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(0.8)),
    legend.position = "right",
    legend.direction = NULL,
    legend.justification = "center",
    legend.box = NULL,
    panel.background = element_rect(fill = palette$background,colour = NA),
    panel.border = element_blank(),
    panel.grid.major = element_line(linetype='dashed',colour = palette$gridline),
    panel.grid.minor = element_line(linetype='dashed',colour = palette$gridline),
    panel.margin = grid::unit(0.5 * spacing, 'cm'),
    panel.margin.x = NULL,
    panel.margin.y = NULL,
    panel.ontop = FALSE,
    strip.background = element_rect(
      fill = ifelse(inner, 'white', palette$background),
      colour = NA),
    strip.text.x = element_text(
      debug=FALSE,
      margin=margin(),
      size = rel(1.1),
      face = 'bold'),
    strip.text.y = element_text(
      debug=FALSE,
      margin=margin(),
      angle = -90,
      face = 'bold',
      size = rel(1.1)),
    strip.switch.pad.grid = grid::unit(0, 'cm'),
    strip.switch.pad.wrap = grid::unit(0, 'cm'),
    plot.background = element_rect(
      colour = ifelse(inner, 'white', palette$background),
      fill = ifelse(inner, 'white', palette$background)),

    plot.title = element_text(
      debug=FALSE,
      margin=margin(0, 0, 6.6, 0),
      size = rel(1.2),
      vjust = spacing,
      face='bold'),
    plot.margin = grid::unit(c(0.625, 0.625, 0.625, 0.625) * spacing, 'cm'),
    complete = TRUE
  )
}
#
#
# tm0 <- function(...){
#   type <- 'outer'
#   inner <- type == 'inner'
#   palette <- list(
#     background = "#ffffff", # #ffffff #F0EDFF #F8EDFA #FDF8FD
#     text = list(inner = "#555555", outer = "#111111"),
#     line = list(inner = "#826A50", outer = "#362C21"),
#     gridline = "#c9c7d3",
#     swatch = c("#111111","#65ADC2","#233B43","#E84646","#C29365","#362C21","#316675","#168E7F","#109B37"),
#     gradient = list(low = "#65ADC2", high = "#362C21")
#   )
#   spacing <- 0.5
#   line_colour <- "#1d1d1d"
#   text_colour <- "#555555"
#   text_size <- 12
#   line_weight <- 0.5
#   x_title_spacing <- function(spacing)
#     max(-1.2, -(spacing / 1.25) + 0.5)
#   y_title_spacing <- function(spacing)
#     max(0.8, min(2.4, spacing))
#
#   theme(
#     #legend.title=element_blank(),
#     line = element_line(
#       colour = line_colour,
#       size = line_weight,
#       linetype = 1,
#       lineend = "butt"),
#     rect = element_rect(
#       fill = "white",
#       colour = text_colour,
#       size = 0.5,
#       linetype = 1),
#     text = element_text(
#       debug=FALSE,
#       margin=margin(),
#       family = '',
#       face = "plain",
#       colour = text_colour,
#       size = text_size,
#       hjust = 0.5,
#       vjust = 0.5,
#       angle = 0,
#       lineheight = 0.9),
#     axis.text = element_text(
#       debug=FALSE,
#       margin=margin(),
#       size = rel(0.8),
#       colour = text_colour),
#     strip.text = element_text(
#       debug=FALSE,
#       margin=margin(),
#       size = rel(0.8)),
#     axis.line = element_line(
#       colour = line_colour),
#     axis.line.x = element_line(colour = line_colour),
#     axis.line.y = element_line(colour = line_colour),
#     axis.text.x = element_text(
#       debug=FALSE,
#       margin=margin(0.1 * spacing, 0.1 * spacing, 0.1 * spacing, 0.1 * spacing, unit = 'cm'),
#       vjust = 1,
#       colour = text_colour,
#       face='bold'),
#     axis.text.y = element_text(
#       debug=FALSE,
#       margin=margin(0.1 * spacing, 0.1 * spacing, 0.1 * spacing, 0.1 * spacing, unit = 'cm'),
#       hjust = 1,
#       colour = text_colour,
#       face='bold'),
#     axis.ticks = element_line(colour = line_colour),
#     axis.title = element_text(face='bold',colour = text_colour),
#     axis.title.x = element_text(
#       debug=FALSE,
#       margin=margin(),
#       vjust=x_title_spacing(spacing)),
#     axis.title.y = element_text(
#       debug=FALSE,
#       margin=margin(),
#       angle = 90,
#       vjust=y_title_spacing(spacing)),
#     axis.ticks.length = grid::unit(0.15, "cm"),
#     axis.ticks.length.x.bottom = grid::unit(0.15, "cm"),
#     axis.ticks.length.x.top = grid::unit(0.15, "cm"),
#     axis.ticks.length.y.left = grid::unit(0.15, "cm"),
#     axis.ticks.length.y.right = grid::unit(0.15, "cm"),
#     legend.background = element_rect(
#       colour = ifelse(inner, 'white', palette$background),
#       fill = ifelse(inner, 'white', palette$background)),
#     legend.margin = grid::unit(0.2 * spacing, "cm"),
#     legend.key = element_rect(
#       colour = ifelse(inner, 'white', palette$background),
#       fill = palette$background),
#     legend.key.size = grid::unit(
#       1.2, "lines"),
#     legend.key.height = NULL,
#     legend.key.width = NULL,
#     legend.text = element_text(
#       debug=FALSE,
#       margin=margin(),
#       size = rel(0.8)),
#     legend.position = "right",
#     legend.direction = NULL,
#     legend.justification = "center",
#     legend.box = NULL,
#     panel.background = element_rect(fill = palette$background,colour = NA),
#     panel.border = element_blank(),
#     panel.grid.major = element_line(linetype='dashed',colour = palette$gridline),
#     panel.grid.minor = element_blank(),
#     panel.margin = grid::unit(0.5 * spacing, 'cm'),
#     panel.margin.x = NULL,
#     panel.margin.y = NULL,
#     panel.ontop = FALSE,
#     strip.background = element_rect(
#       fill = ifelse(inner, 'white', palette$background),
#       colour = NA),
#     strip.text.x = element_text(
#       debug=FALSE,
#       margin=margin(),
#       size = rel(1.1),
#       face = 'bold'),
#     strip.text.y = element_text(
#       debug=FALSE,
#       margin=margin(),
#       angle = -90,
#       face = 'bold',
#       size = rel(1.1)),
#     strip.switch.pad.grid = grid::unit(0, 'cm'),
#     strip.switch.pad.wrap = grid::unit(0, 'cm'),
#     plot.background = element_rect(
#       colour = ifelse(inner, 'white', palette$background),
#       fill = ifelse(inner, 'white', palette$background)),
#
#     plot.title = element_text(
#       debug=FALSE,
#       margin=margin(0, 0, 6.6, 0),
#       size = rel(1.2),
#       vjust = spacing,
#       face='bold'),
#     plot.margin = grid::unit(c(0.625, 0.625, 0.625, 0.625) * spacing, 'cm'),
#     complete = TRUE
#   )
# }

### Area plots

df <- sampleData('Yea-Num')
gg_area_CatNum(df)
gg_area_CatNum(df, showText = F)
gg_area_CatNum(df, colors = "darkred")
gg_area_CatNum(df,
               title = "Titulo",
               subtitle = "subtitulo" ,
               caption = "Caption")

gg_area_CatNum(df, percentage = TRUE)

gg_area_CatNumP(sampleData("Cat-NumP"))

dfCdn <- sampleData("Cat-Cat-Num")
gg_area_CatCatNum(dfCdn)
gg_area_CatCatNum(dfCdn, graph_type = "stacked")
gg_area_CatCatNum(dfCdn, percentage = TRUE, graph_type = "stacked")
gg_area_CatCatNum(dfCdn, showText = F)

dfCc <- sampleData('Cat-Cat')
gg_area_CatCat(dfCc)
gg_area_CatCat(dfCc, graph_type = "stacked")
gg_area_CatCat(dfCc, percentage = TRUE, graph_type = "stacked")
gg_area_CatCat(dfCc, showText = F)


## Bar plots

df <- sampleData('Cat-Num')
gg_bar_CatNum(df)
gg_bar_CatNum(df, color_scale = "discrete")
gg_bar_CatNum(df, orientation = "hor")
gg_bar_CatNum(df, highlightValue = "FormC", highlightValueColor = "red")
gg_bar_CatNum(df, agg = "mean")
gg_bar_CatNum(df, horLabel = "nombre del eje horizontal",
              verLabel = "nombre del eje vertical")
gg_bar_CatNum(df, format = c("$", ""))
gg_bar_CatNum(df, marks = c(",", "."))
gg_bar_CatNum(df, horLine = 4000)



dfCyn <- sampleData('Cat-Yea-Num')
gg_bar_CatCatNum(dfCyn, color_scale = "continuous")
gg_bar_CatCatNum(dfCyn, graph_type = "stacked")
gg_bar_CatCatNum(dfCyn, graph_type = "stacked", percentage = T)
gg_bar_CatCatNum(dfCyn, format = c("$", ""))


dCN <- sampleData("Cat-Num")
gg_treemap_CatNum(dCN)
gg_treemap_CatNum(dCN,
                  title = "titulo",
                  subtitle =  "subtitulo",
                  caption = "caption",
                  colors = c("#AFCC1D", "#FFDC1A", "#FCDAA4"))
gg_treemap_CatNum(dCN,
                  color_scale = "discrete",
                  highlightValue = "IlkD", highlightValueColor = "red")

gg_treemap_CatNum(dCN,
                  showLegend = F,
                  color_scale = "continuous",
                  agg = "mean",
                  marks = c(".", ","))

gg_treemap_CatNum(dCN,
                  percentage = T,
                  nDigits = 5,
                  sliceN = 3,
                  dropNa = T,
                  legend_position = c("bottom", "bottom"),
                  showText = F)

dC <- sampleData("Cat")
gg_treemap_Cat(dC)

data <- sampleData('Cat-Cat-Num')
gg_treemap_CatCatNum(data, colorGroup = '#cccccc')
gg_treemap_CatCatNum(data, color_scale = 'continuous')
gg_treemap_CatCatNum(data, color_scale = 'continuous', showLegend = F)
gg_treemap_CatCatNum(data,
                     color_scale = 'continuous',
                     showLegend = F,
                     showText = F,
                     colorGroup = '#cccccc',
                     colorText = c('#ffffff'))

data <- sampleData('Cat-Cat')
gg_treemap_CatCat(data)

data <- sampleData('Cat-NumP')
gg_treemap_CatNumP(data)

##### ORDEN DE LAS FUNCIONES... PRIMERO LA DE SORT DESPUÉS LA DE ORDER PARA
# QUE TENGA PRIORIDAD EL SORT
# FALTA HACER EL TEMA, también tiene que admitir temas viejos o ¿¿qué??...

### arreglado el sort de horizontal... colores ejemplo...

# gg_bar_Cat(data.frame(a = letters[1:8]), colors = colorNumeric(c("#009EE3",
# "#E5007D", "#95C11E"), 1:8)(1:8)[sample(8)], diffColorsBar = TRUE)

### script para probar los argumentos de las funciones
# función
# f0 <- gg_bar_Cat
# data <- s0
# order <- "g"
# hg <- c("g", "NA")
# f0(data)
# # # títulos
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO")
# # labels ejes, líneas
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2)
# # orientatio
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor")
# # dropna, format (que le gana a percentage), order, labelwrap, labelRatio
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
#    dropNa = TRUE, format = c("pre", " &$pos"), order = order, labelWrap = 3,
#    labelRatio = 1)
# # percentage, marks, ndigits
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
#    dropNa = TRUE, format = c("pre", " &$pos"), order = order, percentage = TRUE,
#    marks = c(",", "."), nDigits = 4)
# # sort, sliceN, showText, colorText
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
#    dropNa = TRUE, format = c("pre", " &$pos"), order = order, percentage = TRUE,
#    marks = c(",", "."), nDigits = 4, sort = "asc", sliceN = 1,
#    colorText = "darkgreen")
# # colors
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
#    dropNa = TRUE, format = c("pre", " &$pos"), order = order, percentage = TRUE,
#    marks = c(",", "."), nDigits = 4, sort = "asc",
#    colorText = "darkgreen", colors = "red")
# # colors, diffColorsBar
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
#    dropNa = TRUE, format = c("pre", " &$pos"), order = order, percentage = TRUE,
#    marks = c(",", "."), nDigits = 4, sort = "asc",
#    colors = c("blue", "yellow", "orange"), diffColorsBar = TRUE)
# # colors, diffColorsBar
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
# horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
# dropNa = TRUE, format = c("pre", " &$pos"), order = order, percentage = TRUE,
# marks = c(",", "."), nDigits = 4, sort = "asc",
# colors = c("blue", "yellow"), diffColorsBar = TRUE)
# # colors, highlightvalue
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
#    format = c("pre", " &$pos"), order = order, percentage = TRUE,
#    marks = c(",", "."), nDigits = 4, sort = "asc",
#    colors = "purple", highlightValue = hg)
# # colors, highlightvalue
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
#    format = c("pre", " &$pos"), order = order, percentage = TRUE,
#    marks = c(",", "."), nDigits = 4, sort = "asc",
#    colors = c("purple", "red"), highlightValue = hg)
# # colors, highlightvalue, diffcolorsbar
# f0(data, title = "TITL", subtitle = "Subtitl", caption = "CHIM SJAO",
#    horLabel = "HORZ", verLabel = "VERZ", horLine = 4, verLine = 2, orientation = "hor",
#    format = c("pre", " &$pos"), order = order, percentage = TRUE,
#    marks = c(",", "."), nDigits = 4, sort = "asc",
#    colors = c("purple", "red", "yellow"), highlightValue = "NA")


# gg_bar_Cat(s0, title = "title", subtitle = "skrkrj",
#            caption = "sjjsfasdf ", colorScale = "no",
#            highlightValueColor = "blue", dropNa = TRUE, sort = "aasc",
#            order = "X_C", orientation = "ver", percentage = TRUE, nDigits = 5,
#            colors = c("yellow", "orange"))

## PRUEBA CAT CAT NUM
# FALTA LEGEND TITLE

### PREGUNTAS ----- stacked con percentage TRUE no es stacked 100?¿?¿?¿
# poner sólo bar CATCATNUM y como parámetro stacked, grouped, 100

# s0 <- rbind(sampleData("Cat-Cat-Num"),
#             c("ajs sjjsj sjjsjsj sj", "sjaja ja ajds j sjs", 1432))
# gg_bar_grouped_CatCatNum(s0, title = "title", subtitle = "skrkrj",
#                          caption = "sjjsfasdf", horLabel = " HOR",
#                          verLabel = " VER", agg = "sum", dropNa = c(TRUE, TRUE),
#                          labelRatio = 1, legendPosition = "right", legendTitle = "d",
#                          marks = c(",", "."), percentage = TRUE, nDigits = 4,
#                          order1 = "CatE", order2 = "TypeD", colorScale = "continuous",
#                          colors = c("red", "yellow", "brown", "blue"), labelWrap = c(20, 40))



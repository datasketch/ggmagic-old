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


#### AYUDA LÍNEAS HIGHCHARTS


#### PRUEBAS CMLDIJ
# Barras ------------------------------------------------------------------

# Categoricas
#
# datCat <- sampleData('Cat', nrow = 300)
# hgch_bar_Cat(datCat)
# hgch_bar_Cat(datCat, title = 'Esto es un título',
#              subtitle = 'Esto es un subtitulo',
#              caption = 'Esto es un caption'
# )
# hgch_bar_Cat(datCat, orientation = 'hor', verLabel = 'Texto vertical',
#              horLabel = 'Texto horizontal')
# hgch_bar_Cat(datCat, orientation = 'ver', verLabel = 'Texto vertical',
#              horLabel = 'Texto horizontal')
# hgch_bar_Cat(datCat, horLine = 40, percentage = TRUE)
# hgch_bar_Cat(datCat, orientation = 'hor', horLine = 3)
# hgch_bar_Cat(datCat, colorScale = 'discrete', theme = tma(showText = FALSE))
#
# # Categorica-Numerica
#
# datCatNum <- sampleData('Cat-Num')
# hgch_bar_CatNum(datCatNum)
# hgch_bar_CatNum(datCatNum, sort = 'asc')
# hgch_bar_CatNum(datCatNum, sort = 'desc', colorScale = 'discrete')
# hgch_bar_CatNum(datCatNum, export = TRUE, nDigits = 2, theme = tma(showText = F),
#                 marks = c(',', '.'), format = c('$', ''))
#
# # Data-Numerica
# datDatNum <- sampleData('Dat-Num')
# hgch_bar_DatNum(datDatNum)
# hgch_bar_DatNum(datDatNum, dropNa = TRUE)
#
#
# #Años - Númerico
# datYeaNum <- sampleData('Yea-Num', 250)
# hgch_bar_YeaNum(datYeaNum, agg = 'mean')
#
#
# # Categoricos - categoricos
# dfCC <- sampleData('Cat-Cat', 999)
# hgch_bar_grouped_CatCat(dfCC)
# hgch_bar_grouped_CatCat(dfCC, percentage = TRUE)
# hgch_bar_stacked_CatCat(dfCC)
# hgch_bar_stacked_CatCat(dfCC, percentage = T)
#
# # Categoricos - categoricos - Numericos
# dfCCN <- sampleData('Cat-Cat-Num', 500)
# hgch_bar_grouped_CatCatNum(dfCCN)
# hgch_bar_grouped_CatCatNum(dfCCN, colorScale = "no")
# hgch_bar_stacked_CatCatNum(dfCCN, percentage = T)
# hgch_bar_stacked_CatCatNum(dfCCN, percentage = T,
#                            orientation = "ver", horLine = 60,
#                            horLineLabel  = 'anaperra',
#                            verLine = 3)
#
#
# # Categoricos - Fecha - numericos
# dfCDN <- sampleData('Cat-Dat-Num')
# hgch_bar_grouped_CatDatNum(dfCDN)
# hgch_bar_stacked_CatDatNum(dfCDN)
#
# # Categoricos - Años - Numericos
# dfCAN <- sampleData('Cat-Yea-Num')
# hgch_bar_grouped_CatYeaNum(dfCAN)
# hgch_bar_stacked_CatYeaNum(dfCAN, percentage = TRUE)
#
#
# # Categoricos y P columnas numericas
# dfCNp <- sampleData('Cat-NumP')
# hgch_bar_grouped_CatNumP(dfCNp)
# # Líneas ------------------------------------------------------------------
#
#
# # Categoricas
#
# datCat <- sampleData('Cat', nrow = 300)
# hgch_line_Cat(datCat)
# hgch_line_Cat(datCat, spline = T)
# hgch_line_Cat(datCat, startAtZero = F,  sort = 'desc', order = c('IlkD'))
# hgch_line_Cat(datCat, title = 'Esto es un título',
#               subtitle = 'Esto es un subtitulo',
#               caption = 'Esto es un caption'
# )
#
# # Categorica-Numerica
#
# datCatNum <- sampleData('Cat-Num')
# hgch_line_CatNum(datCatNum)
# hgch_line_CatNum(datCatNum, sort = 'asc', dropNa = TRUE)
# hgch_line_CatNum(datCatNum, sort = 'desc', colors = c('orange'))
# hgch_line_CatNum(datCatNum, export = TRUE, nDigits = 2, theme = tma(showText = F),
#                  marks = c(',', '.'), format = c('', ' <b>cosas</b>'), order = c('FormE'))
#
# # Data-Numerica
# datDatNum <- sampleData('Dat-Num', 100)
# hgch_line_DatNum(datDatNum)
# hgch_line_DatNum(datDatNum, dropNa = TRUE, colors = '#8B0000')
#
#
# #Años - Númerico
# datYeaNum <- sampleData('Yea-Num', 250)
# hgch_line_YeaNum(datYeaNum, agg = 'mean', startAtZero = F)
#
#
# # Categoricos - categoricos
# dfCC <- sampleData('Cat-Cat', 999)
# hgch_line_CatCat(dfCC)
# hgch_line_CatCat(dfCC, percentage = TRUE)
#
# # Categoricos - categoricos - Numericos
# dfCCN <- sampleData('Cat-Cat-Num', 500)
# hgch_line_CatCatNum(dfCCN, spline = T, plotBandsColorY = '#DDD', plotBandsToY = 10000, plotBandsFromY = 5000,
#                     plotBandsColorX = '#CCC', plotBandsFromX = 3, plotBandsToX = 5)
# hgch_line_CatCatNum(dfCCN, colors = c('#FF0AC2', '#FFCDDD', '#FDFD0D', '#FDACDC'))
#
#
# # Categoricos - Fecha - numericos
# dfCDN <- sampleData('Cat-Dat-Num')
# hgch_line_CatDatNum(dfCDN)
#
# # Categoricos - Años - Numericos
# dfCAN <- sampleData('Cat-Yea-Num')
# hgch_line_CatYeaNum(dfCAN, startAtZero = F)
#
# # Categoricos y P columnas numericas
# dfCNp <- sampleData('Cat-NumP')
# hgch_line_CatNumP(dfCNp)

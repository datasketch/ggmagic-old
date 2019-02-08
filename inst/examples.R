library(devtools)
load_all()
document()
install()

library(ggmagic)

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

dfCdn <- sampleData("Cat-Cat-Num")
gg_area_CatCatNum(dfCdn)
gg_area_CatCatNum(dfCdn, graphType = "stacked")
gg_area_CatCatNum(dfCdn, percentage = TRUE, graphType = "stacked")
gg_area_CatCatNum(dfCdn, showText = F)

dfCc <- sampleData('Cat-Cat')
gg_area_CatCat(dfCc)
gg_area_CatCat(dfCc, graphType = "stacked")
gg_area_CatCat(dfCc, percentage = TRUE, graphType = "stacked")
gg_area_CatCat(dfCc, showText = F)


## Bar plots

df <- sampleData('Cat-Num')
gg_bar_CatNum(df)
gg_bar_CatNum(df, colorScale = "discrete")
gg_bar_CatNum(df, orientation = "hor")
gg_bar_CatNum(df, highlightValue = "FormC", highlightValueColor = "red")
gg_bar_CatNum(df, agg = "mean")
gg_bar_CatNum(df, horLabel = "nombre del eje horizontal",
              verLabel = "nombre del eje vertical")
gg_bar_CatNum(df, format = c("$", ""))
gg_bar_CatNum(df, marks = c(",", "."))
gg_bar_CatNum(df, horLine = 4000)



dfCyn <- sampleData('Cat-Yea-Num')
gg_bar_CatCatNum(dfCyn, colorScale = "continuous")
gg_bar_CatCatNum(dfCyn, graphType = "stacked")
gg_bar_CatCatNum(dfCyn, graphType = "stacked", percentage = T)
gg_bar_CatCatNum(dfCyn, format = c("$", ""))


dCN <- sampleData("Cat-Num")
gg_treemap_CatNum(dCN)
gg_treemap_CatNum(dCN,
                  title = "titulo",
                  subtitle =  "subtitulo",
                  caption = "caption",
                  colors = c("#AFCC1D", "#FFDC1A", "#FCDAA4"))
gg_treemap_CatNum(dCN,
                  colorScale = "discrete",
                  highlightValue = "IlkD", highlightValueColor = "red")

gg_treemap_CatNum(dCN,
                  showLegend = F,
                  colorScale = "continuous",
                  agg = "mean",
                  marks = c(".", ","))

gg_treemap_CatNum(dCN,
                  percentage = T,
                  nDigits = 5,
                  sliceN = 3,
                  dropNa = T,
                  legendPosition = c("bottom", "bottom"),
                  showText = F)

dC <- sampleData("Cat")
gg_treemap_Cat(dC)

data <- sampleData('Cat-Cat-Num')
gg_treemap_CatCatNum(data)
gg_treemap_CatCatNum(data, colorScale = 'continuous')
gg_treemap_CatCatNum(data, colorScale = 'continuous', showLegend = F)
gg_treemap_CatCatNum(data,
                     colorScale = 'continuous',
                     showLegend = F,
                     showText = F,
                     colorGroup = '#cccccc',
                     colorText = c('#ffffff'))

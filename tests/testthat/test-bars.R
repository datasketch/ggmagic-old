test_that("Bars", {

  library(homodatum)
  library(paletero)
  library(makeup)

  data <- sampleData("Dat-Num", n = 10, rep = FALSE)
  data
  # opts <- getDefaultOptions()

  makeup(getFringeDataFrame(fringe(data))$a, sample = "Enero 4")

  v <- as.Date(homodatum:::Dat_show(data[[1]]))
  makeup(v, sample = "Enero 4")
  makeup(v, sample = "Enero 4 2020")
  makeup_dat(v, sample = "Ene 4")

  gg_bar_DatNum(data)
  gg_bar_DatNum(data, title = "Nice title")
  gg_bar_DatNum(data, background_color = "#FF9876")
  gg_bar_DatNum(data, background_color = "#FF9876", palette_colors = c("#45FF34"))

  opts <- list(
    format_dat_sample = "Ene 10",
    locale = "es-CO",
    title = "Nice chart",
    subtitle = "I really like this",
    caption = "This is some info about the chart",
    theme = list(background_color = "#888810")
  )
  gg_bar_DatNum(data, opts = opts)




  gg_bar_CatNum(sampleData("Cat-Num", nrow = 10))


  # Validar leyendas que sí sean continuas o discretas
  # Check y-limits para dejar aire arriba

})

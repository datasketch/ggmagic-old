test_that("Bars", {

  library(homodatum)
  library(paletero)
  library(makeup)

  data <- sampleData("Dat-Num", n = 10, rep = FALSE)
  data <- sampleData("Dat-Num")
  data
  # opts <- ggmagic:::ggmagic_defaults()

  gg_bar_DatNum(data)
  gg_bar_DatNum(data, logo = "datasketch", caption ="some caption")
  gg_bar_DatNum(data, footer_include = TRUE, caption ="some caption")
  gg_bar_DatNum(data, footer_include = TRUE, logo = "datasketch")
  gg_bar_DatNum(data, title = "Nice **title**")

  gg_bar_DatNum(data, title = "Nice **title**",
                caption = "Hola _caption_",
                logo = "https://www.r-project.org/logo/Rlogo.png",
                logo_width = 20)

  gg_bar_DatNum(data, title = "Nice **title**",
                caption = "Hola _caption_",
                logo = "https://www.r-project.org/logo/Rlogo.png",
                footer_text = "Visualización por: ",
                logo_width = 20)


  gg_bar_DatNum(data, background_color = "#FF9876")
  gg_bar_DatNum(data, background_color = "#c39876",
                palette_colors = c("#45FF34"))

  opts <- list(
    format_dat_sample = "Ene 10",
    locale = "es-CO",
    title = "Nice chart",
    subtitle = "I really like this",
    caption = "This is some info about the chart",
    theme = list(background_color = "#888810")
  )
  gg_bar_DatNum(data, opts = opts)


  opts <- list(
    format_dat_sample = "Ene 10",
    locale = "es-CO",
    title = "Nice chart",
    subtitle = "I really like this",
    caption = "This is some info about the chart",
    footer_text = "powered by: ",
    logo = "datasketch",
    theme = list(background_color = "#888810")
  )
  gg <- gg_bar_DatNum(data, opts = opts)
  gg

  # Validar leyendas que sí sean continuas o discretas
  # Check y-limits para dejar aire arriba



})

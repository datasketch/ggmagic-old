test_that("gg bar DatNum", {


  data <- sampleData("Dat-Num", n = 10, rep = FALSE)
  opts <- dsvizopts::dsviz_defaults()

  l <- ggmagic_prep(data, opts)




  gg_bar_DatNum(data)
  gg_bar_DatNum(data, logo = "datasketch", caption ="some caption")
  gg_bar_DatNum(data,
                branding_include = TRUE,
                branding_text ="Hecho con amor")
  gg_bar_DatNum(data, title = "Nice **title**")

  gg_bar_DatNum(data, title = "Nice **title**",
                caption = "Hola _caption_",
                branding_include = TRUE,
                logo = "https://www.r-project.org/logo/Rlogo.png",
                logo_width = 20)

  gg_bar_DatNum(data, title = "Nice **title**",
                caption = "Hola _caption_",
                logo = "https://www.r-project.org/logo/Rlogo.png",
                branding_include = TRUE,
                branding_background_color = "#ccccfa",
                footer_text = "Visualización por: ")


  gg_bar_DatNum(data, background_color = "#FF9876")
  gg_bar_DatNum(data, background_color = "#c39876",
                palette_colors = c("#45FF34"))

  opts <- list(
    format_dat_sample = "Ene 10",
    locale = "es-CO",
    title = "Nice chart",
    subtitle = "I really like this",
    caption = "This is some info about the chart",
    theme = list(background_color = "#a8a840", text_color = "#ffffff")
  )
  gg_bar_DatNum(data, opts = opts)
  gg_bar_DatNum(data,   format_dat_sample = "Ene 10",
                locale = "es-CO")


  opts <- list(
    format_dat_sample = "Ene 10",
    locale = "es-CO",
    title = "Nice chart",
    subtitle = "I really like this",
    caption = "This is some info about the chart",
    footer_text = "powered by: ",
    logo = "datasketch",
    theme = list(background_color = "#a8a840", text_color = "#ffffff")
  )
  gg <- gg_bar_DatNum(data, opts = opts)
  gg

  # Validar leyendas que sí sean continuas o discretas
  # Check y-limits para dejar aire arriba



})

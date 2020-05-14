test_that("gg bar DatNum", {


  data <- sampleData("Dat-Num", n = 10, rep = FALSE)
  opts <- dsvizopts::dsviz_defaults()

  l <- ggmagic_prep(data, opts)




  gg_bar_DatNum(data)
  gg_bar_DatNum(data, grid_color = "#44aa56", grid_size = 0.8, line_size = 0.3,
                axis_line_color = "#aa0000")
  gg_bar_DatNum(data,
                grid_y_color = "#444444",
                grid_x_color = "#ff4444",
                grid_line_type = "dashed")

  gg_bar_DatNum(data,
                grid_y_color = "#444444",
                grid_x_color = "#ff4444",
                grid_size = 0.1,
                grid_y_line_type = "dashed",
                axis_line_color = "#00f100",
                axis_line_size = 3)


  gg_bar_DatNum(data, logo = "datasketch", caption ="some caption")
  gg_bar_DatNum(data,
                branding_include = TRUE,
                branding_text ="Hecho con amor")
  gg_bar_DatNum(data, title = "Nice **title**")

  gg_bar_DatNum(data, title = "Nice **title**",
                subtitle = "This is some small information",
                caption = "Hola _caption_",
                branding_include = TRUE,
                logo = "https://www.r-project.org/logo/Rlogo.png",
                logo_width = 20)

  gg_bar_DatNum(data, title = "Nice **title**",
                subtitle = "This is some small information",
                caption = "Hola _caption_",
                logo = "https://www.r-project.org/logo/Rlogo.png",
                branding_include = TRUE,
                branding_background_color = "#ccccfa",
                branding_text = "Visualización por mi")


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

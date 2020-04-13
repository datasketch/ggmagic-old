test_that("branding works", {

  library(gridExtra)
  library(homodatum)
  library(makeup)
  library(ggtext)
  library(gridtext)

  data <- sampleData("Dat-Num", n = 10, rep = FALSE)
  opts <- list(
    format_dat_sample = "Ene 10",
    locale = "es-CO",
    title = "Nice chart",
    subtitle = "I really like this",
    caption = "This is some info about the chart",
    # footer_include = FALSE,
    branding_include = TRUE,
    logo_position = "left",
    branding_background_color = "#444444",
    branding_text = "Developed with love in Bogotá."
    # logo = "datasketch" # Not necessary to include logo, included by default
    # theme = list(background_color = "#dddddd")
  )
  # opts <- modifyList(theme_defaults(), opts)
  # opts_theme <- merge_theme_options(opts)

  ggg <- gg_bar_DatNum(data, opts = opts)
  ggg

  # ggsave("~/Downloads/plot.png", ggg)
  # ggsave("~/Downloads/plot.svg", ggg)
  # ggsave("~/Downloads/plot.pdf", ggg, device = cairo_pdf)



})

# test_that("branding works", {

  library(gridExtra)

  data <- sampleData("Dat-Num", n = 10, rep = FALSE)
  opts <- list(
    format_dat_sample = "Ene 10",
    locale = "es-CO",
    title = "Nice chart",
    subtitle = "I really like this",
    caption = "This is some info about the chart",
    theme = list(background_color = "#888810")
  )
  gg <- gg_bar_DatNum(data, opts = opts)
  gg


  hjust <- 1
  footer_background_color <- "#666060"
  footer_text_color <- "#99c448"

  grid.newpage()
  img_src <- system.file("extdata", "Rlogo.png", package = "gridtext")
  text <- glue::glue("<img src='{img_src}' width='30'/>")
  logo <- richtext_grob(text,
                        x = hjust,
                        y = 0,
                        hjust = hjust,
                        vjust = 0,
                        # margin = unit(c(25,0,25,0),'pt'),
                        gp = gpar(fontsize = 10,
                                  fontfamily = "Ubuntu",
                                  col = footer_text_color
                        ))
  text <- richtext_grob("Hola Este es otro texto",
                        x = 0,
                        y = 0,
                        hjust = 0,
                        vjust = 0,
                        margin = unit(c(0,0,0,0),'pt'),
                        padding = unit(c(20,0,20,0),'pt'),
                        box_gp = gpar(col = "black", fill = "cornsilk"),
                        gp = gpar(fontsize = 10,
                                  fontfamily = "Ubuntu",
                                  col = footer_text_color
                        ))

  grid.newpage()
  grid.draw(text)
  grid.newpage()
  textTree <- grobTree(text, logo)
  grid.draw(textTree)
  rect <- rectGrob(
    y = 0,
    width = 1,
    height = unit(85, "pt"),
    gp=gpar(fill=footer_background_color ,
            col = footer_background_color,
            alpha=0.5)
    )
  footer <- grobTree(rect, textTree)
  grid.newpage()
  grid.draw(footer)

  g <- grid.arrange(gg, bottom = footer
                    #heights = unit(c(100,10), c("pt", "pt"))
                    )
  grid.draw(g)



# })

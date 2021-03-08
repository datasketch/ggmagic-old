test_that("Test theme options", {


  opts <- dsviz_default_opts()
  l_gg <- ggmagic:::add_ggmagic_theme(opts = opts$theme)
  opts$theme$text_size
  l_gg$axis.line$size
})

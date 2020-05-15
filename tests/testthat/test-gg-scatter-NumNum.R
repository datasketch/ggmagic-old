test_that("gg scatter NumNum", {


  data <- sample_data("Num-Num", n = 100, rep = FALSE)
  opts <- dsvizopts::dsviz_defaults()

  l <- ggmagic_prep(data, opts)

  # TODO scatter NumNum should have

  gg_scatter_NumNum(data)
  gg_bar_CatNum(data, logo = "datasketch", caption ="some caption")

  opts <- dsvizopts::dsviz_defaults()
  opts$postprocess$sort <- "asc"
  l <- ggmagic_prep(data, opts)

})

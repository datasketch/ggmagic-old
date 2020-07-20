test_that("gg bar DatNum", {


  data <- sample_data("Cat-Dat-Num", n = 100, rep = FALSE)

  gg_line_CatDatNum(data)
  gg_line_CatDatNum(data, title = "Nice chart", caption ="some caption")
  gg_line_CatDatNum(data, title = "Nice chart", caption ="some caption",
                    legend_position = "bottom")


  opts <- dsvizopts::dsviz_defaults()
  opts$theme$legend_position <- "bottom"

  l <- ggmagic_prep(data, opts, family = "line")
  l$legend$position

  expect_equal(lapply(l$d, class),
               list(a = "character",b = "Date", c = "numeric",
                    ..colors = "character"))



})

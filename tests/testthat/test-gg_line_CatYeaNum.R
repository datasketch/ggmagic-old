test_that("gg bar YeaNum", {


  data <- sample_data("Cat-Yea-Num", n = 100, rep = FALSE)

  gg_line_CatYeaNum(data)
  gg_line_CatYeaNum(data, title = "Nice chart", caption ="some caption")
  gg_line_CatYeaNum(data, title = "Nice chart", caption ="some caption",
                    legend_position = "top")


  opts <- dsvizopts::dsviz_defaults()
  opts$theme$legend_position <- "bottom"

  l <- ggmagic_prep(data, opts, family = "line")

  expect_equal(lapply(l$d, class),
               list(a = "character",b = "integer", c = "numeric",
                    ..colors = "character"))



})

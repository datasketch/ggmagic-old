test_that("gg_pie_CatNum", {

  data <- sampleData("Cat-Num")
  opts <- dsviz_default_opts()


  l <- ggmagic_prep(data, opts, extra_pattern = "pie")

  d$..ylabpos
  d

  gg_pie_CatNum(data, opts = list(dataLabels_show = TRUE))
  gg_pie_CatNum(data, dataLabels_location = 1.15,
                opts = list(dataLabels_show = TRUE,
                            title = "Hello"))

  gg_pie_Cat(data)



})

test_that("gg_pie_Cat", {

  data <- sampleData("Cat")
  opts <- dsviz_default_opts()
  gg_pie_CatNum(data)

})

test_that("gg_donut_CatNum", {

  data <- sampleData("Cat-Num")
  opts <- dsviz_default_opts()

  gg_donut_CatNum(data)

  opts <- list(dataLabels_show = TRUE)
  opts <- dsvizopts::merge_dsviz_options(opts)

  l <- ggmagic_prep(data, opts, extra_pattern = "donut")
  l$d
  gg_donut_CatNum(data, opts = list(dataLabels_show = TRUE))

  data <- sampleData("Cat")
  gg_donut_CatNum(data, opts = list(dataLabels_show = TRUE,
                                    donut_width = 0.8,
                                    donut_dataLabels_pos = 0.5))


})


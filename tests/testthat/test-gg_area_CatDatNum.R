test_that("gg area CatDatNum", {

  data <- tibble(
    cats = c(rep("First", 20),rep("Second", 10)),
    years = c(seq(as.Date("2010-05-04"), by = "day", length.out = 20),
              seq(as.Date("2010-05-11"), by = "day", length.out = 10)),
    nums = c(rep(1:5,4), 1:10)
  )
  opts <- dsvizopts::dsviz_defaults()
  l <- ggmagic_prep(data, opts, family = "area")
  d <- l$d


  gg_area_CatDatNum(data)

  gg_area_CatDatNum(data, graph_type = "stacked")

  gg_area_CatDatNum(data, area_alpha = 0.3)
  gg_area_CatDatNum(data, title = "Nice chart", caption ="some caption",
                    legend_position = "top", graph_type = "stacked",
                    area_alpha = 0.9)


})

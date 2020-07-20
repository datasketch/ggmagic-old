test_that("gg area CatYeaNum", {

  data <- tibble(
    cats = c(rep("First", 20),rep("Second", 10)),
    years = c(2000:2019,sample(2000:2010,10)),
    nums = c(rep(1:5,4), 1:10)
  )
  opts <- dsvizopts::dsviz_defaults()
  l <- ggmagic_prep(data, opts, family = "area")
  d <- l$d


  gg_area_CatYeaNum(data)

  gg_area_CatYeaNum(data, graph_type = "stacked")

  gg_area_CatYeaNum(data, area_alpha = 0.3)
  gg_area_CatYeaNum(data, title = "Nice chart", caption ="some caption")
  gg_area_CatYeaNum(data, title = "Nice chart", caption ="some caption",
                    legend_position = "top", graph_type = "stacked",
                    area_alpha = 0.9)


})

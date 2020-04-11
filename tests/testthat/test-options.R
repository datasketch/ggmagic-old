test_that("Options", {

  defaults <- list(a = "AAA", b = "BBB")
  # TODO Test default options against full list of params

  myfun <- function(x, ...){
    opts <- mergeOptions(..., defaults = defaults)
    opts
  }
  myfun_opts <- myfun(x = 0)
  # expect_equal(myfun_opts, discard(defaults, is.null))
  expect_equal(myfun_opts, defaults)

  myfun_opts <- myfun(x = 0, a = "a", b = "b")
  expect_equal(myfun_opts, list(a = "a", b = "b"))
  myfun_opts <- myfun(x = 0, agg = "MyAgg")
  expect_equal(myfun_opts$agg,"MyAgg")
  expect_equal(myfun_opts, c(defaults, list(agg = "MyAgg")))

  myfun_opts <- myfun(x = 0, opts = list(agg = "MyAggList", title = "New Title"))
  expect_equal(myfun_opts$agg,"MyAggList")
  expect_equal(myfun_opts$title,"New Title")
  expect_equal(myfun_opts, c(defaults, list(agg = "MyAggList", title =  "New Title")))
  myfun_opts <- myfun(x = 0, agg = "MyAgg", opts = list(agg = "MyAggOpts"))
  expect_equal(myfun_opts$agg,"MyAggOpts")

})

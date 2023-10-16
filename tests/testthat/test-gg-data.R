test_that("Data prep to make ggplots", {

  # si los datos ingresados no estan en formato hdtable hace la debida transformación
  d <- gg_transform_hdtable(data = iris, dic = NULL)
  expect_equal(class(d), c("hdtable", "R6"))
  expect_true(!is.null(d$dd))
  expect_true(!is.null(d$dic))
  expect_true(!is.null(d$data))


  df <- cars
  dic <- data.frame(id = names(df), label = c("Speed", "Distance"))
  df <- hdtable::hdtable(d = df, dic = dic)
  d <- gg_transform_hdtable(d = df, dic = NULL)
  expect_equal(df, d)

  # se hace una lista de tipos de datos de la base ingresada
  l_hdtype <- gg_hdtype(d$dic, vars = "speed")
  expect_equal(l_hdtype, list("Num" = "speed"))
  hdtype <- gg_extract_hdtype(l_hdtype)
  expect_equal(hdtype, "Num")

  # se extraen los datos y el tipo de datos que se quiere graficar
  #opts <- dsopts::dsopts_default("dataprep")
  opts <- list(agg = "sum")
  results <- gg_data(iris, vars = c("species", "petal_width"), opts = opts)
  expect_equal(names(results), c("data", "dic", "hdtype", "vars"))

})

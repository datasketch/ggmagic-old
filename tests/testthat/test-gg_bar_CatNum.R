test_that("gg bar Cat Num", {

  library(homodatum)

  data <- sample_data("Cat")
  gg_bar_Cat(data)

  # Error
  gg <- gg_bar_Cat(d = data.frame(x="1"), title = "Another Chart")
  gg


  data <- sample_data("Cat-Num")
  gg_bar_CatNum(data)
  gg_bar_CatNum(data, title = "datasketch", caption ="some caption")

  d <- palmerpenguins::penguins
  g <- gg_bar_CatNum(d %>% select(sex, body_mass_g),
                     title = "Penguins",
                     subtitle = as.character(Sys.time()))
  g

  #

  data <- palmerpenguins::penguins %>% select(sex, body_mass_g)
  opts <- opts <- dsvizopts::dsviz_defaults()

  gg_bar_CatNum(data, palette_colors = rainbow(3),color_by = 1)

  gg_bar_CatNum(data, palette_colors = rainbow(3),
                color_by = 1, sort = "asc", percentage = TRUE,
                format_sample_num = "2.16%", dataLabels_show = TRUE,
                dataLabels_format_sample = "2.16"
                )

  data <- sample_data("Cat-Num")
  gg <- gg_bar_CatNum(data, color_by = names(data)[1])
  gg

  gg_bar_CatNum(data, sort = "asc")

  gg_bar_CatNum(data, logo = "datasketch", caption ="some caption")

  gg_bar_CatNum(data, logo = "datasketch",
                caption ="some caption",
                color_by = names(data)[1])

  gg_bar_CatNum(data, logo = "datasketch",
                caption ="some caption",
                color_by = names(data)[2],
                ver_title = "Vertical",
                hor_title = "Horizontal")

  gg_bar_CatNum(data, logo = "datasketch",
                caption ="some caption",
                color_by = names(data)[1],
                orientation = "hor",
                ver_title = "Vertical",
                hor_title = "Horizontal")


  gg_bar_CatNum(data = data,
                agg = "mean",
                caption = "This is a caption",
                drop_na = TRUE,
                highlight_value = "CatB",
                highlight_value_color = "#CDA015",
                hor_label = "Horizontal axis",
                title = "This is a title",
                subtitle = "This is a subtitle",
                text_color = "#de8040",
                text_size = 10,
                ver_title = "Vertical axis")

  opts <- dsvizopts::dsviz_defaults()

  f <- homodatum::fringe(data)
  d <- fringe_d(f)
  summarizeData(d, opts$summarize$agg, to_agg = b, a)
  l <- ggmagic_prep(data, opts)

  opts$postprocess$sort <- "asc"
  l <- ggmagic_prep(data, opts)
  l$d


})

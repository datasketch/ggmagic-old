gg_validator <- function(dic, var_cat, viz = "pie") {
  dic <- dic |> filter(id %in% var_cat)
  leng_cat <- dic$format[[var_cat]][["n_categories"]]
  if (leng_cat > 6) {
    return(warning(paste0(viz, " Chart with More Than 6 Categories
It is not recommended to use a pie chart with more than 6 categories.",
viz," charts work best when representing a small number of categories to
easily visualize proportions. When you have too many categories,
the slices become small and difficult to distinguish, making it challenging to
interpret the data effectively.

Consider using alternative chart types such as bar charts or treemaps for datasets
with a larger number of categories. These chart types provide a clearer way to display and
compare data when dealing with multiple categories.")))
  } else {
    return()
  }
}



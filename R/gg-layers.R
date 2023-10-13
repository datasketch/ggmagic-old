
gg_basic <- function(data, x_col, y_col, viz = "bar") {

  gg <- switch(viz,
               bar = ggplot(data = data,
                            mapping = aes(x = .data[[x_col]], y = .data[[y_col]])) +
                 geom_bar(stat = "identity"),

               pie = ggplot(data,
                            mapping = aes(x = "", y = .data[[y_col]], fill = .data[[x_col]])) +
                 geom_bar(stat="identity", width=1, color="white") +
                 coord_polar("y", start=0),

               stop(paste("Visualización", viz, "no reconocida"))
  )

  return(gg)
}


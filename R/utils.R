generate_colors <- function(initial_colors, n) {
  if (length(initial_colors) >= n) {
    return(initial_colors[1:n])
  }

  palette_function <- colorRampPalette(initial_colors, space = "rgb")
  extended_palette <- palette_function(n)

  return(extended_palette)
}

find_extreme_colors <- function(colors) {
  # Convertir colores a matriz RGB
  rgb_matrix <- t(sapply(colors, function(col) col2rgb(col)/255))

  # Convertir colores RGB a espacio CIELab
  lab_colors <- grDevices::convertColor(rgb_matrix, from = "sRGB", to = "Lab", scale.in = 1)

  # Identificar el color más claro y el más oscuro basado en luminosidad 'L'
  lightest_color <- colors[which.max(lab_colors[,1])]
  darkest_color <- colors[which.min(lab_colors[,1])]

  return(list(low = lightest_color, high = darkest_color))
}


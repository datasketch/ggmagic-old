gg_bar_text_position <- function(data = NULL,
                                 datalabel_inside = FALSE,
                                 bar_orientation = "hor",
                                 bar_graph_type = "basic") {

  hjust_setting <- 0.5
  vjust_setting <- 0.5
  # Configuraciones para gráficos de barras agrupadas
  if (bar_graph_type == "grouped") {
    if (datalabel_inside) {
      hjust_setting <- 1.5
    } else {
      hjust_setting <- -0.2
    }

    # Ajustes de orientación
    if (bar_orientation == "ver") {
      hjust_setting <- 0.5
      vjust_setting <- ifelse(datalabel_inside, 1.5, -0.2)
    } else {
      vjust_setting <- 0.5
    }

    # Configuraciones para gráficos de barras apiladas
  } else if (bar_graph_type == "stacked") {
    if (bar_orientation == "hor") {
      hjust_setting <- ifelse(datalabel_inside, 1.5, -0.09)
    } else {
      vjust_setting <- ifelse(datalabel_inside, 1.5, -0.2)
    }

    # Configuraciones para gráficos de barras básicos
  } else {
    if (bar_orientation == "hor") {
      hjust_setting <- ifelse(datalabel_inside, 1.5, -0.09)
    } else {
      vjust_setting <- ifelse(datalabel_inside, 1.5, -0.2)
    }
  }

  return(list(vjust = vjust_setting, hjust = hjust_setting))
}

gg_add_text <- function(gg, viz, opts) {

  if (!opts$datalabel_show) return(gg)

  if (viz == "bar") {
    just_settings <- gg_bar_text_position(datalabel_inside = opts$datalabel_inside,
                                          bar_orientation = opts$bar_orientation,
                                          bar_graph_type = opts$bar_graph_type)
    if (opts$bar_graph_type == "grouped") {
      gg + geom_text( aes(label = ..label), vjust = just_settings$vjust, hjust = just_settings$hjust,
                      position = position_dodge(width = 1), inherit.aes = TRUE)
    } else if (opts$bar_graph_type == "stacked") {
      gg + geom_text(aes(y = label_y, label = ..label), vjust = "center")
    } else {
      gg + geom_text(aes(label = ..label, vjust = just_settings$vjust, hjust = just_settings$hjust))
    }
  }

  if (viz == "pie") {
    gg + geom_text(aes(y = ypos, label = ..label))
  }

  if (viz == "donut") {
    gg + geom_text(x = 3.5, aes(y = label_position, label = ..label))
  }

}





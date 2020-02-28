# save plot
#' @export
save_ggmagic <- function (viz, filename, format = NULL, width = 10, height = 7, ...) {
  if (is.null(format)) {
    format <- file_ext(filename) %||% "png"
  }
  filename <- file_path_sans_ext(filename)
  tmp <- paste(tempdir(), "svg", sep = ".")
  svglite::svglite(tmp, width = width, height = height)
  print(viz)
  dev.off()
  bitmap <- rsvg::rsvg(tmp, height = 500)
  if (format == "png") {
    png::writePNG(bitmap, paste0(filename, ".", format), dpi = 144)
  }
  if (format == "jpeg") {
    jpeg::writeJPEG(bitmap, paste0(filename, ".", format))
  }
  if (format == "svg") {
    rsvg::rsvg_svg(tmp, paste0(filename, ".", format))
  }
  if (format == "pdf") {
    rsvg::rsvg_pdf(tmp, paste0(filename, ".", format))
  }
}


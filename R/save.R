
#' @export
save_ggmagic <- function(p, filename,format = NULL, width = NULL, height = NULL){
  format <- file_ext(filename) %||% "png"
  width <- width %||% 12
  height <- height %||% 8
  filename <- paste0(file_path_sans_ext(filename),'.',format)
  ggsave(filename, plot = p, width = width, height = height,units = "cm")
}

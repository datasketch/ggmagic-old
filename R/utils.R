# mergeOptions <- function(opts,defaultOpts){
#   optNames <- names(defaultOpts)
#   o <- list()
#   for(i in optNames){
#     o[[i]] <- opts[[i]] %||% defaultOpts[[i]]
#   }
#   o
# }


sysfile <- function(..., package = "ggmagic"){
  if (is.null(package)){
    path = file.path(...)
  } else {
    path = system.file(..., package = package)
  }
  path
}

#' @export
`%||%` <- function (x, y)
{
  if (is_empty(x))
    return(y)
  else if (is.null(x) || is.na(x))
    return(y)
  else if( class(x)=="character" && nchar(x)==0 )
    return(y)
  else x
}


is.empty <- function(x){
  if(length(x) == 0) return(TRUE)
  if(length(x) == 1 && nchar(x) == 0) return(TRUE)
  !as.logical(length(x))
}

# is.empty <- function(x){
#   #   !is.null(x)
#   !as.logical(length(x))
# }


removeNulls <- function(x){
  if (length(x) == 0 || !is.list(x))
    return(x)
  if(is.empty(x)) return(list())
  x[!unlist(lapply(x,is.null))]
}


file_path_sans_ext <- function (x)
{
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

file_ext <- function (x)
{
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}



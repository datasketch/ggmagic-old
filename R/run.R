#' @import dsvizopts
#' @export
run_gg <- function(d,ggname){

  if(validateD(d,ggname))
    do.call(ggname,list(d))
  else
    stop("D did not validate")
}

validateD <- function(d,ggname){
  guessFtype(d) %in% ggFtype(ggname)
}



#' @export
ggWhich <- function(d){
  pf <- ggFtype()
  ftype <- guessFtype(d) # TODO possibleFtypes
  names(keep(pf, ~ ftype %in% .))
}


#' @export
ggList <- function(){
  #http://stackoverflow.com/questions/7495685/how-to-access-the-help-documentation-rd-source-files-in-r
  db <- Rd_db("ggmagic")
  meta <- unname(map_chr(db, tools:::.Rd_get_name))
  keep(meta, ~ grepl("^gg_.*\\.$",.))
}

#' @export
ggFtype <- function(gg = NULL){
  db <- Rd_db("ggmagic")
  meta <- lapply(db, tools:::.Rd_get_section, "section")
  cleanFtypeDoc <- function(ftype){
    ftype <- as.character(ftype[[2]][[2]])
    strsplit(gsub(" |\n","",ftype),",")[[1]]
  }
  meta <- lapply(meta,cleanFtypeDoc)
  names(meta) <- gsub(".Rd","",names(meta))
  if(!is.null(gg)) return(meta[[gg]])
  meta
}



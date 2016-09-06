#' @export
getEscherMeta <- function(){
  db <- Rd_db("escher")
  desc <- lapply(db, tools:::.Rd_get_metadata, "description")
  desc <- desc[grepl("escher_",names(desc),fixed = TRUE)]
  parseDocDescription(desc)
}

parseDocDescription <- function(desc){
  meta <- lapply(seq_along(desc),function(i){
    d <- desc[[i]]
    r <- list()
    name <- names(desc)[i]
    r$name <- gsub(".Rd","",name)
    r$description <- d[2,]
    meta <- d[3:nrow(d),]
    meta <- strsplit(meta,":")
    metaNames <- unlist(Map(function(m)m[[1]],meta))
    metaNames <- gsub(":$","",metaNames,perl=TRUE)
    metaValues <- Map(function(m){
      x <- paste(m[-1],collapse = "")
      gsub("^ ","",x,perl=TRUE)
      },meta)
    names(metaValues) <- metaNames
    c(r, metaValues)
  })
  meta
}



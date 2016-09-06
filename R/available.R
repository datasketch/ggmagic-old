

#' @export
whichggmagic <- function(d){
  pf <- ggmagicFtypes()
  possibleFtype <- guessFtype(d)
  #add parsable ftypes
  (pf %>% filter(ftype == possibleFtype))$ggmagicId
}


#' @export
availableggmagic <- function(){
  meta <- system.file("meta.yaml",package="ggmagic", mustWork=TRUE)
  l <- yaml.load_file(meta)
  ids <- list.mapv(l, id)
  #ftypes <- list.map(l, fringes[[1]]$ftypes)
  #names(ftypes) <- ids
  #d <- melt(ftypes)
  #list.stack(ftypes)
  ids
}

#' @export
ggmagicFtypes <- function(){
  meta <- system.file("meta.yaml",package="ggmagic", mustWork=TRUE)
  l <- yaml.load_file(meta)
  ids <- list.mapv(l, id)
  ftypes <- list.map(l, fringes[[1]]$ftypes)
  names(ftypes) <- ids
  d <- melt(ftypes)
  names(d) <- c("ftype","ggmagicId")
  d
}

#' @export
printggmagic <- function(ids = NULL){
  avIds <- availableggmagic()
  if(is.null(ids))
    ids <-avIds
  else{
    if(!all(ids %in% avIds))
      stop("ggmagic ids not found")
  }
  path <- system.file("imgs",package="ggmagic", mustWork=TRUE)
  pathTpl <- paste0(file.path(path,"{id}"),".png")
  imgTags <- lapply(ids,function(id){
    imgPath <- pystr_format(pathTpl,list(id=id))
    src <- knitr::image_uri(imgPath)
    img(src=src, width="100%", style="max-width:400px")
  })
  html_print(imgTags)
}



#' @export
ggWhich <- function(d){
  pf <- ggFtype()
  ftype <- guessFtype(d) # TODO possibleFtypes
  names(keep(pf, ~ ftype %in% .))
}


#' @export
ggList <- function(type = NULL,wrongNames = FALSE){
  #http://stackoverflow.com/questions/7495685/how-to-access-the-help-documentation-rd-source-files-in-r
  db <- tools::Rd_db("ggmagic")
  meta <- unname(map_chr(db, tools:::.Rd_get_name))
  meta <- meta[!grepl("gg_test_docs",meta)]
  if(wrongNames) return(keep(meta, ~ !grepl("^gg_.*\\.$",.)))
  ggs <- keep(meta, ~ grepl("^gg_.*\\.$",.))
  if(!is.null(type))
    return(ggs[grepl(type,ggs)])
  ggs
}

#' @export
ggFtype <- function(gg = NULL){
  db <- tools::Rd_db("ggmagic")
  db <- db[grepl("\\.\\.Rd$",names(db))]
  names(db) <- gsub(".Rd","",names(db))
  i <<- 1
  f <- function(dbi) {
    # dbi <- db[[i-1]]
    i <<-  i + 1
    x <- as.character(dbi)
    xx <- x[which(x == "ftypes"):which(x == "\\examples")]
    xxx <- xx[which(xx == "\n"):which(xx == "\\examples")]
    xxx <- xxx[xxx != "\n" & xxx != "}" & xxx != "\\examples"]
    xxx <- gsub(" |\n", "", xxx)
    xxx <- xxx[!duplicated(xxx)]
    ftype <- as.character(paste(xxx, collapse = ", "))
    strsplit(gsub(" |\n","",ftype),",")[[1]]
  }
  results <- purrr::map(db,f)
  names(results) <- gsub(".Rd","",names(results))

  #meta <- map(db, tools:::.Rd_get_section, "section")
  #meta <- meta[!grepl("gg_test_docs",names(meta))]
  #ftype <- meta$gg_test_docs.Rd

  # safe_cleanFtypeDoc <- safely(ggmagic:::cleanFtypeDoc)
  # parsedMeta <- map(meta,safe_cleanFtypeDoc)
  # results <- parsedMeta %>% map(~.$result)
  # errors <- parsedMeta %>% map(~.$error) %>% purrr::discard(is.null)
  # names(results) <- gsub(".Rd","",names(results))
  # names(errors) <- gsub(".Rd","",names(errors))
  #if(!is_empty(errors))
  #  stop("Something wrong with ftypes for:\n",paste(names(errors),collapse = "\n  "))
  if(!is.null(gg)) return(results[[gg]])
  results
}

#' @export
ggNames <- function(gg = NULL){
  db <- tools::Rd_db("ggmagic")
  db <- db[grepl("\\.\\.Rd$",names(db))]
  names(db) <- gsub(".Rd","",names(db))
  i <<- 1
  f <- function(dbi) {
    # dbi <- db[[i-1]]
    i <<-  i + 1
    x <- as.character(dbi)
    xx <- x[which(x == "\\title"):which(x == "\\name")]
    xxx <- xx[which(xx == "{"):which(xx == "}")]
    xxx <- xxx[xxx != "\n" & xxx != "{" & xxx != "}"]
    xxx <- xxx[-length(xxx)]
    xxx <- gsub("\n", "", xxx)
    xxx <- xxx[!duplicated(xxx)]
    ftype <- as.character(paste(xxx, collapse = ""))
  }
  results <- purrr::map(db,f)
  names(results) <- gsub(".Rd","",names(results))

  if(!is.null(gg)) return(results[[gg]])
  results
}


# cleanFtypeDoc <- function(ftype){
#   sectionName <- as.character(ftype[[1]][[1]])
#   if(sectionName != "ftypes") stop("No section name ftype")
#   ftype <- as.character(ftype[[2]][[2]])
#   strsplit(gsub(" |\n","",ftype),",")[[1]]
# }


check_fonts <- function(theme){
  families <- unique(unlist(theme[grepl("family", names(theme))]))
  ok <- families %in% extrafont::fonttable()$FamilyName
  if(!all(ok)){
    stop("Font not found in system: ", paste0(families[!ok],collapse = ", "))
  }

}


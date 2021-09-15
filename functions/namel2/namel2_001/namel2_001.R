#helper function (convert vector to named list)
namel2 <- function (vec){
  tmp <- as.list(1:length(vec))
  names(tmp)<-as.character(unlist(vec))
  tmp
}
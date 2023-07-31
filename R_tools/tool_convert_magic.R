convert_magic <- function(obj,types){
  for (i in 1:length(obj)){
    temp <- switch(types[i],character = as.character)
    obj[,i] <- temp(obj[,i])
  }
  obj
}
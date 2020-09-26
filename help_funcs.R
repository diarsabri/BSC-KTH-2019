GOOGLE_MAPS_KEY = #

funct <- function(arg1) {
  tmp <- fromJSON(url(paste("https://maps.googleapis.com/maps/api/place/textsearch/json?query=",arg1,"&key=AIzaSyCZG-cND0N33EKE9DzZNmQn3AQnIPqocyI")))
  tmp2 <- tmp[["results"]][["formatted_address"]]
  tmp3 <- tmp[["results"]][["geometry"]][["location"]][["lng"]]
  tmp4 <- tmp[["results"]][["geometry"]][["location"]][["lat"]]
  return(c(tmp2,tmp3,tmp4))
}

rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)

  pattern <- unique(pattern)

  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"

  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )

  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )

  accentTypes <- c("´","`","^","~","¨","ç")

  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))

  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)

  return(str)
}

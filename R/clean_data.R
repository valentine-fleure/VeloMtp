#' CLean data 
#' 
#' @description fonction pour enlever les majuscules, la ponctuation, les espaces et les accents d'un objet
#'
#' @param data x l'objet que l'on veut clean
#'
#' @return x l'objet netooyé
#' @export
#'

clean_data<-function(x){
  x<-gsub("[[:punct:]]", "", x) #enlever les tirets, ponctuations
  x<-tolower(x) #enlever les majuscule
  x<-gsub("[[:space:]]", "", x)#enlever les espace 
  x<-iconv(x, to="ASCII//TRANSLIT")#enlever les accents
  
  return(x)
}

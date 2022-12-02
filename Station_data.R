

#' Nombre de velos par station
#'
#' @param data --> jeu de donnée 
#' @param col --> colonne pour le compatge
#' @param Fusion 
#'
#' @return data frame avec nombre de velo par station
#' @export
#'
Data_station <- function(data, col, Fusion){
  compt_station <- as.data.frame(janitor::tabyl(data, col))
  compt_station <- compt_station[compt_station[1]<59,]

names(compt_station)<-c("numero", "n", "percent")
Station<-merge(Fusion,compt_station,by="numero", all=TRUE)
  
return(Station)}



#' Reorganise l'ordre d'un data frame
#'
#' @param data data frame a ranger
#' @param ascend TRUE=ordre croissant, FALSE=ordre decroissant
#' @param value colone du data frame pour faire l'ordre
#'
#' @return data frame reorganisé
#' @export
#'
#'
Data_order<- function(data, ascend, value){
  if (ascend==TRUE){
    order=Station %>% arrange(value)}  #ordre croissant
  else{order=Station %>% arrange(desc(value))} #ordre decroissant
  return(order)
}









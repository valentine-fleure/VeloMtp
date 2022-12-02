#' Domaine vitale du cycliste
#'
#' @param Course_velo2 
#' @param Fusion 
#'
#' @return ggplot avec le domaine vital
#' @export
#'
domaine_vitale <- function (Course_velo2, Fusion){
  
  list_point=data.frame(c(Course_velo2$Departure.station, Course_velo2$Return.station))
  
  ID=as.character(paste("Utilisateur :", Course_velo2$new_account[1]))
    
  names(list_point)<-c("numero")
  Coordo<-merge(Fusion,list_point,by="numero", all=TRUE)
  
  Coordo<-Coordo[Coordo$numero!=98,]
  Coordo$x <- as.numeric(Coordo$x)
  Coordo$y<- as.numeric(Coordo$y)

  names(Coordo)=c("numero", "Nom", "Nombre.totales.de.places", "y","x", "type_stati"   )
  data<-Coordo
  
  kernel<-adehabitatHR::kernelUD(sp::SpatialPoints(data[,c("x","y")]),h="href", grid=300)
  
  pourcent=c(95,90,85,75,50,25)
  Kernel_df=data.frame()
  KDAREAS=data.frame()
  
  for (i in 1:length(pourcent)) {
    pour=pourcent[i]
    
    kernel.poly <- adehabitatHR::getverticeshr(kernel, percent = pour)
    
    kdareas=fortify(kernel.poly)
    kdareas$id=pour
    
    res=c(pour,kernel.poly$area)
    Kernel_df=rbind(Kernel_df,res)
    KDAREAS=rbind(KDAREAS,kdareas)
  }
  
  ### surface kernel 
  names(Kernel_df)=c("pourcent", "area")
  Kernel_df$area=Kernel_df$area*1e8     # faire *1e8 pour avoir en km2 
  #print(Kernel_df$area)
  
  KDAREAS$id=as.factor(KDAREAS$id)    
  
  ### Plot kernel 
  
  map <- ggmap::get_stamenmap(bbox = c(top=43.7, bottom=43.5, right=4, left=3.8), zoom = 13 )
  
  titre=as.character(paste(ID, " - aire du domaine vital :", round(Kernel_df$area[1], digit=2), "km2"))
  color=viridis(6)
  plot=ggmap(map)+
    geom_point(data=data,aes(x=x, y=y), alpha=0.2)+
    geom_polygon(data=KDAREAS[KDAREAS$id==95,], aes(x=long, y=lat,  fill=id, colour=id, group=piece), alpha = 0.3)+
    geom_polygon(data=KDAREAS[KDAREAS$id==90,], aes(x=long, y=lat, fill=id,  colour=id, group=piece), alpha = 0.3)+
    geom_polygon(data=KDAREAS[KDAREAS$id==85,], aes(x=long, y=lat,  fill=id, colour=id, group=piece), alpha = 0.3)+
    geom_polygon(data=KDAREAS[KDAREAS$id==75,], aes(x=long, y=lat,  fill=id, colour=id, group=piece), alpha = 0.3)+
    geom_polygon(data=KDAREAS[KDAREAS$id==50,], aes(x=long, y=lat,  fill=id, colour=id, group=piece), alpha = 0.3)+
    labs( x = "Longitude", y = "Lattitude",
          title =titre)+  theme(legend.position = "bottom")+
    theme_minimal()+
    scale_fill_manual(name="Kernel %",values = c(color[6], color[5], color[4], color[3], color[2]))+
    scale_color_manual(name="Kernel %", values = c(color[6], color[5], color[4], color[3], color[2]))
  
return(plot)  
}
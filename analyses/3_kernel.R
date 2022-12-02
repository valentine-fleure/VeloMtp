#domaine vitale 
Course_velo <- read.csv2(here::here(file.path("data","derived-data", "Course_velo.csv")), sep="")
Course_velo2<-Course_velo[Course_velo$new_account=="18982",] 

Fusion <- read.csv2(here::here(file.path("data","derived-data", "Fusion.csv")), sep="")

list_point=data.frame(c(Course_velo2$Departure.station, Course_velo2$Return.station))


names(list_point)<-c("numero")
Coordo<-merge(Fusion,list_point,by="numero", all=TRUE)

Coordo<-Coordo[Coordo$numero!=98,]

Coordo$x <- as.numeric(Coordo$x)
Coordo$y<- as.numeric(Coordo$y)
summary(Coordo)

names(Coordo)=c("numero", "Nom", "Nombre.totales.de.places", "y","x", "type_stati"   )
data<-Coordo

library(adehabitatHR)

kernel=kernelUD(SpatialPoints(data[,c("x","y")]),h="href", grid=300)

pourcent=c(99,95,90,85,75,50,25)
Kernel_df=data.frame()
KDAREAS=data.frame()

for (i in 1:length(pourcent)) {
  pour=pourcent[i]
  
  kernel.poly <- getverticeshr(kernel, percent = pour)
  
  kdareas=fortify(kernel.poly)
  kdareas$id=pour
  
  res=c(pour,kernel.poly$area)
  Kernel_df=rbind(Kernel_df,res)
  KDAREAS=rbind(KDAREAS,kdareas)
}

### surface kernel 
names(Kernel_df)=c("pourcent", "area")
Kernel_df$area=Kernel_df$area*1e8     # faire *1e8 pour avoir en km2 
print(Kernel_df$area)

KDAREAS$id=as.factor(KDAREAS$id)    
summary(KDAREAS)

### Plot kernel 
color=viridis(6)
ggplot(KDAREAS) +
  geom_point(data=data,aes(x=x, y=y), alpha=0.2)+
  geom_polygon(data=KDAREAS[KDAREAS$id==99,], aes(x=long, y=lat,  fill=id, colour=id, group=piece), alpha = 0.2)+
  geom_polygon(data=KDAREAS[KDAREAS$id==95,], aes(x=long, y=lat,  fill=id, colour=id, group=piece), alpha = 0.2)+
  geom_polygon(data=KDAREAS[KDAREAS$id==90,], aes(x=long, y=lat, fill=id,  colour=id, group=piece), alpha = 0.2)+
  geom_polygon(data=KDAREAS[KDAREAS$id==85,], aes(x=long, y=lat,  fill=id, colour=id, group=piece), alpha = 0.2)+
  geom_polygon(data=KDAREAS[KDAREAS$id==75,], aes(x=long, y=lat,  fill=id, colour=id, group=piece), alpha = 0.2)+
  geom_polygon(data=KDAREAS[KDAREAS$id==50,], aes(x=long, y=lat,  fill=id, colour=id, group=piece), alpha = 0.2)+
  labs( x = "Longitude", y = "Lattitude",
        title ="Site 89 [FRP-BA11317]")+  theme(legend.position = "bottom")+
  scale_fill_manual(name="Kernel %",values = c(color[6],color[5], color[4], color[3], color[2], color[1]))+
  scale_color_manual(name="Kernel %", values = c(color[6], color[5], color[4], color[3], color[2], color[1]))




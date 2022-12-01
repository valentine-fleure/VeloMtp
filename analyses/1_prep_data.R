################# 1- PREPARATION DES DONNEES

### data avec 
Course_velo=read.csv2(here::here(file.path("data","raw-data", "CoursesVelomagg.csv")), sep=";")
Station=read.csv2(here::here(file.path("data","raw-data", "StationVelomagg.csv")), sep=",")
Coord=read.csv2(here::here(file.path("data","raw-data", "DisponibiliteVelomagg.csv")), sep=";")

# prepartion data coordonnée
# extraction coordonnées GPS
Coord$x<- sapply(strsplit(Coord$geopoint, ","), "[[", 1)
Coord$y <- sapply(strsplit(Coord$geopoint, ","), "[[", 2)
Coord<-Coord[,c("Nom", "Nombre.totales.de.places", "x", "y")]

Station<-Station[,c("nom", "numero", "type_stati")]
names(Station)<-c("Nom", "numero", "type_stati")

Fusion<-merge(Coord,Station,by="Nom", all=TRUE)

Coord$Nom<-clean_data(Coord$Nom)


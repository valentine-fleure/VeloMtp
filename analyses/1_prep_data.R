################# 1- PREPARATION DES DONNEES

### data avec 
Course_velo <- read.csv2(here::here(file.path("data","raw-data", "CoursesVelomagg.csv")), sep=";")
Station <- read.csv2(here::here(file.path("data","raw-data", "StationVelomagg.csv")), sep=",")
Coord <- read.csv2(here::here(file.path("data","raw-data", "DisponibiliteVelomagg.csv")), sep=";")

Course_velo <- Course_velo[Course_velo$Departure.station != "", ] #on enlmeve les elements vides
Course_velo <- Course_velo[Course_velo$Return.station != "", ]

# prepartion data coordonnée
# extraction coordonnées GPS
Coord$x<- sapply(strsplit(Coord$geopoint, ","), "[[", 1)
Coord$y <- sapply(strsplit(Coord$geopoint, ","), "[[", 2)
Coord<-Coord[,c("Nom", "Nombre.totales.de.places", "x", "y")]

Station<-Station[,c("nom", "numero", "type_stati")]
names(Station)<-c("Nom", "numero", "type_stati")

#clean nom station
Coord$Nom<-clean_data(Coord$Nom)
Station$Nom<-clean_data(Station$Nom)

Coord$Nom[Coord$Nom=="parvisjulesferrygaresaintroch"]<-"parvisjulesferry"
Coord$Nom[Coord$Nom=="perolsetangdelor"]<-"perols"
Coord$Nom[Coord$Nom=="pontdelattesgaresaintroch"]<-"pontdelattesgarestroch"
Coord$Nom[Coord$Nom=="ruejulesferrygaresaintroch"]<-"ruejulesferry"
Coord$Nom[Coord$Nom=="suddefrance"]<-"montpelliersuddefrance"

Fusion<-merge(Coord,Station,by="Nom", all=TRUE)

test=strsplit(Course_velo$Departure.station, " ")
test2=unlist(test)

Course_velo$Departure.station <- as.numeric(sapply(strsplit(Course_velo$Departure.station, split = " "), "[[", 1))
Course_velo$Return.station <- as.numeric(sapply(strsplit(Course_velo$Return.station, split = " "), "[[", 1))

Course_velo=Course_velo[c("new_account", "Departure",  "Return", "Bike", "Departure.station", "Return.station", "Covered.distance..m.", "Duration..sec..",              
                         "Departure.temperature...C.")]

####enregistrement 
write.table(Course_velo, here::here(file.path("data/derived-data/Course_velo.csv")),
            col.names = TRUE, row.names = FALSE)
write.table(Fusion, here::here(file.path("data/derived-data/Fusion.csv")),
            col.names = TRUE, row.names = FALSE)



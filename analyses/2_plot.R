

path_courses = here::here("data","derived-data", "Course_velo.csv")
Course_velo = load_csv(path_courses, sep = " ")

path_fusion = here::here("data","derived-data", "Fusion.csv")
Fusion = load_csv(path_fusion, sep = " ")

      
#### heatmap
tabCont=as.data.frame.matrix(table(Course_velo$Departure.station, Course_velo$Return.station))
tabCont=tabCont[1:58, 1:58]
tabCont=tabCont[-57, -57]

names(tabCont)=order_depnumero$Nom
rownames(tabCont)=order_depnumero$Nom

library(heatmaply)
heatmaply(as.matrix(tabCont), 
               dendrogram = "none",
               xlab = "", ylab = "", 
               main = "",
               scale = "column",
               margins = c(60,100,40,20),
               grid_color = "white",
               grid_width = 0.00001,
               titleX = FALSE,
               hide_colorbar = TRUE,
               branches_lwd = 0.1,
               label_names = c("Station de depart ", "Station d'arrivé ", "Value"),
               fontsize_row = 5, fontsize_col = 5,
               labCol = colnames(as.matrix(tabCont)),
               labRow = rownames(as.matrix(tabCont)),
               heatmap_layers = theme(axis.line=element_blank()))

library(tidyverse)

##### Etude point de depart - ppint d'arrivé
Depstation <- Data_station(Course_velo, "Departure.station",  Fusion)
Returnstation <- Data_station(Course_velo, "Return.station",  Fusion)

data=Depstation

#### ordre 
# Departure
order_dep=data %>% arrange(desc(n))# arrangement selon n
order_depnumero=data %>% arrange(numero)# arrangement selon numeros 

#### table - les 10 premiers sites les plus utilisés
Dep_tab=order_dep[1:10,c("Nom", "n")]
knitr::kable(Dep_tab, caption="Nombre de velos au départ des 10 Station les plus importantes")

### plot graphique 
PLOT(order_dep,"Nombre de vélos selon les stations de depart" )

#leaflet  --> faire fonction pour arrivé
leaflet_map(data, "Stations de depart")





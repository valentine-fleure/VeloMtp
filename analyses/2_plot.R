library(tidyverse)

Course_velo <- read.csv2(here::here(file.path("data","derived-data", "Course_velo.csv")), sep="")
Fusion <- read.csv2(here::here(file.path("data","derived-data", "Fusion.csv")), sep="")

Fusion$numero[Fusion$numero==0]<-57
Fusion$numero[Fusion$numero==59]<-58

compt_depstation <- as.data.frame(janitor::tabyl(Course_velo, Departure.station))
compt_depstation <- compt_depstation[compt_depstation$Departure.station<59,]

names(compt_depstation)<-c("numero", "n", "percent")
Depstation<-merge(Fusion,compt_depstation,by="numero", all=TRUE)

order_dep=Depstation %>% arrange(desc(n))  # arrangement selon n
order_numero=Depstation %>% arrange(numero)

names(Course_velo)=c("new_account" , "Departure", "Return", "Bike", "Departure.station" ,      
                     "Return.station", "Covered.distance", "Duration", "Temperature")

Course_velo$Departure<-as.Date(Course_velo$Departure)
Course_velo2<-Course_velo[Course_velo$Departure<="2022-01-31",]  #seulement mois de janvier

#### table - les 10 premiers sites les plus utilisés
library(knitr)
Dep_tab=order_dep[1:10,c("Nom", "n")]
kable(Dep_tab, caption="Nombre de velos au depart de chaque Station")

### plot graphique 
library(ggplot2)
library(viridis)


hist=ggplot(order_dep, aes(n, stats::reorder(Nom,n), fill=n)) + 
  scale_fill_viridis_c()+
  geom_bar(stat="identity") +
  theme_minimal()+
  labs(title = 'Nombre de vélos selon les stations',
       x = 'Nombre de vélos', y = "Stations") +
  theme(axis.text.x = element_text(), legend.position = "none")

plotly::ggplotly(hist)
              
hist=ggplot(order_dep, aes(n, stats::reorder(Nom,n), fill=n)) + 
  scale_fill_viridis_c()+
  geom_bar(stat="identity") +
  theme_minimal()+
  labs(title = 'Nombre de vélos selon les stations',
       x = 'Nombre de vélos', y = "Stations") +
  theme(axis.text.x = element_text(), legend.position = "none")

plotly::ggplotly(hist)   
      
#### heatmap
tabCont=as.data.frame.matrix(table(Course_velo$Departure.station, Course_velo$Return.station))
tabCont=tabCont[1:58, 1:58]
tabCont=tabCont[-57, -57]

names(tabCont)=order_numero$Nom
rownames(tabCont)=order_numero$Nom

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


#leaflet  --> faire fonction pour arrivé
Depstation$x=as.numeric(Depstation$x)
Depstation$y=as.numeric(Depstation$y)
mean(Depstation$x)

library(leaflet)
pal <- colorNumeric(palette = "Spectral",
                    domain = Depstation$n)

leaflet(Depstation) %>%
  addTiles() %>%
  setView(lng = mean(Depstation$y), lat = mean(Depstation$x), zoom = 15)%>%
  addCircleMarkers(lng = ~y, lat = ~x, weight = 1,
                   radius = 7, popup = ~Nom,
                   color = ~pal(Depstation$n), fillOpacity=10)%>%
  addLegend("topleft", pal = pal, values = ~Depstation$n,
            title = "Year SB",
            opacity = 1)%>%
  addLabelOnlyMarkers(~Depstation$x, ~Depstation$y, label =  ~as.character(Depstation$Nom), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))

# trop moche --> trop de site
library(ggridges)
library(hrbrthemes)
Course_velo2$Departure.station=as.factor(Course_velo2$Departure.station)

ggplot(Course_velo2, aes(x = Covered.distance, y = Departure.station)) +
  geom_density_ridges()
  
  
  
  

  
  library(ggraph)
  ggraph(Course_velo2, layout = 'dendrogram', circular = TRUE) + 
    geom_conn_bundle(data = get_con(from = Course_velo2$Departure.station, to = Course_velo2$Return.station), alpha=0.2, colour="skyblue", tension = .5) + 
    geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05)) +
    theme_void()



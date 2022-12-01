Course_velo <- read.csv2(here::here(file.path("data","derived-data", "Course_velo.csv")), sep="")
Fusion <- read.csv2(here::here(file.path("data","derived-data", "Fusion.csv")), sep="")

Fusion$numero[Fusion$numero==0]<-57
Fusion$numero[Fusion$numero==59]<-58

compt_depstation <- as.data.frame(janitor::tabyl(Course_velo, Departure.station))
compt_depstation <- compt_depstation[compt_depstation$Departure.station<59,]

names(compt_depstation)<-c("numero", "n", "percent")
Depstation<-merge(Fusion,compt_depstation,by="numero", all=TRUE)


#### table 
library(knitr)
Dep_tab=Depstation[c("Nom", "n")]
kable(Dep_tab, caption="Nombre de velos au depart de chaque Station")

### plot 
library(ggplot2)
library(viridis)
library(tidyverse)

hist=ggplot(order_dep, aes(n, stats::reorder(Nom,n), fill=n)) + 
  scale_fill_viridis_c()+
  geom_bar(stat="identity") +
  theme_minimal()+
  labs(title = 'Nombre de vélos selon les stations',
       x = 'Nombre de vélos', y = "Stations") +
  theme(axis.text.x = element_text(), legend.position = "none")

plotly::ggplotly(hist)
                 
order_dep=Depstation %>% arrange(desc(n))        

#### heatmap
tabCont=table(Course_velo$Departure.station, Course_velo$Return.station)
tabCont
corrplot::cor(as.data.frame(tabCont))
corrplot::corrplot(, method="circle", 
                   type="upper", order="hclust", tl.col="black", tl.srt=45)





pal <- colorNumeric(palette = "RdPu",
                    domain = data$YearSB)

leaflet(points) %>%
  addTiles() %>%
  setView(lng = 4.387285, lat = 43.58985, zoom = 8)%>%
  addCircleMarkers(lng = ~longitudes, lat = ~latitudes, weight = 1,
                   radius = 7, popup = ~labels,
                   color = ~pal(data$YearSB), fillOpacity=1)%>%
  addLegend("topleft", pal = pal, values = ~data$YearSB,
            title = "Year SB",
            opacity = 1)%>%
  addLabelOnlyMarkers(~data$X, ~data$Y, label =  ~as.character(data$Site), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T))

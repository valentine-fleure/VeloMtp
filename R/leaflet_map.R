
#' Map leaflet 
#'
#' @param Depstation fichier station departure ou return
#' @param title titre graphique
#'
#' @return map leaflet
#' @export
#'
leaflet_map <- function(Depstation, title){
Depstation$x=as.numeric(Depstation$x)
Depstation$y=as.numeric(Depstation$y)

library(leaflet)
pal <- colorNumeric(palette = "Spectral",
                    domain = Depstation$n)

return(leaflet(Depstation) %>%
  addTiles() %>%
  setView(lng = mean(Depstation$x), lat = mean(Depstation$y), zoom = 12)%>%
  addCircleMarkers(lng = ~x, lat = ~y, weight = 1,
                   radius = 7, popup = ~Nom,
                   color = ~pal(Depstation$n), fillOpacity=10)%>%
  addLegend("topleft", pal = pal, values = ~Depstation$n,
            title = title,
            opacity = 1)%>%
  addLabelOnlyMarkers(~Depstation$y, ~Depstation$x, label =  ~as.character(Depstation$Nom), 
                      labelOptions = labelOptions(noHide = T, direction = 'top', textOnly = T)))
  
}

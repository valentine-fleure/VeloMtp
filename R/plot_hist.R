#' Plot histograme Utilisation velo
#'
#' @param data Departure ou Return
#' @param title nom du graph
#'
#' @return ggplot histograme avec plotly
#' @export
#'
#'
PLOT <- function (data, title){
  library(ggplot2)
  library(viridis)

  hist=ggplot(data, aes(n, stats::reorder(Nom,n), fill=n)) + 
    scale_fill_viridis_c()+
    geom_bar(stat="identity") +
    theme_minimal()+
    labs(title = title,
         x = 'Nombre de vélos', y = "Stations") +
    theme(axis.text.x = element_text(), legend.position = "none")
  
  return(plotly::ggplotly(hist))
}

library(ggplot2)

#' Plot
#' Plot and save the plot
#'
#' @param courses_month data
#' @param month
#'
#' @return No return value
#' @export
#'
#' @examples
plot_temperature <- function(courses_month, month) {
  courses_month = courses_month[courses_month$Covered.distance <= 1000, ]
  courses_month = courses_month[courses_month$Duration <= 20000, ]
  
  plot = ggplot(courses_month,
                aes(x = Duration, y = Covered.distance, color = Temperature)) +
    geom_point(alpha = 0.5) +
    theme_classic() +
    xlab("Temps de trajet (min)") +
    ylab("Distance parcourue (km)")
  
  filename = paste0("temps_temp_dist_", month, ".png")
  ggsave(here::here("outputs", filename), plot)
}

#' Load_csv
#'
#' @param path
#' @param sep
#'
#' @return data frame
#' @export
#'
#' @examples
load_csv <- function(path, sep) {
  x = read.csv2(path, sep = sep)
  return(x)
}

#' Download course
#' Download course csv 
#' https://data.montpellier3m.fr/sites/default/files/ressources/TAM_MMM_CoursesVelomagg.csv
#' 
#' @return No return value
#' @export
#'
#' @examples
download_course <- function() {
  url = "https://data.montpellier3m.fr/sites/default/files/ressources/TAM_MMM_CoursesVelomagg.csv"
  filename = "CoursesVelomagg.csv"
  path = here::here("data", "raw-data")
  utils::download.file(url = paste0(url),
                       destfile = file.path(path, filename))
}

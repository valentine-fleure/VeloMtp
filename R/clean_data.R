#' Clean data
#'
#' @description fonction pour enlever les majuscules, la ponctuation, les espaces et les accents d'un objet
#'
#' @param data x l'objet que l'on veut clean
#'
#' @return x l'objet nettoyé
#' @export
#'

clean_data <- function(x) {
  x <- gsub("[[:punct:]]", "", x) #enlever les tirets, ponctuations
  x <- tolower(x) #enlever les majuscule
  x <- gsub("[[:space:]]", "", x)#enlever les espace
  x <- iconv(x, to = "ASCII//TRANSLIT")#enlever les accents
  
  return(x)
}

#' clean_courses
#' Clean the dataframe and save it in data/derived-data
#'
#' @param courses a data frame data/raw-data/CoursesVelomagg.csv
#'
#' @return courses
#' @export
#'
#' @examples
clean_courses <- function(courses) {
  # Colonnes util
  courses = courses[c(
    "new_account",
    "Departure",
    "Return",
    "Bike",
    "Departure.station",
    "Return.station",
    "Covered.distance..m.",
    "Duration..sec..",
    "Departure.temperature...C."
  )]
  # On enleve les stations vides
  courses <- courses[courses$Departure.station != "",]
  courses <- courses[courses$Return.station != "",]
  
  # On recupere les id des stations
  courses$Departure.station <-
    as.numeric(sapply(strsplit(courses$Departure.station, split = " "), "[[", 1))
  courses$Return.station <-
    as.numeric(sapply(strsplit(courses$Return.station, split = " "), "[[", 1))
  
  write.table(courses,
              here::here(file.path("data/derived-data/Course_velo.csv")),
              col.names = TRUE,
              row.names = FALSE)
  
  return(courses)
}

#' 
#' Clean, merge station dataframes and save in data/derived-data
#'
#' @param station data frame data/raw-data/StationVelomagg.csv
#' @param coord data frame data/raw-data/DisponibiliteVelomagg.csv
#'
#' @return fusion data frame 
#' @export
#'
#' @examples
clean_station <- function(station, coord) {
  # extraction coordonnées GPS
  coord$x <- sapply(strsplit(coord$geopoint, ","), "[[", 1)
  coord$y <- sapply(strsplit(coord$geopoint, ","), "[[", 2)
  coord <- coord[, c("Nom", "Nombre.totales.de.places", "x", "y")]
  
  #Select column
  station <- station[, c("nom", "numero", "type_stati")]
  names(station) <- c("Nom", "numero", "type_stati")
  
  #clean nom station
  coord$Nom <- clean_data(coord$Nom)
  station$Nom <- clean_data(station$Nom)
  
  coord$Nom[coord$Nom == "parvisjulesferrygaresaintroch"] <-
    "parvisjulesferry"
  coord$Nom[coord$Nom == "perolsetangdelor"] <- "perols"
  coord$Nom[coord$Nom == "pontdelattesgaresaintroch"] <-
    "pontdelattesgarestroch"
  coord$Nom[coord$Nom == "ruejulesferrygaresaintroch"] <-
    "ruejulesferry"
  coord$Nom[coord$Nom == "suddefrance"] <- "montpelliersuddefrance"
  
  fusion <- merge(coord, station, by = "Nom", all = TRUE)
  
  write.table(fusion,
              here::here(file.path("data/derived-data/Fusion.csv")),
              col.names = TRUE,
              row.names = FALSE)
  
  return(fusion)
}

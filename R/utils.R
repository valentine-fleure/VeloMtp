#' Extract data
#' Extrait les donnees relatives à un mois
#'
#' @param courses dataframe
#' @param month str exemple "06"
#'
#' @return dataframe
#' @export
#'
#' @examples
extract_data <- function(courses, month) {
  if (stringr::str_length(month) != 2) {
    stop("Le mois doit être écrit sous la forme 01, 02, ..., 12", call. = FALSE)
  }
  deb_month = paste0("01-", month, "-2022")
  fin_month = paste0("31-", month, "-2022")
  courses_month = courses[courses$Departure >= deb_month &
                            courses$Departure <= fin_month, ]
  return(courses_month)
}
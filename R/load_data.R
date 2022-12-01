#' Load_csv
#'
#' @param path 
#' @param sep 
#'
#' @return data frame
#' @export
#'
#' @examples
load_csv <- function(path, sep){
  x = read.csv2(path, sep=sep)
  return(x)
}

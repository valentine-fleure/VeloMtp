load_csv <- function(path, sep){
  x = read.csv2(path, sep=sep)
  return(x)
}

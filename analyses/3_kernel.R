#domaine vitale 
Course_velo <- read.csv2(here::here(file.path("data","derived-data", "Course_velo.csv")), sep="")
Course_velo2<-Course_velo[Course_velo$new_account=="18982",] 

Fusion <- read.csv2(here::here(file.path("data","derived-data", "Fusion.csv")), sep="")

domaine_vitale(Course_velo2, Fusion)






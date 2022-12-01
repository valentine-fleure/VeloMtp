################# 1- PREPARATION DES DONNEES

### Load

# Courses velo
path_courses = here::here("data","raw-data", "CoursesVelomagg.csv")
Course_velo = load_csv(path_courses, sep = ";")

# Station
path_station = here::here("data","raw-data", "StationVelomagg.csv")
Station = load_csv(path_station, sep = ",")

# Coordonnees
path_coord = here::here("data","raw-data", "DisponibiliteVelomagg.csv")
Coord = load_csv(path_coord, sep = ";")

### Clean

# Courses velo
Course_velo = clean_courses(Course_velo)

# Stations
Fusion = clean_station(Station, Coord)
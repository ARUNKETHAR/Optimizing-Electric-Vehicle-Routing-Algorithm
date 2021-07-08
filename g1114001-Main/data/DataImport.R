#
# written by Tyler Messerly
#

import_data <- function(){
  ##IMPORTS ALL EXITS AND POPULATION DENSITY DATA
  library(readr)
  
  Midwest_Exits <<- read_csv("MidwestExitsData.csv");          #LAT/LONG of all exits in the midwest
  West_Exits <<- read_csv("WestExitsData.csv");                #LAT/LONG of all exits in the west
  Pop_Density <<- read.csv("County_Info.csv", header = TRUE);  #LAT/LONG and population density of each county by state
  
}


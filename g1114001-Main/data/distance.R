#
# written by Annah Aunger
#

distance <- function(point1, point2) {
  
##this function takes two points by (lat, long) 
# and returns the distance in miles
  
#install.packages('geosphere')
library(geosphere)

#point1 = c(latitude, longitude)
#distGeo takes long, lat as vectors of the points
point1_ = c(point1[2],point1[1])
point2_ = c(point2[2], point2[1])
  
distMeters <-distGeo(point1_, point2_)
distMiles<-0.000621371*distMeters
distMiles

}

#
# written by Annah Aunger
#

check150_1 <- function(p_points){
  ##checks if the distance between two points is <= 150 miles and 
  # stores the distance to the next adjacent 'TRUE' point
  
  prospective_points <- p_points
  
  source("distance.R")
  
  #adds column of NA distances
  prospective_points$distAdjacent <- rep(NA, length(prospective_points$latitude))
  
  ##check if adjacent points are <=150 miles
  for (i in 1:(length(prospective_points$isUrban))) { #outer loop assigns the first true point
    
    if(prospective_points[i,3] == TRUE){ #find if current point is true
      index1 <- i
      index2 <- i + 1
      
      #assign the point a lat and long
      point1 <- c(prospective_points[index1, 1], prospective_points[index1,2]) #lat, long
      
      # what if this was the last true? (check that index does not equal end)
      while(prospective_points[index2,3] != TRUE && index2 != length(prospective_points$latitude)) { 
        index2 <- index2 + 1
        
      } #will find the next adjacent TRUE or index2 will reach end of dataframe
      
      if(index2 == length(prospective_points$latitude)) { # index one was the last true in the df
        prospective_points[index1, 4] = 0 #set value to 0, bc should not be na value
        break #exit the for loop
      }
        
      point2 <- c(prospective_points[index2, 1], prospective_points[index2, 2])
      
    
      #adjDistance <- distance(point1, point2)
        
      ##if the <250 miles criteria is not met, find the point that would satisfy the criteria
      while(distance(point1, point2) > 150) {
        index2 <- index2 - 1 #find a closer point 
        point2 <- c(prospective_points[index2, 1], prospective_points[index2, 2]) #reassign point
      }
    
      #point2 should now be <=150 miles
      #mark the isUrban to TRUE so the next part of the loop will use that as a valid prospective point
      prospective_points[index2, 3] <- TRUE
    
      #update the distance column
      adjDistance <- distance(point1, point2)
      prospective_points[index1,4] <- adjDistance
      
    } # end if
  } #end for loop
  
  #check if the last point is true, because it shouldn't have an "na" so just place a zero there
  if(prospective_points[(length(prospective_points$latitude)),3] == TRUE){
    prospective_points[(length(prospective_points$latitude)),4] <- 0
  }
  
  
  #return value
  prospective_points
}


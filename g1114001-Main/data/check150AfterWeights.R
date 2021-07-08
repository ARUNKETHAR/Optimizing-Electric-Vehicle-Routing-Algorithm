#
# written by Annah Aunger
#


find_local_max <- function(index1, index2, prospective_points){
  
  ## given a dataframe with lats, longs, and weights,
  # will return the local maximum weight between two indexes
  
  index1 <- i1
  index2 <- i2
  
  point2 <- c(prospective_points[i2-1,1], prospective_points[i2-1,2]) #assign point
  
  max_index <- i2 - 1 # set max to next lower index
  
  while(i2 > i1) {
    i2 <- i2 - 1 #find a closer point
    
    #find local max between index1 and 2
    if(prospective_points[i2,5] > prospective_points[max_index,5]){
      max_index <- i2
      point2 <- c(prospective_points[i2,1], prospective_points[i2,2]) #reassign point
    }
    
  } # end while
  
  #return value = array with index of local max and the lat long of the point
  
  c(max_index, point2)
  
}


check150_2 <- function(prospective_points, n ){
  
  ## sorts the prospective points by descreasing weight
  # labels n chargers as selected for the given prospective points
  # checks if the distance between two adjacent points is <= 150 miles
  # reassigns selctions if 150 mile criteria is not met
  
  if ( n < length(prospective_points$latitude) ) {
    paste("smaller distances between prospective points is needed to place charging station locations")
  }
  
  source("distance.R")
  
  #add a "placed" column
  prospective_points$placed <- rep(FALSE, length(prospective_points$latitude))
  
  #go through the dataframe and store the indexes of max weights in order descending
  sorted_indexes <- order(-prospective_points$weights)
  
  num_placed <- 0 #number of chargers placd is currently zero
  
  #"place" the chargers by max weight
  while(num_placed < n){
    #mark the "placed" column as true
    prospective_points[(sorted_indexes[num_placed + 1]),6] <- TRUE
    #increment the num_placed variable
    num_placed <- num_placed + 1
  }
  
  
  
  #check that the chargers that are picked are no greater than 150 miles apart
  for (j in 1:(length(prospective_points$latitude))) {
    if(prospective_points[j, 6] == TRUE) { ##if this point was selected
      index1 <- j
      index2 <- j + 1
    } else {
      break # we need to find the next value that is true 
    }
    
    point1 <- c(prospective_points[index1, 1], prospective_points[index1,2]) #lat, long
    
    while(prospective_points[index2,6] != TRUE && index2 != length(prospective_points$latitude)){
      index2 <- index2 + 1
    } # will find the next TRUE or will reach the end of the dataframe
    
    if(index2 == length(prospective_points$latitude)) { # index one was the last true in the df
      break #exit the for loop
    }
    
    point2 <- c(prospective_points[index2, 1], prospective_points[index2, 2])
  
    
    ##if the <150 miles criteria is not met, find the point that would satisfy the criteria 
    # that has the next highest weight
    # mark the placed on previous index2 to FALSE as well
    
    index2_before = index2
    
    while( distance(point1, point2) > 150){ # checks 150 miles criteria
      temp <- find_local_max(index1, index2, prospective_points) #finds local max btwn indexes
      index2 <- temp[1] #reassigns index2
      point2 <- temp[2] #reassigns point2
    }
    
    #point2 should now be <=150 miles
    #mark TRUE so the next part of the loop will use that as a valid placed
    prospective_points[index2, 6] <- TRUE
    #mark old index2 as FALSE because we didnt end up choosing it
    prospective_points[index2_before, 6] <- FALSE
  
  } #end for
  
  prospective_points
}

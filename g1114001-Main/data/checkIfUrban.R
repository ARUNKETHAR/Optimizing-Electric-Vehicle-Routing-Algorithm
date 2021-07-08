#
# written by Tyler Messerly
#

##Check if a specific prospective point is urban (population density > 100 ppl/sq. mi.)

checkIfUrban <- function(prospective_points){
  source("distance.R");
  source("DataImport.R");
  import_data();
  count1 <- 1;

  point_density <-c();
  isUrban <- c();
  
  #Loop through all the prospective points
  while(count1 <= nrow(prospective_points)){
    min_dist <- 10000;
    #Loop through all county locations
    count2 <- 1;
    while(count2 <= nrow(Pop_Density)){
      #Find the distance between the county and the prospective point
      select_dist <- distance(c(prospective_points[count1,1],prospective_points[count1,2]),c(Pop_Density[count2,3],Pop_Density[count2,4]));
      #Check if distance is the closest of all checked so far
      if(select_dist < min_dist){
        min_dist <- select_dist;
        #Record the population density for new minimum distance
        #point_density <- append()
        point_density <- Pop_Density[count2,5];
        point_density <- as.numeric(as.character(point_density));
      }
      count2 <- count2 + 1;
    }
    
    #Given the population density to a point, determine and record if urban or not
    if(point_density < 100){
      isUrban <- append(isUrban,"TRUE");
    }
    else{
      isUrban <-append(isUrban,"FALSE");
    }
    count1 <- count1 + 1;
    
  }
  
  
  #Put isUrban data in the dataframe as a new column
  prospective_points$isUrban <- isUrban;
  
  #Remove all Urban areas from dataframe
  #prospective_points <- prospective_points[!(prospective_points$isUrban == "FALSE"),];
  prospective_points;
}


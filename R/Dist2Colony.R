#' Dist2Colony Calculate the distance of a vector of points (lat,long) to a a fixed point
#'
#' @author Abram B. Fleishman <abram.fleishman AT sjsu.edu>

#' @param tracks a dataframe
#' @param ColonyLong a longitude value of length = 1
#' @param ColonyLat a latitude value of length = 1
#' @return a vector of distances from adjacent points in meters
#' @examples
#' Dist2Colony( tracks=tracks, ColonyLat=56.22234,ColonyLong=-164.45922)
#'
#' @export
Dist2Colony<-function(tracks,ColonyLat,ColonyLong){

  Point2Colony<-vector(mode = "numeric",length = nrow(tracks))
  for(i in 1:length(tracks$Latitude)){ #this is a for loop
    # This is a function to calculate distance between two points from the argosfilter package
    Point2Colony[i]<-argosfilter::distance(lat1 = ColonyLat,lon1 = ColonyLong ,lat2 = tracks$Latitude[i],lon2 = tracks$Longitude[i])

  }
  return(Point2Colony)
}

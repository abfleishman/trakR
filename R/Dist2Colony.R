#' Dist2Colony Calculate the distance of a vector of points (lat,long) to a fixed point
#'
#'Calculate the distance of a vector of points (lat,long) to a fixed point
#'
#' @author Abram B. Fleishman \email{abram@@conservationmetrics.com}
#'
#' @param tracks a dataframe
#' @param dataLat quoted latitude
#' @param dataLon quoted longitude
#' @param ColonyLong a longitude value of length = 1
#' @param ColonyLat a latitude value of length = 1
#' @return a vector of distances from adjacent points in kilometers
#' @export

Dist2Colony<-function(tracks,dataLat="lat",
                      dataLon="lon",
                      ColonyLat,
                      ColonyLong){

  Point2Colony<-vector(mode = "numeric",length = nrow(tracks))

  for(i in 1:nrow(tracks)){
    # This is a function to calculate distance between two points from the
    # argosfilter package
    Point2Colony[i]<-distance(lat1 = ColonyLat,
                                           lon1 = ColonyLong ,
                                           lat2 = tracks[[dataLat]][i],
                                           lon2 = tracks[[dataLon]][i])

  }
  return(Point2Colony)
}

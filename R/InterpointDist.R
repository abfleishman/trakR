#' InterpointDist Calculate the Distance between points on a track for each bird
#' Vector of distances between points
#'
#' @author Abram B. Fleishman \email{abram@@conservationmetrics.com}
#' @param tracks data.frame of data to be queried for  lat and long values
#' @param ID quoted name of column in data that is and ID field
#' @param lat quoted name of column in data that has latitude values
#' @param lon quoted name of column in data that has longitude values
#' @return A vector of distances in meters between adjacent points in an animal track
#' @export

InterpointDist<-function(tracks,
                         ID="CaptureID",
                         lat="Latitude",
                         lon="Longitude"){

  dataOut<-NULL
  Birds<-unique(tracks[[ID]])

  for(i in 1:length(Birds)){
    Data<-tracks[tracks[[ID]] == Birds[i],]
    # argosfilter::distanceTrack is a function to calculate distance between
    # points along a track
    InterpointDist<-c(NA,
                      round(distanceTrack(lat = Data[[lat]],
                                          lon = Data[[lon]])*1000,
                            digits=1))
    dataOut<-c(dataOut, InterpointDist)
  }
  return(dataOut)
}


#' distanceTrack (from argosfilter)
#'
#' @param lat vector of latitudes
#' @param lon vector of longitudes
#'
#' @export
#'
distanceTrack <- function(lat, lon){
  d<-numeric(length(lat)-1)
  for (k in 1:(length(lat)-1)){
    lat1<-lat[k]
    lat2<-lat[k+1]
    lon1<-lon[k]
    lon2<-lon[k+1]
    d[k]<-distance(lat1,lat2,lon1,lon2)
  }
  d
}


#' Distance (from argosfilter)
#'
#' @param lat1 latitude of first point
#' @param lat2 longtitude of first point
#' @param lon2 lonitude of second point
#'
#' @export
#'
distance <-  function(lat1, lat2, lon1, lon2){
  if (lat1==lat2 & lon1==lon2) dd<-0 else {
    rlat1=radian(lat1)
    rlat2=radian(lat2)
    rlon=radian(lon2-lon1)
    dd<-60*(180/pi)*acos(sin(rlat1)*sin(rlat2)+cos(rlat1)*cos(rlat2)*cos(rlon))
    dd<-dd*1852/1000 #distance is given in km
  }
  dd
}

#' degree to radian
#'
#' @param degree degrees
#'
#' @export
#'
radian <-  function(degree){
  degree*(pi/180)
}
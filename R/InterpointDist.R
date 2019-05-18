#' InterpointDist Calculate the Distance between points on a track for each bird
#' Vector of distances between points
#'
#' @author Abram B. Fleishman \email{abram@@conservationmetrics.com}
#' @param tracks data.frame of data to be queried for  lat and long values
#' @param ID quoted name of column in data that is and ID field
#' @param lat quoted name of column in data that has latitude values
#' @param lon quoted name of column in data that has longitude values
#' @importFrom argosfilter distanceTrack
#' @return A vector of distances in meters between adjacent points in an animal track
#' @export

InterpointDist<-function(tracks,
                         ID="CaptureNum",
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

#' BearingFromPoint Calculate bearing between two points along a vector
#'
#' @author Abram B. Fleishman \email{abram@@conservationmetrics.com}
#'
#' @param tracks a dataframe
#' @param ID a quoted string indicating the column name for a unique ID key
#' @param lat a quoted string indicating the column name for longitude values
#' @param lon a quoted string indicating the column name for latitude values
#' @return a vector of bearings from adjacent points
#' @export
#' @importFrom geosphere bearing
#' @importFrom dplyr lead

BearingFromPoint<-function(tracks,
                           ID = "File",
                           lat = "Latitude",
                           lon = "Longitude"){

    # remove bad positions
  tracks<-as.data.frame(tracks)

  tracks<- tracks[ tracks[lon] > -180 & tracks[lon] < 180 & tracks[lat] > -90 & tracks[lat] < 90,]
  dataOut<-NULL

  Birds<-unique(tracks[[ID]])

  for(i in 1:length(Birds)){
    Data<-tracks[tracks[[ID]]==Birds[i],]

    BearingfromPoint<-c(round(
      bearing(
        cbind(Data[[lon]],Data[[lat]]),
        cbind(lead(Data[[lon]]), lead(Data[[lat]]))),
      digits=1))

    dataOut<-c(dataOut,BearingfromPoint)

  }
  return(dataOut)
}

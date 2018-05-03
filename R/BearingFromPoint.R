#' BearingFromPoint Calculate bearing between two points in a vector
#'
#' @author Abram B. Fleishman <abram.fleishman AT sjsu.edu>

#' @param tracks a dataframe
#' @param ID a quoted string indicating the column name for a unique ID key
#' @param lat a quoted string indicating the column name for longitude values
#' @param lon a quoted string indicating the column name for latitude values
#' @return a vector of bearings from adjacent points
#' @examples
#'  \dontrun{Bering<-BearingFromPoint(tracks,ID="File", lat="Latitude",lon="Longitude")}
#' @export
BearingFromPoint<-function(tracks,ID = "File" , lat = "Latitude", lon = "Longitude"){

    # remove bad positions
  tracks<-as.data.frame(tracks)

  tracks<- tracks[ tracks[lon] > -180 & tracks[lon] < 180 & tracks[lat] > -90 & tracks[lat] < 90,]
  dataOut<-NULL

  Birds<-unique(tracks[[ID]])

  for(i in 1:length(Birds)){
    Data<-tracks[tracks[[ID]]==Birds[i],]

    BearingfromPoint<-c(round(
      geosphere::bearing(
        cbind(Data[[lon]],Data[[lat]]),
        cbind(dplyr::lead(Data[[lon]]), dplyr::lead(Data[[lat]]))),
      digits=1))
    dataOut<-c(dataOut,BearingfromPoint)
  }
  return(dataOut)
}

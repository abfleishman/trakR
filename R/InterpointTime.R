#' InterpointTime Calculate the time between points on a track for each bird
#'
#' Vector of times between points
#'
#' @author Abram B. Fleishman <abram.fleishman AT sjsu.edu>
#' @param tracks data.frame of data to be queried for lat and long values
#' @param ID quoted name of column in data that is and ID field
#' @param DateTime quoted name of column in data that has DateTime values in the format YYYY-mm-dd HH:MM:SS

#' @return A vector of times differnces in seconds between adjacent points in an animal track
#' @export
#############################################################################################
# Calculate time between points in a timeseries
#############################################################################################

InterpointTime<-function(tracks, ID="File", DateTime="DateTime"){

    # Initialize a vector wehere the data will be dumped, for time differences.
  dataOut <- NULL
  Birds <- unique(tracks[[ID]])

  # Run a for loop, where for each unique key, it subsets the data by that key and calculates the difference in time.
  for(i in 1:length(Birds)) {

    Data<-tracks[tracks[[ID]] == Birds[i],]

    Data$PointDur <- NA
    Data$PointDur <- dplyr::lead(Data[[DateTime]]) - Data[[DateTime]]

    dataOut<-c(dataOut,Data$PointDur)
  }

  return(dataOut)
}
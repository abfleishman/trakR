#' InterpointTime Calculate the time between points on a track for each bird
#'
#' Vector of times between points
#'
#' @author Abram B. Fleishman \email{abram@@conservationmetrics.com}
#' @param tracks data.frame of data to be queried for lat and long values
#' @param ID quoted name of column in data that is and ID field
#' @param DateTime quoted name of column in data that has DateTime values in the
#'   format YYYY-mm-dd HH:MM:SS
#' @importFrom dplyr lead
#' @return A vector of times differences in seconds between adjacent points in
#'   an animal track
#' @export
#############################################################################################
# Calculate time between points in a timeseries
#############################################################################################

InterpointTime<-function(tracks, ID="File", DateTime="DateTime"){
  ## this should be a test for if the df is sorted correctly but it is not
  ## working and I do not care enough right now to figure it out
  # tracks_sorted<-tracks %>% arrange(ID,DateTime)
  #
  # if(!all(tracks$Timestamp==tracks_sorted$Timestamp  )){
  #   stop("Dataframe not sorted by ID and datetime. To accurately calculate interpoint time you must sort your data first!")
  # }
  # Initialize a vector where the data will be dumped, for time differences.
  dataOut <- NULL
  Birds <- unique(tracks[[ID]])

  # Run a for loop, where for each unique key, it subsets the data by that key
  # and calculates the difference in time.
  for(i in 1:length(Birds)) {

    Data<-tracks[tracks[[ID]] == Birds[i],]

    Data$PointDur <- NA
    # force difftime in secs
    Data$PointDur <- as.numeric(difftime(time1 =  lead(Data[[DateTime]]),
                                         time2 = Data[[DateTime]],
                                         units = "secs"))

    dataOut<-c(dataOut,Data$PointDur)
  }

  return(dataOut)
}

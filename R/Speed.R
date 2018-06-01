#' Speed Calculate the instantanious speed at each point along on a track for
#' each bird Vector of times between points
#'
#' @author Abram B. Fleishman <abram.fleishman AT sjsu.edu>
#' @param Dist vector of distances between points (m)
#' @param Time vector of times between points (Seconds)

#' @return A vector of speeds (km/h) between adjacent points in an animal track
#' @export

Speed <- function(Dist , Time ){
  round( (as.numeric(Dist) / 1000) / (as.numeric(Time) / (60 * 60)), digits = 3)
}

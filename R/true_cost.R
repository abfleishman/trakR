#' Calculate true cost of flight
#'
#' Calculate the true cost of flight with the wind
#'
#' @param wind.speed a vector of wind speed in meters per second
#' @param bird.bearing.to a vector bering from the previous point to the current point along a tajectory(in degrees)
#' @param wind.bearing.to a vector bering for the wind trajectory (in degrees)
#'
#' @importFrom dplyr case_when
#' @export
#' @author Abram B. Fleishman \email{abram@@conservationmetrics.com}
#' @author Caitlin Kroeger \email{ckroeger@@ucsc.edu}

true_cost<-function(wind.speed, bird.bearing.to, wind.bearing.to){


  HRMA<-ifelse(abs(bird.bearing.to-wind.bearing.to)>180,
               360-abs(bird.bearing.to-wind.bearing.to),
               abs(bird.bearing.to-wind.bearing.to))

  HF <- case_when(HRMA<=1 ~ 0.1,
                  HRMA>1 ~ 2*HRMA)

  true_cost <- (1/wind.speed)*HF
  return(true_cost)
}

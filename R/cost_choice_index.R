#' Calculate cost choice index for a single point
#'
#' Calculate the cost choice index for a single point
#'
#' @param wind.speed a single wind speed in meters per second
#' @param bird.bearing.to a single bering from the previous point to the current point along a tajectory(in degrees)
#' @param wind.bearing.to a single bering for the wind trajectory (in degrees)
#'
#' @importFrom dplyr case_when
#' @importFrom purrr pmap_dbl
#' @export
#' @author Abram B. Fleishman \email{abram@@conservationmetrics.com}
#' @author Caitlin Kroeger \email{ckroeger@@ucsc.edu}

cost_choice_index<-function(wind.speed, bird.bearing.to, wind.bearing.to){
  pmap_dbl(list(wind.speed=wind.speed,
                bird.bearing.to=bird.bearing.to,
                wind.bearing.to=wind.bearing.to),function(wind.speed, bird.bearing.to, wind.bearing.to){
                  # horizontal relative moving angle
                  # angle of the birds trajectory relative to the wind azimuth
                  HRMA<-ifelse(abs(bird.bearing.to-wind.bearing.to)>180,
                               360-abs(bird.bearing.to-wind.bearing.to),
                               abs(bird.bearing.to-wind.bearing.to))

                  # horizontal factor
                  # incremental penalization of angular deviations from the wind direction
                  HF <- case_when(HRMA<=1 ~ 0.1,
                                  HRMA>1 ~ 2*HRMA)

                  # true cost for the chosen trajectory
                  true_cost <- (1/wind.speed)*HF

                  # calculate all the other possible berings in 15 degree increments
                  bearings<-seq(bird.bearing.to,bird.bearing.to+360,15)
                  bearings<-ifelse(bearings>360,bearings-360,bearings)

                  # using those bearings to calculate alternative costs
                  HRMAs<-ifelse(abs(bearings-wind.bearing.to)>180,
                                360-abs(bearings-wind.bearing.to),
                                abs(bearings-wind.bearing.to))


                  HFs <- case_when(HRMAs<=1 ~ 0.1,
                                   HRMAs>1 ~ 2*HRMAs)

                  alternative_costs <- (1/wind.speed)*HFs


                  #  caluclate the choice index as the proportion of time the alt cost was < the
                  #  true cost high value means birds made less efficent choice.
                  cost_choice_index<-sum(alternative_costs<true_cost)/length(bearings)

                  return(cost_choice_index)
                })
}

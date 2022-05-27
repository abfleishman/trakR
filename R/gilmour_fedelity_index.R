#' Gilmour's Fidelity Index
#'
#' Used for calculation of Fidelity Index as described by: Gilmour, ME, JA
#' Castillo-Guerrero, AB Fleishman, S Hernandez-Vazquez, HS Young, SA Shaffer.
#' 2018. Plasticity of foraging behaviors in response to diverse environmental
#' conditions. Ecosphere 9(7):e02301. https://doi.org/10.1002/ecs2.2301 (open access)
#'
#' This function compares maximum displacement (distance and angle) of two
#' separate trips from a point of origin. Multiple displacement values can be
#' compared. These displacement values are centered and scaled to create an
#' index of site fidelity that ranges -1 to 1.
#'
#' The equations used in this script are modified from:
#'  Willis-Norton et al. 2015. Deep-Sea Research Part II: Topical Studies in Oceanography 113:260-267
#'  Hazen et al. 2016. Journal of Applied Ecology 54:1415-1428, and
#'  Shaffer et al. 2017. Movement Ecology 5:27 https://doi.org/10.1186/s40462-017-0118-9.
#'
#' @param dat a data frame of tracking data with columns that include positions
#'   (longitude/Latitude, Easting/Northing,etc), an animal ID, unique trip IDs
#'   (unique to each animal/trip combo), a column with a distance to the colony,
#'   and a column with a bearing from each point to the colony.
#' @param Longitude quoted column name for location in the x direction.  can be
#'   longitude or easting or any other x coordinator
#' @param Latitude quoted column name for location in the y direction.  can be
#'   latitude or northing or any other y coordinator
#' @param animal_id quoted column name of a unique ID field for each animal
#' @param trip_id quoted column name of a unique ID field for each animal/trip
#'   (can be a combo of animalID and a trip number for that animal)
#' @param distance2colony quoted column name of a distance to the colony (units
#'   do not matter)
#' @param bear2col quoted column name of a bearing in degrees from each point to
#'   the colony
#' @importFrom dplyr group_by filter select bind_rows ungroup n summarise
#' @importFrom magrittr %>%
#' @importFrom rlang !! sym
#' @importFrom utils combn
#' @return a list of data.frames with the fidelity index for each trip
#'   combination (fidelity_index_trips ) and an average fidelity index for each animal (fidelity_index_animal)
#' @author Morgan E. Gilmour \email{morgan.gilmour@@bucknell.edu}
#' @author Abram B. Fleishman \email{abram@@conservationmetrics.com}
#' @export

gilmour_fidelity<-function(dat, Longitude, Latitude, animal_id,
                           trip_id, distance2colony, bear2col){
  fed<-dat %>%
    # filter(!!sym(trip_id)!="0") %>% # TODO: remove trip "0" from trips. see #10 on github.
    group_by(!!sym(animal_id),!!sym(trip_id)) %>%
    # filter the max distance to colony for each animal trip
    filter(!!sym(distance2colony)==max(!!sym(distance2colony),na.rm=T)) %>%
    dplyr::select(!!sym(animal_id),
                  !!sym(trip_id),
                  !!sym(bear2col),
                  !!sym(distance2colony)) %>%
    ungroup
  # loop to calculate the delta dist and angle for each pair of trips within
  # each animal
  animals<-unique(fed[[animal_id]])
  FI<-NULL
  for(i in 1:length(animals)){
    # subset each animal
    a<-fed[fed[[animal_id]]==animals[i],]
    # skip if there are not two trips
    if(nrow(a)<2) {
      message("Skipping: ",animals[i], " only 1 trip")
      next
    }
    # make all the combinations of trips  to loop through
    b<-t(combn(a[[trip_id]],2))
    # make data.frame to be populated
    b<-data.frame(animal_id = animals[i],TripNuma=b[,1],TripNumb=b[,2],FI=NA,distA=NA,distB=NA,bearA=NA,bearB=NA)

    # Loop through each row and populate the ditance and bearing for each trip
    # and then the difference between them
    for(j in 1:nrow(b)){
      b$distA[j]<-a[[distance2colony]][a$TripNum==b$TripNuma[j]]
      b$distB[j]<-a[[distance2colony]][a$TripNum==b$TripNumb[j]]
      b$bearA[j]<-a[[bear2col]][a$TripNum==b$TripNuma[j]]
      b$bearB[j]<-a[[bear2col]][a$TripNum==b$TripNumb[j]]
      # delta distance is the absolute difference between a and b divided by the larger dist
      b$deltaDistance[j]<-(abs(b$distA[j]-b$distB[j])/max(c(b$distA[j],b$distB[j])))
      # absolute value of diff of bearing
      b$deltaAngle[j]<-(abs(b$bearA[j] - b$bearB[j]))
      # FI is intermidiary step no final product
      b$FI[j]<-2*(b$deltaDistance[j] + (b$deltaAngle[j]/90))
    }
    FI<-bind_rows(FI,b)
  }

  # Delta distance currently ranges 0 (the same) and 1 (different).
  # Divide by -1 to flip the scale (-1=different, 0=same).
  # Add 1 to shift scale so it ranges 0 (different) to +1 (same).
  FI$deltaDistance.flip<- 1+(FI$deltaDistance/-1)

  # Correct for the fact that delta angles that
  # are >180 are actually smaller angles in the opposite direction;
  # sign of degree doesn't matter because delta is an absolute difference; delta is just a magnitude.
  # We want the smallest way around the circle.
  # Scale deltaAngle 0-180.
  FI$deltaAngle180<-ifelse(FI$deltaAngle>180,
                           360-FI$deltaAngle,
                           FI$deltaAngle)



  # Center the delta Angle so that 0 means a 90 degree difference between tripA & tripB.
  # 90 is very different and -90 is the same
  FI$deltaAngle180.centered<-FI$deltaAngle180-90

  # Rescale to -1 to 1 and flip sign so that +1 means similar trips and -1 is different
  FI$deltaAngle180.centered.wt<-FI$deltaAngle180.centered/-90

  # We multiply by the sign of the deltaAngle180.centered.wt making it so that
  # if a trip goes the same distance in an opposite direction it gets a negative number.
  # sign() returns a +1 or a -1 if a number is positive or negative, respectively.
  # If the max points are greater than 90-degrees apart, the distance gets a negative sign
  # because the points are getting further apart, the longer the deltaDistance is.
  # If the max points are less than 90-degrees apart, the distance gets a positive sign
  # because the points are closer together.
  # So, multiply the deltaDistance by +1 or -1.
  FI$deltaDistance.flip.sign<-FI$deltaDistance.flip*
    sign(FI$deltaAngle180.centered.wt)

  # Create Fidelity Index
  # divide by 2 to re-scale so index ranges -1 (different) to +1 (similar)
  FI$FI<-(FI$deltaDistance.flip.sign+FI$deltaAngle180.centered.wt)/2

  # make a mean for each bird
  FI_mean<-FI %>%
    group_by(animal_id) %>%
    summarise(FI=mean(FI,na.rm=T),
              FI_samplesize=n())

  list(fidelity_index_trips=FI,fidelity_index_animal=FI_mean)
 }
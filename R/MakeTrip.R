#' MakeTrip Function to make a TripNum variable
#'
#' The objective of this function to to partition tracking data into "trips"
#' based on begining and returning to a fixed location.  It was designed for use
#' with GPS tracking data from seabirds that during the breeding season are
#' central place foragers, leaving from colony sites on food finding trips and
#' returning to the colony afterward.  This function relies on a column with a
#' disntance in km from each point along the track to the colony and a distance
#' cutoff.  It numbers each trip starting with the first point beyond the
#' distance cutoff and ending with the last point beyond the cutoff with a
#' sequential number starting with 1.  All points inside the cutoff are labled
#' 0.  In the future it could be nice to have these labeled with sequetial
#' numbers as well.
#'
#' @author Abram B. Fleishman <abram.fleishman AT sjsu.edu>
#' @param tracks data.frame of data that you want to parse into trips
#' @param ID quoted name of column in data that is a unique key to individual
#'   bird_tag_deployment combos. This is the File from the GypsyLocRead output
#' @param DistCutOff Distance in km to use as a cut off radius around the colony
#'   to use to split the trips
#' @param Dist2Colony quoted name of column in data that has the distance in km
#'   from each point to the colony
#' @param NumLocCut Minimum number of points to consider in a trip
#' @return A new data frame with all original data plus two new columns: TripNum
#'   (consecutive trip number 0 = at colony) and ColonyMovement (in/out colony
#'   movements)
#' @export

MakeTrip<-function(tracks,
                   ID="File",
                   DistCutOff=10,
                   Dist2Colony="Dist2Colony",
                   NumLocCut=3){

  Birds<-unique(tracks[[ID]])

  dataOut<-NULL
  for(j in 1:length(Birds)){

    # Subset for each bird
    BirdSub<-tracks[tracks[[ID]]==Birds[j],]
    BirdSub$InColony<-NULL

    # If distance to colony is less than DistCutOff m make it a 0 else make it a 1
    # 0 =  in colony 1 = on trip
    BirdSub$InColony<-ifelse(BirdSub[[Dist2Colony]] < DistCutOff|is.na(BirdSub[[Dist2Colony]]),0,1)

    # offset by one (drop first record) Detect state change for "out" events
    # else NA
    # if the current ooint is in the colony but the next point is out of
    # the colony label it an "out"
    BirdSub$ColonyMovement<-ifelse(BirdSub$InColony == 0 & dplyr::lead(BirdSub$InColony) == 1, "Out", NA)

    # Detect state change for "In" events else "out" or NA
    # if the current ooint is out of the colony but the next point is in
    # the colony label it an "in"
    BirdSub$ColonyMovement<-ifelse(BirdSub$InColony == 1 & dplyr::lead(BirdSub$InColony) == 0, "In", BirdSub$ColonyMovement)

    # Get indicies of out events
    Out<-grep("Out", x = BirdSub$ColonyMovement)

    # If there is an "in" event get the indicies of the in events
    # else make the 2nd to last point an in event
    if ("In" %in% BirdSub$ColonyMovement ) {
      In<-grep("In", x = BirdSub$ColonyMovement)
    } else {
      In<-length(BirdSub$ColonyMovement)-1
    }

    # if the first "in" comes after the first "out" than make first index an "out"
    if(In[1]<Out[1])  Out<-c(1,Out)

    #  if the last out is a larger index than the last in make the last event an "in"
    if(Out[length(Out)]>In[length(In)]) In<-c(In,nrow(BirdSub)-1)

    # add 1 to all the indecies in "in" so that they are the first point at colony
    In<-In+1

    # get the indeces of the ins and outs that have more than NumLocCut points
    sel<-which(abs(In-Out)>NumLocCut)

    #Trims trips that are shorter than a given number of locations, set to 0 to omit
    In<-In[sel]
    Out<-Out[sel]

    # add a vector of NAs to BirdSub that will be populated with trip numbers
    BirdSub$TripNum<-NA

    # add a tripnumber "i" to all the events between the ith out and ith in
    for(i in 1:length(Out)){
      BirdSub$TripNum[Out[i]:In[i]]<-i
    }

    dataOut<-dplyr::bind_rows(dataOut,BirdSub)
  }

  # if not on a trip (within distance to colony threshold) than give a 0
  dataOut$TripNum[is.na(dataOut$TripNum)]<-0

  return(dataOut[ , -which(names(dataOut) =="InColony")])
}

#' Function to make a TripNum variable for Gypsy data that has been run through the GypsyLocRead function
#' iThis is the File from the GypsyLocRead output
#'
#' @author Abram B. Fleishman <abram.fleishman AT sjsu.edu>
#' @param tracks data.frame of data that you want to parse into trips
#' @param ID quoted name of column in data that is a unique key to individual bird_tag_deployment combos. This is the File from the GypsyLocRead output
#' @param DistCutOff Distance in km to use as a cut off radius around the colony to use to split the trips
#' @param Dist2Colony quoted name of column in data that has the distance in km from each point to the colony
#' @return A new data frame with all original data plus two new columns: TripNum (consecutive trip number 0 = at colony) and ColonyMovement (in/out colony movements)
#' @examples
#'
#' MakeTrip(tracks,ID="File",DistCutOff=50,Dist2Colony="Dist2Colony")
#' @export
#'

MakeTrip<-function(tracks,ID="File",DistCutOff=10,Dist2Colony="Dist2Colony"){
  Birds<-unique(tracks[[ID]])

  require("dplyr")

  dataOut<-NULL
  for(j in 1:length(Birds)){
    # Subset for each bird
    BirdSub<-tracks[tracks[[ID]]==Birds[j],]

    # If distance to colony is less than DistCutOff m make it a 0 else make it a 1
    BirdSub$InColony<-ifelse(BirdSub[[Dist2Colony]]<DistCutOff,0,1)

    # offset by one (drop first record)
    # Detect state change for "out" events else NA
    BirdSub$ColonyMovement<-ifelse(BirdSub$InColony==0&lead(BirdSub$InColony)==1,"Out",NA)

    # Detect state change for "In" events else "out" or NA
    BirdSub$ColonyMovement<-ifelse(BirdSub$InColony==1&lead(BirdSub$InColony)==0,"In",BirdSub$ColonyMovement)

    # Get indicies of out events
    Out<-grep("Out",x = BirdSub$ColonyMovement)

    # If there is an "in" event get the indicies of the in events
    if("In" %in% BirdSub$ColonyMovement){
      In<-grep("In",x = BirdSub$ColonyMovement)
      } else {
        In<-length(BirdSub$ColonyMovement)-1
      }

    # if the first "in" comes after the first "out" than make first index an out
    if(In[1]<Out[1])  Out<-c(1,Out)
    #  if the last out is a larger index than the last in make the last event an in
    if(Out[length(Out)]>In[length(In)]) In<-c(In,length(BirdSub$Date)-1)

    # add 1 to all the indecies in "in" to make the event the first point inside the circle
    In<-In+1

    # add a vector of NAs to BirdSub that will be populated with trip numbers
    BirdSub$TripNum<-NA

    # add a tripnumber "i" to all the events between the ith out and ith in
    for(i in 1:length(Out)){
      BirdSub$TripNum[Out[i]:In[i]]<-i
    }
    dataOut<-bind_rows(dataOut,BirdSub)
  }

  # if not on a trip (within distance to colony threshold) than give a 0
  dataOut$TripNum[is.na(dataOut$TripNum)]<-0

  return(dplyr::select(dataOut,-InColony))
}

#' Number events (such as foraging events) by bird and trip
#'
#' Sometimes it is useful to number events consecutively such as forageing
#' events for a seabird.  often times these events have multiple rows per
#' event/bout.  This function numbers these evens consecutively by bird and by
#' trip nested within bird.
#'
#' @author Abram B. Fleishman \email{abram@@conservationmetrics.com}
#' @author Rachael Orben \email{Rachael.Orben@@oregonstate.edu}
#' @param dat data.frame of with a minimum of three columns with birdID, tripID
#'   from the MakeTrip function and a event column
#' @param BirdID a unique ID for each bird
#' @param TripID a unique ID for each trip nested within each bird. I.e. birdA
#'   has trips 1,2,3; BirdB has trips 1,2,3,4
#' @param eventCol a column with values indicating the presence of an event.
#'   Must be in the format 0= no event, 1= event
#' @return A new data frame with all original data plus two new columns:
#'   event_bird with numbered events 1:n_vents by bird, and event_trip with
#'   numbered events 1:n_events per trip
#'   @importFrom dplyr bind_rows
#' @export

label_events<-function(dat,
                       BirdID="CaptureID",
                       TripID="TripID",
                       eventCol="forage"){

  Birds<-unique(dat[[BirdID]])
  dat$event_trip<-NA
  dat$event_bird<-NA
  data_out<-NULL
  for(i in 1:length(Birds)){
    dat_temp<-dat[dat[[BirdID]]==Birds[i],]
    dat_temp_rle<-rle(dat_temp[[eventCol]])

    cumsum_length<-cumsum(dat_temp_rle$lengths)
    cumsum_length0<-cumsum_length[dat_temp_rle$values==0]
    endIdx<-cumsum_length[dat_temp_rle$values==1]

    startIdx<-c(1,(cumsum_length0+1))[1:(length(cumsum_length0))]
    if(dat_temp[[eventCol]][nrow(dat_temp)]==1|dat_temp[[eventCol]][1]==0){
      endIdx<-c(endIdx,max(cumsum_length0))
    }
    for(k in 1:length(startIdx)){
      dat_temp$event_bird[startIdx[k]:endIdx[k]]<-k
    }
    trips<-unique(dat_temp[[TripID]])

    for(j in 1:length(trips)){
      dat_temp_trip<-dat_temp[dat_temp[[TripID]]==trips[j],]
      dat_temp_rle_trip<-rle(dat_temp_trip[[eventCol]])

      cumsum_length_trip<-cumsum(dat_temp_rle_trip$lengths)
      cumsum_length0_trip<-cumsum_length_trip[dat_temp_rle_trip$values==0]
      endIdx_trip<-cumsum_length_trip[dat_temp_rle_trip$values==1]
      dat_temp_trip[[eventCol]][1]
      if(dat_temp_trip[[eventCol]][nrow(dat_temp_trip)]==1|dat_temp_trip[[eventCol]][1]==0){
        endIdx_trip<-c(endIdx_trip,max(cumsum_length0_trip))
      }

      startIdx_trip<-c(1,(cumsum_length0_trip+1))[1:(length(cumsum_length0_trip))]
      for(kk in 1:length(startIdx_trip)){

        dat_temp_trip$event_trip[startIdx_trip[kk]:endIdx_trip[kk]]<-kk
      }
      data_out<-bind_rows(data_out,dat_temp_trip)
    }
  }
  return(data_out)
}

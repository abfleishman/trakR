#' Inbound/Outbound flight leg identification
#'
#' This function's intention is to partition a track into an inbound and outbound leg demarcated when the bird first stops moving away from the colony. The first stopping point is identified by taking the change in dist2col first derivative, smoothing it with a left aligned rolling mean, and rounding to 1 digit. The first point where the bird is not changing the distance to the colony is the end of the outbound leg, while the last point where the birds is not getting close to the colony is the inbound leg.
#'
# METHODS: Trip departure and return segments are identified by calculating delta-d
# , the change in distance from the colony, lagged by lag gps points, and smoothed
# by calculating a 10 point left aligned rolling mean. These values were rounded
# to 1 significant digit. Trips were segmented by assigning points between the
# beginning of a trip and the first point delta-d which equaled zero as the “out” leg
# and the last point delta-d that equaled zero until the final point of the trip as
# the “in” leg. The island was buffered so that the first and last points delta-d
# could not be within 10 km of the island. If there was no point on the trip
# that delta-d reached zero, i.e. the bird continuously moved away from the colony
# until it began to return, the trip was split at the apex point where the bird
# was furthest from the colony. This method effectively finds the first and last
# points that the bird ceases to move away from the colony over a 15-minute
# period.
#' @author Abram B. Fleishman \email{abram@@conservationmetrics.com}
#' @param tracks dataframe with tracking data sorted by ID, tripID, datetime
#' @param CaptureID column name holding animal ID
#' @param TripID column name holding Trip ID
#' @param DateTime column name holding the datetime
#' @param dist2colony column name holding distances to the colony of each point
#' @param lag is the number of points to skip to caculate the change in distance. e.g. if lag=3, the diff between i and i-3 will be calculated
#' @param nPointsToSmooth number of points to smooth to determine legs
#' @param minDist2Col buffer in km around colony that will removed to avoid false out/in leg identification
#' @param Plot True/False whether to plot each trip (good for figuring out the right parameters)
#' @param Lon column name holding Longitude (x-coord) (only needed if Plot==T)
#' @param Lat column name holding Latitude (y coord) (only needed if Plot==T)
#' @param pdfName fullpath to pdf that will be saved (only needed if Plot==T)
#' @importFrom graphics  plot lines
#' @importFrom grDevices pdf dev.off
#' @importFrom dplyr lag arrange_
#' @importFrom magrittr %>%
#' @importFrom zoo rollmean
#' @export
InOutPoints<-function(tracks=tracks,CaptureID="CaptureID",TripID="TripID",
                      DateTime = "datetime",
                      dist2colony="dist2colony",lag=1,nPointsToSmooth=10,
                      minDist2Col=5,Plot=F,Lon="lon",Lat="lat",pdfName="inout_plots.pdf"){
  # Create the output vector
  inOut<-character(length = nrow(tracks))
  tracks$ddist2colonysmooth<-NA
  if(Plot==T) pdf(pdfName)
  # Loop through birds
  for(i in unique(tracks[[CaptureID]])) {
    tracksID<-tracks[tracks[[CaptureID]]==i,]

    # Loop Through trips
    for(j in unique(tracksID[[TripID]])){
      trackssub<-tracksID[tracksID[[TripID]]==j,] %>% arrange(DateTime)
      trxIdxs<-which(tracks[[CaptureID]]==i&tracks[[TripID]]==j)
      if(nrow(trackssub)<=lag) {
        warning(paste("TripID",unique(tracksID[[TripID]])[j],"for CaptureID",
                      unique(tracks[[CaptureID]])[i])," skipped because it has less rows than your lag.")
        next() #skip if empty
      }

      # Create temp output vector
      inOutTemp<-rep(NA,length.out=nrow(trackssub))

      # Calculate the change (delta) in distance to the colony with a user defined lag
      trackssub$ddist2colony<-trackssub[[dist2colony]]-lag(trackssub[[dist2colony]],n = lag)

      # Calculate a rolling mean for the delta dist2colony and round to 1 digit
      trackssub$ddist2colonysmooth[(1+lag):nrow(trackssub)]<-floor(round(rollmean(trackssub$ddist2colony[(1+lag):nrow(trackssub)],
                                                                                  k=nPointsToSmooth,fill = NA,align = "left"),1))

      zeroIdxs<-which(trackssub$ddist2colonysmooth==0&trackssub[[dist2colony]]>=minDist2Col)
      apexPoint<-which(trackssub[[dist2colony]]==max(trackssub[[dist2colony]],na.rm=T))

      if(length(zeroIdxs)==0|suppressWarnings(min(zeroIdxs))>apexPoint) zeroIdxs<-apexPoint
      outIdx<-1:min(zeroIdxs[zeroIdxs<=apexPoint])

      if(length(zeroIdxs[zeroIdxs>=apexPoint])==0|max(zeroIdxs)<apexPoint) zeroIdxs<-apexPoint
      inIdx<-max(zeroIdxs[zeroIdxs>=apexPoint]):nrow(trackssub)

      inOutTemp[outIdx]<-"out"
      inOutTemp[inIdx]<-"in"
      inOutTemp[is.na(inOutTemp)]<-"mid"
      inOut[trxIdxs]<-inOutTemp
      if(Plot==T){ plot(trackssub[[Lon]],trackssub[[Lat]],col=factor(inOutTemp),main=paste("i: ",i," J:",j))
      lines(trackssub[[Lon]],trackssub[[Lat]])}
      # plot(trackssub$dist2colony,main=paste("i: ",i," J:",j))
    }
  }
  if(Plot==T) dev.off()
  return(inOut)
}

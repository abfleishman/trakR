#' Calculate Bhattacharyya's affinity for two groups of animal tracks
#'
#' This function calculated  Bhattacharyya's affinity for two groups of animal
#' tracks.  This could be Two sexes, high/low contaiminants loads, high low
#' stress. You must have a tripID column that is unique to the trip, not just
#' trip within bird.  This can be created by paste(birdID, tripID,sep="_) if you
#' do not already have this column.
#'
#' @author Rachael Orben \email{Rachael.Orben@@oregonstate.edu}
#' @author Abram B. Fleishman \email{abram@@conservationmetrics.com}
#'
#' @param tracks a data frame with tracking data
#' @param  tripid quoted column name housing unique trip ID. This can be created
#'   by paste(birdID, tripID,sep="_) if it does not already exist.
#'   trakR::MakeTrip can partition trips if not already done
#' @param groupid quoted column name housing latitude
#' @param lon quoted column name housing latitude
#' @param lat quoted column name housing latitude
#' @param colonyLon central longitude of the colony or center of the tracking
#'   data for reprojecting that lon/lat data into Lambert Azmuthel Equal Area in
#'   km
#' @param colonyLat central latitude of the colony or center of the tracking
#'   data for reprojecting that lon/lat data into Lambert Azmuthel Equal Area in
#'   km
#' @param its number of iterations
#' @param h the smoothing parameter for kernelUD
#' @param ud.grid a "SpatialPixelsDataFrame"  generated with an extent and cell
#'   size with the function adehabitatMA::ascgen()
#' @param Plot TRUE/FALSE if each randomization should be plotted?
#'
#' @importFrom sp SpatialPointsDataFrame CRS spTransform
#' @importFrom adehabitatHR kernelUD getverticeshr kerneloverlaphr
#' @importFrom ggplot2 fortify ggplot geom_polygon aes facet_wrap labs
#' @importFrom dplyr bind_rows mutate
#' @importFrom utils head
#' @importFrom stats sd
#' @export
kernalOverlapBA_p<-function (tracks, tripid="tripID", groupid = "Sex",
                       lon="Longitude",lat="Latitude",
                       colonyLon=-113.244555,colonyLat=31.015513,
                       its=1000, h=1.4809524, ud.grid,Plot=F){

  # get unique trips
  UniTripID<-unique(tracks[[tripid]]) #unique trips

  # get the unique levels of the group var
  GroupID_levels<-unique(tracks[[groupid]])

  # get n trips for each group and the total n trips
  GroupA_length<-length(unique(tracks[[ tripid ]][ tracks[[groupid]]==GroupID_levels[1] ]))
  GroupB_length<-length(unique(tracks[[tripid]][ tracks[[groupid]]==GroupID_levels[2] ]))
  n_UniTripID<-GroupA_length+GroupB_length

  # make a trip and grou ID colomn for later use
  tracks$groupid<-tracks[[groupid]]
  tracks$trip.id<-tracks[[tripid]]

  # Warn and remove NAs
  if(nrow(tracks[is.na(tracks[[lat]]),])>0){
    lenny<-nrow(tracks)
    tracks<- tracks[!is.na(tracks[[lat]]),]
    warning(paste("There are",lenny-nrow(tracks)," NAs in  your position column. Removing!"))
  }

  # Calculate BA for the real groups ------------------

  # Make a SPDF
  tracks.spdf <- SpatialPointsDataFrame(coords=cbind(tracks[[lon]],
                                                     tracks[[lat]]),
                                        data=data.frame(id=tracks$groupid),
                                        proj4string = CRS("+proj=longlat +ellps=WGS84
                                                             +datum=WGS84 +no_defs"))
  # Project data into laea
  tracks.spdf.t <- spTransform(tracks.spdf,
                               CRS(paste("+proj=laea +units=km +lon_0=",
                                         colonyLon, " +lat_0=", colonyLat	, sep="")))


  # calculate kernelUD
  ud <- kernelUD(tracks.spdf.t, h = h,grid=ud.grid)

  # Plot the actual overlap
    uds<-suppressWarnings(bind_rows(
      mutate(suppressWarnings(fortify(getverticeshr(ud, percent=95, standardize=T))), ud="95"),
      mutate(suppressWarnings(fortify(getverticeshr(ud, percent=50, standardize=T))), ud="50")))
    print(ggplot(data=uds)+
            geom_polygon(aes(x=long,y=lat,group=group,fill=id),alpha=.5)+
            facet_wrap(~ud)+
            labs(fill=groupid))


  # 50% overlap
  BA_o50<-kerneloverlaphr(ud , method="BA", percent=50, conditional=TRUE)
  BA_o50<-BA_o50[1,2]

  # 95% overlap
  BA_o95<-kerneloverlaphr(ud , method="BA", percent=95, conditional=TRUE)
  BA_o95<-BA_o95[1,2]

  # Calculate BA for the Randomized groups ------------------

  # create data out structures
  RandomIndices_50<-numeric(length = its)
  RandomIndices_95<-numeric(length = its)

  for (i in 1:its){

    print(paste("iteration:" ,i,"of",its))

    # Shuffle the order of the UniTripIDs
    Idx<-sample(UniTripID, n_UniTripID, replace = FALSE, prob = NULL)

    # take the 1st x UniTripIDs where x is the sample size for group A
    UniTripID_A<-Idx[1:GroupA_length]

    # take the last x UniTripIDs where x is the sample size for group b. this
    # assumes that all UniTripID are classified into a group
    UniTripID_B<-Idx[(GroupA_length+1):GroupB_length]

    # add the random grouping to the data
    tracks$groupRan<-NA
    tracks$groupRan[tracks$trip.id%in%UniTripID_A]<-"A"
    tracks$groupRan[tracks$trip.id%in%UniTripID_B]<-"B"

    # Make SPDF with the random grouping as the ID column
    tracks.spdf <- SpatialPointsDataFrame(coords=cbind(tracks[[lon]],tracks[[lat]]),
                                                 data=data.frame(id=tracks$groupRan),
                                                 proj4string = CRS("+proj=longlat +ellps=WGS84
                                                            +datum=WGS84 +no_defs"))
    # Project data into laea
    tracks.spdf.t <- spTransform(tracks.spdf,
                                        CRS(paste("+proj=laea +units=km +lon_0=", colonyLon,
                                                  " +lat_0=", colonyLat	, sep="")))

    # make the kernelUD
    ud1 <- kernelUD(tracks.spdf.t, h = h,grid=ud.grid)

    # Plot each if desired (only for diagnostics?)
    if(Plot==T){
      uds<-bind_rows(
        mutate(fortify(getverticeshr(ud1, percent=95, standardize=T)), ud="95"),
        mutate(fortify(getverticeshr(ud1, percent=50, standardize=T)), ud="50"))
      head(uds)
      print(ggplot(data=uds)+geom_polygon(aes(x=long,y=lat,group=group,fill=id),alpha=.5)+facet_wrap(~ud))
    }

    # Calculate BA
    BA_50<-kerneloverlaphr(ud1 , method="BA", percent=50, conditional=TRUE)
    BA_95<-kerneloverlaphr(ud1 , method="BA", percent=95, conditional=TRUE)

    # add each iterations result
    RandomIndices_50[i]<-BA_50[1,2]
    RandomIndices_95[i]<-BA_95[1,2]
  }

  # Calculate the P
  pval_50<-length(RandomIndices_50[RandomIndices_50<BA_o50])/its
  pval_95<-length(RandomIndices_95[RandomIndices_95<BA_o95])/its

  # make a resutls table
  results<-rbind(data.frame(ud=50,p=pval_50,BA=BA_o50,BA_rand_mean=mean(RandomIndices_50),
                            BA_rand_min=min(RandomIndices_50),BA_rand_max=max(RandomIndices_50),BA_rand_sd=sd(RandomIndices_50)),
                 data.frame(ud=95,p=pval_95,BA=BA_o95,BA_rand_mean=mean(RandomIndices_95),
                            BA_rand_min=min(RandomIndices_95),BA_rand_max=max(RandomIndices_95),BA_rand_sd=sd(RandomIndices_95)))

  return(results)

}

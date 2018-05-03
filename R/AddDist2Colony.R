#' AddDist2Colony Calculate
#'
#' @author Abram B. Fleishman <abram.fleishman AT sjsu.edu>

#' @param tracks a data frame with with latitude and longitude and colony site names
#' @param CaptureSitesData data with colony site names and colony lat long
#' @param dataLon quoted name of column in data that has longitude values
#' @param dataLat quoted name of column in data that has latitude values
#' @param SiteName a quoted string indicating what the column houses your SiteNames.  Must be the same across both your data and the CaptureSitesData
#' @param dataset a commented string indicating what dataset you are using (currently only works with a single variable)
#' @return vector of distances from each colony
#' @examples
#' AddDist2Colony(data=data,CaptureSitesData=CapSitesSel,SiteName="SiteShort")
#' @export

AddDist2Colony<-function(tracks=tracks,CaptureSitesData=CapSitesSel,SiteName="SiteShort"){
  dataOut<-vector(mode = "numeric",length = nrow(tracks))
  Sites<-as.character(unique(tracks[[SiteName]]))

  for(j in 1:length(Sites)){

    CapSub<-CapSitesSel[CapSitesSel[SiteName]==Sites[j],]
    dataSub<-tracks[tracks[SiteName]==Sites[j],]
    distanceVector<-Dist2Colony(tracks = dataSub,ColonyLat = CapSub$Lat,ColonyLong = CapSub$Lon)
    dataOut[tracks[SiteName]==Sites[j]]<- distanceVector
  }
  return(dataOut)
}

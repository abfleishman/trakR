#' AddDist2Colony Calculate the Distance from Points to Many Colonies
#'
#' AddDist2Colony takes tracks with lat/lon positions and and
#'
#' @author Abram B. Fleishman \email{abram@@conservationmetrics.com}
#'
#' @param tracks a data frame with with latitude and longitude and colony site names
#' @param CaptureSitesData data.frame with colony site names and colony lat long
#' @param dataLon quoted name of column in data that has longitude values
#' @param dataLat quoted name of column in data that has latitude values
#' @param capLat quoted name of column in CaptureSitesData that has latitude values
#' @param capLon quoted name of column in CaptureSitesData that has longitude values
#' @param SiteName a quoted string indicating what the column houses your SiteNames.
#'   Must be the same across both your data and the CaptureSitesData
#'
#' @return vector of distances from each colony
#'
#' @export

AddDist2Colony<-function(tracks,
                         dataLat="lat",
                         dataLon="lon",
                         CaptureSitesData,
                         SiteName="SiteShort",
                         capLat="lat",
                         capLon="lon"){

  dataOut<-vector(mode = "numeric",length = nrow(tracks))
  Sites<-as.character(unique(tracks[[SiteName]]))

  for(j in 1:length(Sites)){

    CapSub<-CaptureSitesData[CaptureSitesData[SiteName] == Sites[j],]
    dataSub<-tracks[tracks[SiteName] == Sites[j],]
    dataOut[tracks[SiteName]==Sites[j]] <- Dist2Colony(tracks = dataSub,
                                                       dataLat=dataLat,
                                                       dataLon=dataLon,
                                                       ColonyLat = CapSub[[capLat]],
                                                       ColonyLong = CapSub[[capLon]])
  }
  return(dataOut)
}

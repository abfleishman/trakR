#' DepthETOPO Compute the depth from Etopo1 is at 1 arc minute resolution.
#' It would be nice to find something higher resolution
#' @author Abram B. Fleishman <abram.fleishman AT sjsu.edu>
#' @param tracks data.frame of data to be queried for  latand long values
#' @param dataLon quoted name of column in data that has longitude values
#' @param dataLat quoted name of column in data that has latitude values
#' @param left The left side of the bounding box of data
#' @param right The right side of the bounding box of data
#' @param top The top side of the bounding box of data
#' @param bottom The bottom side of the bounding box of data
#' @return A vector of elevation/depth in meters from ETOPO1 world bathymatry at a resolution of 1 arc minute
#' @examples
#' DepthETOPO(tracks=tracks,dataLat="Latitude", dataLon="Longitude",left=-179,right= -164,top=60,bottom=50)
#' @export


# Etopo1 is at 1 arc minute resolution.  It would be nice to find something higher resolution
# load track data
# tracks<-readRDS(data,file = "processedDATA/GPSrlki_raw.rda")

DepthETOPO<-function(tracks=tracks,dataLat="Latitude", dataLon="Longitude",left=-179,right= -164,top=60,bottom=50){

  # Get bathymetric data ETOPO2
  dat <- getNOAA.bathy(lon1 = right ,lon2 = left ,lat1 = bottom,lat2 = top ,resolution = 1, keep=F)

  SPData<-as.SpatialGridDataFrame(dat)

  depth<-extract.data(cbind(tracks[[dataLon]],tracks[[dataLat]]),asc.from.sp(SPData))
  return(depth)
}

# tracks$depth<-DepthETOPO(data=tracks,dataLat="Latitude", dataLon="Longitude",left=-179,right= -164,top=60,bottom=50)
# head(as.data.frame(tracks),100)

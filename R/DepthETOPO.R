#' DepthETOPO Compute the depth from Etopo1 is at 1 arc minute resolution.
#'
#' It would be nice to find something higher resolution
#'
#' @author Abram B. Fleishman \email{abram@@conservationmetrics.com}
#'
#' @param tracks data.frame of data to be queried for  latand long values
#' @param dataLon quoted name of column in data that has longitude values
#' @param dataLat quoted name of column in data that has latitude values
#' @param left The left side of the bounding box of data
#' @param right The right side of the bounding box of data
#' @param top The top side of the bounding box of data
#' @param bottom The bottom side of the bounding box of data
#' @param resolution arc minutes res
#' @param keep save a file
#'
#' @return A vector of elevation/depth in meters from ETOPO1 world bathymatry at a resolution of 1 arc minute
#'
#' @export
#' @importFrom marmap getNOAA.bathy as.SpatialGridDataFrame
#' @importFrom SDMTools extract.data asc.from.sp

DepthETOPO<-function(tracks,
                     dataLat="Latitude",
                     dataLon="Longitude",
                     left=-179,right= -164,top=60,bottom=50,resolution = 30, keep=F){

  # Get bathymetric data ETOPO2
  dat <- getNOAA.bathy(lon1 = right ,lon2 = left ,
                               lat1 = bottom,lat2 = top ,
                               resolution = resolution, keep=keep)

  SPData<-as.SpatialGridDataFrame(dat)

  depth<-extract.data(cbind(tracks[[dataLon]],
                                      tracks[[dataLat]]),
                                asc.from.sp(SPData))
  return(depth)
}


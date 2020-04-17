#' DepthETOPO Compute the depth from ETOPO1 is at 1 arc minute resolution.
#'
#' Extract sea floor depth at each point along a track.  Relies on `marmap::getNOAA.bathy` which queries the ETOPO1 database hosted on the NOAA website, given the coordinates of the area of interest and desired resolution. Then extracts the values at each point along the track.
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
#' needs to add :importFrom SDMTools extract.data asc.from.sp
#'
#' @export
#' @importFrom marmap getNOAA.bathy as.SpatialGridDataFrame
#'
#' @references Amante, C. and B. W. Eakins, ETOPO1 1 Arc-Minute Global Relief Model: Procedures, Data Sources and Analysis. NOAA Technical Memorandum NESDIS NGDC-24, 19 pp, March 2009. http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO1/docs/ETOPO1.pdf



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


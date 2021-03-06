% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kernalOverlapBA_p.R
\name{kernalOverlapBA_p}
\alias{kernalOverlapBA_p}
\title{Calculate Bhattacharyya's affinity for two groups of animal tracks}
\usage{
kernalOverlapBA_p(
  tracks,
  tripid = "tripID",
  groupid = "Sex",
  lon = "Longitude",
  lat = "Latitude",
  colonyLon = -113.244555,
  colonyLat = 31.015513,
  its = 10,
  h = 1.4809524,
  ud.grid,
  Plot = F
)
}
\arguments{
\item{tracks}{a data frame with tracking data}

\item{tripid}{quoted column name housing unique trip ID. This can be created
by paste(birdID, tripID,sep="_) if it does not already exist.
trakR::MakeTrip can partition trips if not already done}

\item{groupid}{quoted column name housing latitude}

\item{lon}{quoted column name housing latitude}

\item{lat}{quoted column name housing latitude}

\item{colonyLon}{central longitude of the colony or center of the tracking
data for reprojecting that lon/lat data into Lambert Azmuthel Equal Area in
km}

\item{colonyLat}{central latitude of the colony or center of the tracking
data for reprojecting that lon/lat data into Lambert Azmuthel Equal Area in
km}

\item{its}{number of iterations}

\item{h}{the smoothing parameter for kernelUD}

\item{ud.grid}{a "SpatialPixelsDataFrame"  generated with an extent and cell
size with the function adehabitatMA::ascgen()}

\item{Plot}{TRUE/FALSE if each randomization should be plotted?}
}
\description{
This function calculated  Bhattacharyya's affinity for two groups of animal
tracks.  This could be Two sexes, high/low contaiminants loads, high low
stress. You must have a tripID column that is unique to the trip, not just
trip within bird.  This can be created by paste(birdID, tripID,sep="_) if you
do not already have this column.
}
\author{
Rachael Orben \email{Rachael.Orben@oregonstate.edu}

Abram B. Fleishman \email{abram@conservationmetrics.com}
}

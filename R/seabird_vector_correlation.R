#' Calculate vector correlation coefficient between two-dimensional vectors
#'
#' This function calculates the vector correlation coefficient between sequences
#' of two-dimensional vectors. It is based on calculations in Jupp and Mardia
#' (1980). See also: Stephens (1979) and Jupp (1987).
#'
#' These vectors must be specified in terms of their u (X1, X2) and v (Y1, Y2)
#' components. For two-dimensional vectors, the vector correlation coefficient
#' (squared), R2, will vary between 0.0 (uncorrelated) and 2.0 (completely
#' correlated).
#'
#' This function was intended for use with seabird tracking data. The u- and
#' v-vectors from a bird's trajectory can be obtained by decomposing the bird's
#' speed and heading values. One package/function that decomposes these values
#' is rWind::ds2uv().
#'
#' Vector correlation coefficients calculated according to the following
#' function should be accurate to at least three decimal places using single
#' precision arithmetic.
#'
#' "NS" is the sample size. Sample sizes less than 8 are not recommended because
#' the asymptotic chi-square distribution breaks down for smaller values.
#'
#' This code was converted from use in Fortran 77 to use in MatLab by Stephanie
#' Flora on 16Nov2004 and was converted for use in R by Morgan Gilmour on
#' 06Sept2022.
#'
#' @param X1 X data set 1 (u) The data containing the u-vector of the bird's
#'   trajectory. Can be an indexed column or vector object.
#' @param Y1 Y data set 1 (v) The data containing the v-vector of the bird's
#'   trajectory. Can be an indexed column or vector object.
#' @param X2 X data set 2 (u) The data containing the u-vector of the wind's
#'   trajectory. Can be an indexed column or vector object.
#' @param Y2 Y data set 2 (v) The data containing the v-vector of the wind's
#'   trajectory. Can be an indexed column or vector object.
#'
#' @return Numeric. Vector correlation coefficient, which will range between 0.0
#'   (uncorrelated) and 2.0 (completely correlated).
#' @export
#' @references
#' Stephens, M.A. 1979. Vector Correlation. Biometrika, 66(1):41-48.
#' Jupp, P.E. 1987. A nonparametric correlation coefficient and a two-sample
#' test for random vectors or directions Biometrika, 74:887-890.
#' Jupp, P.E. and Mardia, K.V. 1980.A general correlation coefficient for
#' directional data and related regression problems. Biometrika, 67:163-173.
#'
#' @examples
#'
#' \donttest{
#' NS = 100 #Number of samples (i.e., length(X1))
#' X1 = runif(NS)
#' Y1 = runif(NS)
#' X2 = X1
#' Y2 = Y1
#' seabird_vector_correlation(X1,Y1,X2,Y2)
#' # R2 = 2 because the both data set are identical and therefore perfectly correlated
#'}
#' \donttest{
#' NS = 100 #Number of samples (i.e., length(X1))
#' X1 = runif(NS)
#' Y1 = runif(NS)
#' X2 = runif(NS)
#' Y2 = runif(NS)
#' seabird_vector_correlation(X1,Y1,X2,Y2)
#' # R2 = 0.08 (or similar small value, based on random number generations)
#' # because both data sets are random and therefore VERY uncorrelated.
#'}
#'
seabird_vector_correlation <- function(X1,Y1,X2,Y2){
  NS<-length(X1)
  if(all(length(X1)!=c(length(X2),length(Y1),length(Y2)))){
    stop("X1, X2, Y1, and Y2 need to be the same length")
  }
  if(length(X1)<8){
    warning("Number of samples should be 8 or greater")
  }
  # Calculate covariance for X1 and Y1
  temp.stat1<-STAT1(X1,Y1,X2,Y2,NS)
  VAR1<-temp.stat1[1]
  VAR2<-temp.stat1[2]
  COV12<-temp.stat1[3]

  # Calculate covariance for X1 and X2
  temp.stat2<-STAT2(X1,Y1,X2,Y2,NS)
  VAR3<-temp.stat2[2]
  COV13<-temp.stat2[3]

  # Calculate covariance for X1 and Y2
  temp.stat3<-STAT3(X1,Y1,X2,Y2,NS)
  VAR4<-temp.stat3[2]
  COV14<-temp.stat3[3]

  # Calculate covariance for Y1 and X2
  temp.stat4<-STAT4(X1,Y1,X2,Y2,NS)
  COV23<-temp.stat4[3]

  # Calculate covariance for Y1 and Y2
  temp.stat5<-STAT5(X1,Y1,X2,Y2,NS)
  COV24<-temp.stat5[3]

  # Calculate covariance for X2 and Y2
  temp.stat6<-STAT6(X1,Y1,X2,Y2,NS)
  COV34<-temp.stat6[3]

  # Use covariance values to calculate correlation coefficient, R2.
  R2 <- RSQ(VAR1,VAR2,VAR3,VAR4,COV12,COV13,COV14,COV23,COV24,COV34)

  return(R2)
}


#-----------------------------------------------------------------------------------
#' Calculate covariance for X1 and Y1
#'
#' @param X1 X data set 1 (u)
#' @param Y1 Y data set 1 (v)
#' @param X2 X data set 2 (u)
#' @param Y2 Y data set 2 (v)
#' @param NS # Defined in seabird_vector_correlation() via length(X1)
#'
#' @return Three-column matrix containing the variances of X1 and Y1 and the
#'   covariance of X1 and Y1.
#'
STAT1 <- function(X1,Y1,X2,Y2,NS){

  SUMX1 <- 0.0 # Initialize vectors that will be filled with for-loop
  SUMY1 <- 0.0
  SX1SQ <- 0.0
  SY1SQ <- 0.0
  SX1Y1 <- 0.0

  # Cumulatively add/square u and v vector values by going through each paired
  # row, i, of the X1 and Y1 dataframe/vector
  for ( i in 1:NS){
    SUMX1 <- SUMX1 + X1[i]
    SUMY1 <- SUMY1 + Y1[i]
    SX1SQ <- SX1SQ + X1[i]*X1[i]
    SY1SQ <- SY1SQ + Y1[i]*Y1[i]
    SX1Y1 <- SX1Y1 + X1[i]*Y1[i]
  }

  # Calculate the sample variances
  VAR1 <- (NS * SX1SQ - SUMX1 * SUMX1) / (NS * (NS - 1))
  VAR2 <- (NS * SY1SQ - SUMY1 * SUMY1) / (NS * (NS - 1))
  COV12 <- (NS * SX1Y1 - SUMX1 * SUMY1) / (NS * (NS - 1))
  return(cbind(VAR1,VAR2,COV12))
}
#-----------------------------------------------------------------------------------
#' Calculate covariance for X1 and X2
#'
#' @param X1 X data set 1 (u)
#' @param Y1 Y data set 1 (v)
#' @param X2 X data set 2 (u)
#' @param Y2 Y data set 2 (v)
#' @param NS # Defined in seabird_vector_correlation() via length(X1)
#' @noRd
#' @return Three-column matrix containing the variances of X1 and X2 and the
#'   covariance of X1 and X2.
#'
STAT2 <- function(X1,Y1,X2,Y2,NS){

  SUMX1 <- 0.0
  SUMX2 <- 0.0
  SX1SQ <- 0.0
  SX2SQ <- 0.0
  SX1X2 <- 0.0
  for ( i in 1:NS){
    SUMX1 <- SUMX1 + X1[i]
    SUMX2 <- SUMX2 + X2[i]
    SX1SQ <- SX1SQ + X1[i]*X1[i]
    SX2SQ <- SX2SQ + X2[i]*X2[i]
    SX1X2 <- SX1X2 + X1[i]*X2[i]
  }

  VAR1 <- (NS * SX1SQ - SUMX1 * SUMX1) / (NS * (NS - 1))
  VAR3 <- (NS * SX2SQ - SUMX2 * SUMX2) / (NS * (NS - 1))
  COV13 <- (NS * SX1X2 - SUMX1 * SUMX2) / (NS * (NS - 1))
  return(cbind(VAR1,VAR3,COV13))
}
#-----------------------------------------------------------------------------------
#' Calculate covariance for X1 and Y2
#'
#' @param X1 X data set 1 (u)
#' @param Y1 Y data set 1 (v)
#' @param X2 X data set 2 (u)
#' @param Y2 Y data set 2 (v)
#' @param NS # Defined in seabird_vector_correlation() via length(X1)
#'
#' @return Three-column matrix containing the variances of X1 and Y2 and the
#'   covariance of X1 and Y2.
#'
STAT3 <- function(X1,Y1,X2,Y2,NS){

  SUMX1 <- 0.0
  SUMY2 <- 0.0
  SX1SQ <- 0.0
  SY2SQ <- 0.0
  SX1Y2 <- 0.0
  for (i in 1:NS){
    SUMX1 <- SUMX1 + X1[i]
    SUMY2 <- SUMY2 + Y2[i]
    SX1SQ <- SX1SQ + X1[i]*X1[i]
    SY2SQ <- SY2SQ + Y2[i]*Y2[i]
    SX1Y2 <- SX1Y2 + X1[i]*Y2[i]
  }
  VAR1 <- (NS * SX1SQ - SUMX1 * SUMX1) / (NS * (NS - 1))
  VAR4 <- (NS * SY2SQ - SUMY2 * SUMY2) / (NS * (NS - 1))
  COV14 <- (NS * SX1Y2 - SUMX1 * SUMY2) / (NS * (NS - 1))
  return(cbind(VAR1,VAR4,COV14))
}
#-----------------------------------------------------------------------------------
#' Calculate covariance for Y1 and X2
#'
#' @param X1 X data set 1 (u)
#' @param Y1 Y data set 1 (v)
#' @param X2 X data set 2 (u)
#' @param Y2 Y data set 2 (v)
#' @param NS # Defined in seabird_vector_correlation() via length(X1)
#'
#' @return Three-column matrix containing the variances of Y1 and X2 and the
#'   covariance of Y1 and X2.
#'
STAT4 <- function(X1,Y1,X2,Y2,NS){

  SUMY1 <- 0.0
  SUMX2 <- 0.0
  SY1SQ <- 0.0
  SX2SQ <- 0.0
  SY1X2 <- 0.0
  for (i in 1:NS){
    SUMY1 <- SUMY1 + Y1[i]
    SUMX2 <- SUMX2 + X2[i]
    SY1SQ <- SY1SQ + Y1[i]*Y1[i]
    SX2SQ <- SX2SQ + X2[i]*X2[i]
    SY1X2 <- SY1X2 + Y1[i]*X2[i]
  }
  VAR2 <- (NS * SY1SQ - SUMY1 * SUMY1) / (NS * (NS - 1))
  VAR3 <- (NS * SX2SQ - SUMX2 * SUMX2) / (NS * (NS - 1))
  COV23 <- (NS * SY1X2 - SUMY1 * SUMX2) / (NS * (NS - 1))
  return(cbind(VAR2,VAR3,COV23))
}
#-----------------------------------------------------------------------------------
#' Calculate covariance for Y1 and Y2
#'
#' @param X1 X data set 1 (u)
#' @param Y1 Y data set 1 (v)
#' @param X2 X data set 2 (u)
#' @param Y2 Y data set 2 (v)
#' @param NS # Defined in seabird_vector_correlation() via length(X1)
#'
#' @return Three-column matrix containing the variances of Y1 and Y2 and the
#'   covariance of Y1 and Y2.
#'
STAT5 <- function(X1,Y1,X2,Y2,NS){

  SUMY1 <- 0.0
  SUMY2 <- 0.0
  SY1SQ <- 0.0
  SY2SQ <- 0.0
  SY1Y2 <- 0.0
  for (i in 1:NS){
    SUMY1 <- SUMY1 + Y1[i]
    SUMY2 <- SUMY2 + Y2[i]
    SY1SQ <- SY1SQ + Y1[i]*Y1[i]
    SY2SQ <- SY2SQ + Y2[i]*Y2[i]
    SY1Y2 <- SY1Y2 + Y1[i]*Y2[i]
  }
  VAR2 <- (NS * SY1SQ - SUMY1 * SUMY1) / (NS * (NS - 1))
  VAR4 <- (NS * SY2SQ - SUMY2 * SUMY2) / (NS * (NS - 1))
  COV24 <- (NS * SY1Y2 - SUMY1 * SUMY2) / (NS * (NS - 1))
  return(cbind(VAR2,VAR4,COV24))
}
#-----------------------------------------------------------------------------------
#' Calculate covariance for X2 and Y2
#'
#' @param X1 X data set 1 (u)
#' @param Y1 Y data set 1 (v)
#' @param X2 X data set 2 (u)
#' @param Y2 Y data set 2 (v)
#' @param NS # Defined in seabird_vector_correlation() via length(X1)
#'
#' @return Three-column matrix containing the variances of X2 and Y2 and the
#'   covariance of X2 and Y2.
#'
STAT6 <- function(X1,Y1,X2,Y2,NS){

  SUMX2 <- 0.0
  SUMY2 <- 0.0
  SX2SQ <- 0.0
  SY2SQ <- 0.0
  SX2Y2 <- 0.0
  for (i in 1:NS){
    SUMX2 <- SUMX2 + X2[i]
    SUMY2 <- SUMY2 + Y2[i]
    SX2SQ <- SX2SQ + X2[i]*X2[i]
    SY2SQ <- SY2SQ + Y2[i]*Y2[i]
    SX2Y2 <- SX2Y2 + X2[i]*Y2[i]
  }
  VAR3 <- (NS * SX2SQ - SUMX2 * SUMX2) / (NS * (NS - 1))
  VAR4 <- (NS * SY2SQ - SUMY2 * SUMY2) / (NS * (NS - 1))
  COV34 <- (NS * SX2Y2 - SUMX2 * SUMY2) / (NS * (NS - 1))
  return(cbind(VAR3,VAR4,COV34))
}
#-----------------------------------------------------------------------------------
#' Use covariance values to calculate correlation coefficient, R2.
#'
#' @param VAR1 Variance of X1
#' @param VAR2 Variance of Y1
#' @param VAR3 Variance of X2
#' @param VAR4 Variance of Y2
#' @param COV12 Covariance of X1 and Y1
#' @param COV13 Covariance of X1 and X2
#' @param COV14 Covariance of X1 and Y2
#' @param COV23 Covariance of Y1 and X2
#' @param COV24 Covariance of Y1 and Y2
#' @param COV34 Covariance of X2 and Y2
#'
#' @return Value. Vector correlation coefficient, between vexctors X and Y which
#'   will range between 0.0 (uncorrelated) and 2.0 (completely correlated).
#'
RSQ <- function(VAR1,VAR2,VAR3,VAR4,COV12,COV13,COV14,COV23,COV24,COV34){

  # Multiply variances and covariances to create population covariance matrix
  A1 <- VAR2 * COV13
  A2 <- COV12 * COV23
  A3 <- VAR4 * COV13
  A4 <- COV34 * COV14
  A5 <- VAR2 * COV14
  A6 <- COV12 * COV24
  A7 <- COV34 * COV13
  A8 <- VAR3 * COV14
  B1 <- COV12 * COV13
  B2 <- VAR1 * COV23
  B3 <- VAR4 * COV23
  B4 <- COV34 * COV24
  B5 <- COV12 * COV14
  B6 <- VAR1 * COV24
  B7 <- COV34 * COV23
  B8 <- VAR3 * COV24

  # Subtract covariance^2 products from the variance*covariance products
  C <- A1 - A2
  D <- A3 - A4
  E <- A5 - A6
  F1 <- A8 - A7
  G <- B2 - B1
  H <- B3 - B4
  I <- B6 - B5
  J <- B8 - B7

  # Add the products of the differences between covariance^2 and
  # variance*covariance products
  M <- C*D + E*F1
  N <- G*H + I*J

  # Multiply the sample variances and covariances
  T1 <- VAR1 * VAR2 # X1*Y1
  T2 <- COV12 * COV12 # (X1*Y1)^2
  U1 <- VAR3 * VAR4 # X2*Y2
  U2 <- COV34 * COV34 # (X2*Y2)^2

  # Subtract the covariances from the variances
  T3 <- T1 - T2 # (X1*Y1) - (X1*Y1)^2
  U <- U1 - U2 # (X2*Y2) - (X2*Y2)^2

  # Multiply difference between the u-vector variances by the difference between
  # the v-vector variances
  P <- T3 * U

  # vector correlation coefficient =
  R2sq <- (M + N)/P
  # R <- sqrt(R2sq)
  return(R2sq)
}

sample.line <- function(x, sdist=100){
  if (!require(sp)) stop("sp PACKAGE MISSING")
  if (!inherits(x, "SpatialLinesDataFrame")) stop("MUST BE SP SpatialLinesDataFrame OBJECT")
  lgth <- SpatialLinesLengths(x) 
  lsub <- x[1,]
  ns <- round( (lgth[1] / sdist), digits=0)
  lsamp <- spsample(lsub, n=ns, type="regular", offset=c(0.5,0.5))
  results <- SpatialPointsDataFrame(lsamp, data=data.frame(ID=rep(1:length(lsamp)))) 
  ( results )
}
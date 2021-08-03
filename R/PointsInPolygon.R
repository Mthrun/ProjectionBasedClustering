PointsInPolygon=function(Points,PolygonMatrix,BMUorProjected=F,PlotIt=FALSE,main="Projected Points",...){
  if (!requireNamespace("secr", quietly = TRUE)) {
    message("Subordinate 'secr' is missing. No computations are performed.\n Please install the package which is defined in \"Suggests\".")
    return(Cls="Subordinate clustering package (secr) is missing.\nPlease install the package which is defined in 'Suggests'.")
  }

  if (!is.matrix(Points))
    stop('ProjectedPoints has to be a matrix')
  
  c = ncol(Points)
  if (c > 3 | c < 2)
    stop(paste0('Wrong number of Columns of ProjectedPoints: ', c))
  if (c == 3) {
    #With Key
    if (BMUorProjected) {
      Y = Points[, 2]
      X = Points[, 3]
    } else{
      X = Points[, 2]
      Y = Points[, 3]
    }
  } else{
    #Without Key
    if (BMUorProjected) {
      Y = Points[, 1]
      X = Points[, 2]
    } else{
      X = Points[, 1]
      Y = Points[, 2]
    }
  }
  
  Cls=secr::pointsInPolygon(xy = cbind(X,Y), poly = PolygonMatrix)+1
  if(isTRUE(PlotIt)){
    if (!requireNamespace("DataVisualizations", quietly = TRUE)) {
      message("Subordinate 'DataVisualizations' is missing. No computations are performed.\n Please install the package which is defined in \"Suggests\".Plotting is disabled")
    }else{
      DataVisualizations::Classplot(X = X,Y = Y,Cls = Cls,Plotter = "native",main = main,...)
      lines(PolygonMatrix)
    }
  }
  return(Cls)
}
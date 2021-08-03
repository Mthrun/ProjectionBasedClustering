\name{PointsInPolygon}
\alias{PointsInPolygon}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
PointsInPolygon
}
\description{
Defines a Cls based on points in a given polygon.
}
\usage{
PointsInPolygon(Points, PolygonMatrix, BMUorProjected = F,
 PlotIt = FALSE, main = "Projected Points",\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Points}{
  	[1:n,1:2] xy cartesian coordinates of a projection
}
  \item{PolygonMatrix}{
  numerical matrix of 2 columns defining a closed polygon
  
}
  \item{BMUorProjected}{
	Default ==FALSE, If TRUE assuming BestMatches of ESOM instead of Projected Points
}
  \item{PlotIt}{
  TRUE: Plots marked points
}
  \item{main}{
  title of plot
}
  \item{\dots}{
  Further Plotting Arguments,xlab etc used in \code{\link[DataVisualizations]{Classplot}}
}
}
\details{
Closed polygon means that first and last point have to be the same.
}
\value{
Numerical classification vector Cls with 1=outside polygon and 2= inside polygon
}

\author{
Michael Thrun
}
\seealso{
\code{\link[DataVisualizations]{Classplot}}
}
\examples{
XY=cbind(runif(100,min = -1,max = 1),rnorm(100))
#closed polygon
polymat <- cbind(x = c(0,1,1,0,0), y = c(0,0,1,1,0))
Cls=PointsInPolygon(XY,polymat,PlotIt = TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.

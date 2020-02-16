\name{ProjectionBasedClustering}
\alias{ProjectionBasedClustering}
\title{
automated Clustering approach of the Databonic swarm with abstact U distances
}
\description{
automated Clustering approach of the Databonic swarm with abstact U distances, which
are the geodesic distances based on high-dimensional distances combined with low dimensional graph paths by using \code{ShortestGraphPathsC}.
}
\usage{
ProjectionBasedClustering(k, Data, BestMatches, LC, StructureType = TRUE, PlotIt = FALSE,
                 method = "euclidean")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{number of clusters, how many to you see in the 3d landscape?}
  \item{Data}{[1:n,1:d] Matrix of Data (n cases, d dimensions) that will be used. One DataPoint per row}
  \item{BestMatches}{[1:n,1:2] Matrix with positions of Bestmatches=ProjectedPoints, one matrix line per data point}
  \item{LC}{grid size c(Lines,Columns)}
  \item{StructureType}{Optional, bool; =TRUE: compact structure of clusters assumed, =FALSE: connected structure of clusters assumed. For the two options vor Clusters, see [Thrun, 2017] or Handl et al. 2006}
  \item{PlotIt}{Optional, bool, Plots Dendrogramm}
  \item{method}{Optional, distance method,  do not change}
}
\details{
ProjectionBasedClustering is a flexible and robust clustering framework based on a chose projection method and
 projection method a parameter-free high-dimensional data visualization technique,
 which generates projected points on a topographic map with hypsometric colors, see package GeneralizedUmatrix function GeneralizedUmatrix, 
 called the generalized U-matrix. The clustering method with no 
 sensitive parameters is done by this function. The clustering can be verified by the visualization and vice versa. 
 
 If you want to verifiy your clustering result externally, you can use \code{Heatmap} or \code{SilhouettePlot} of the CRAN package \code{DataVisualizations}.

 Additionally you can use the standard \code{ShepardScatterPlot} or the better approach through the \code{ShepardDensityPlot} of the CRAN package \code{DataVisualizations}.
}
\value{
Cls                 [1:n] vector with selected classes of the bestmatches. You can use \code{plotTopographicMap(Umatrix,Bestmatches,Cls)} for verification.
}
\examples{
data('Hepta')
#2d projection
\donttest{projectionpoints=NeRV(Hepta$Data)
#Computation of Generalized Umatrix
visualization=GeneralizedUmatrix(Data = Hepta$Data,projectionpoints)
# Visualizuation of GenerelizedUmatrix
library(GeneralizedUmatrix)
plotTopographicMap(visualization$Umatrix,visualization$Bestmatches)
# Automatic Clustering
LC=c(visualization$Lines,visualization$Columns)
# number of Cluster from dendrogram or visualization (PlotIt=T)
Cls=ProjectionBasedClustering(k=7, Hepta$Data, 

visualization$Bestmatches, LC,PlotIt=T)
# Verification
plotTopographicMap(visualization$Umatrix,visualization$Bestmatches,Cls)
}
}
\references{
[Thrun/Ultsch, 2017] Thrun, M.C., Ultsch, A.: Projection based Clustering, Conf. Int. Federation of Classification Societies (IFCS),DOI:10.13140/RG.2.2.13124.53124, Tokyo, 2017.

[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, Heidelberg, ISBN: 978-3-658-20539-3, \url{https://doi.org/10.1007/978-3-658-20540-9}, 2018. 
}
\author{
Michael Thrun
}
\note{
If you used pswarm with distance matrix instead of a data matrix you can mds transform your distances into data. The correct dimension can be found through the Sheppard diagram or kruskals stress.

Often it is better to mark the outliers  manually after the prozess of clustering; use in this case the visualization \code{plotTopographicMap} of the package GeneralizedUmatrix. If you would like to mark the outliers interactivly in the visualization use 
the \code{interactiveClustering} function.
}
\keyword{swarm}
\keyword{Databonic swarm}
\keyword{clustering}% __ONLY ONE__ keyword per line
\keyword{cluster analysis}% __ONLY ONE__ keyword per line
\keyword{cluster}% __ONLY ONE__ keyword per line
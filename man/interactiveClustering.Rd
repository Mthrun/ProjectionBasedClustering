\name{interactiveClustering}
\alias{interactiveClustering}
\title{GUI for interactive cluster analysis}

\description{This tool is an interactive shiny tool that visualizes a given generalized Umatrix and allows the user to select areas and mark them as clusters to improve a projection based clustering.}

\arguments{
  \item{Umatrix}{[1:Lines,1:Columns] Matrix of Umatrix Heights}
  \item{Bestmaches}{[1:n,1:2] Array with positions of Bestmatches}
  \item{Cls}{[1:n] Classification of the Bestmatches}
}
\value{
List of
  \item{EnlargedUmatrix}{[1:Lines,1:Columns] Matrix of Umatrix Heights taken four times and arranged in a square 2x2.}
  \item{EnlargedBestmaches}{[1:n,1:2] Array with positions of Bestmatches according to the enlarged umatrix.}
  \item{EnlargedCls}{[1:n] Classification of the Bestmatches according to the enlarged umatrix.}
  \item{Umatrix}{[1:Lines,1:Columns] Matrix of Umatrix Heights}
  \item{Bestmaches}{[1:n,1:2] Array with positions of Bestmatches}
  \item{Cls}{[1:n] Classification of the Bestmatches}
  \item{TopView_TopographicMap}{Plot of a topographic map.}
}

\note{ If you want to verifiy your clustering result externally, you can use \code{Heatmap} or \code{SilhouettePlot} of the CRAN package \code{DataVisualizations}.
}
\details{
Clicking on "Quit" returns the Cls vector to the workspace.
}
\author{Florian Lerch, Michael Thrun}
\references{
[Thrun/Ultsch, 2017] Thrun, M.C., Ultsch, A.: Projection based Clustering, Conf. Int. Federation of Classification Societies (IFCS),DOI:10.13140/RG.2.2.13124.53124, Tokyo, 2017.

[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, Heidelberg, ISBN: 978-3-658-20539-3, \doi{10.1007/978-3-658-20540-9}, 2018. 
}
\examples{
data('Hepta')
#2d projection
# Visualizuation of GeneralizedUmatrix
\donttest{
projectionpoints=NeRV(Hepta$Data)
#Computation of Generalized Umatrix
library(GeneralizedUmatrix)
visualization=GeneralizedUmatrix(Data = Hepta$Data,projectionpoints)
}
## Semi-Automatic Clustering done interactivly in a  shiny gui
\dontrun{
Cls = interactiveClustering(visualization$Umatrix, visualization$Bestmatches)
##Plotting
plotTopographicMap(visualization$Umatrix,visualization$Bestmatches,Cls)
}
}
\name{Isomap}
\alias{Isomap}
\title{ Isomap projection method}
\usage{
Isomap(Inputdistances,k,OutputDimension=2,PlotIt=FALSE,Cls)
}
\arguments{
\item{Inputdistances}{Matrix containing the distances of the data}
\item{k}{number of k nearest neighbors, if the data is fragmented choose an higher k}
 
\item{OutputDimension}{Number of dimensions in the output space, default = 2}
\item{PlotIt}{
	Default: FALSE, If TRUE: Plots the projection as a 2d visualization. 
	 If OutputDimension > 2 only the first two dimensions will be shown.
}
\item{Cls}{Optional and only relevant if PlotIt=TRUE. Numeric vector, given Classification in numbers: every element is the cluster number of a certain corresponding element of data.
}
}
\value{ProjectedPoints[1:n,OutputDimension]   n by OutputDimension matrix containing coordinates of the Projection: A matrix of the fitted configuration..}
\description{
 Isomap procetion as introduced in 2000 by Tenenbaum, de Silva and Langford

Even with a manifold structure, the sampling must be even and dense so that dissimilarities along a manifold are shorter than across the folds. If data do not have such a manifold structure, the results are very sensitive to parameter values.

}
\note{
A wrapper for isomap of the package vegan

if Data fragmented choose an higher k
}

 \author{Michael Thrun}

\keyword{Isomap}
\keyword{isomap}
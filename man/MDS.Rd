\name{MDS}
\alias{MDS}
\title{ Classical multidimensional scaling (MDS)}
\usage{
MDS(DataOrDists,method='euclidean',OutputDimension=2,PlotIt=FALSE,Cls)
}

\arguments{
\item{DataOrDists}{array of data: n cases in rows, d variables in columns, matrix is not symmetric or distance matrix, in this case matrix has to be symmetric}
\item{method}{method specified by distance string: 'euclidean','cityblock=manhatten','cosine','chebychev','jaccard','minkowski','manhattan','binary'}
\item{OutputDimension}{Number of dimensions in the Outputspace, default=2}
  \item{PlotIt}{
	Default: FALSE, If TRUE: Plots the projection as a 2d visualization. 
	}
	\item{Cls}{[1:n,1] Optional,: only relevant if PlotIt=TRUE. Numeric vector, given Classification in numbers: every element is the cluster number of a certain corresponding element of data.
}
}
\value{

  \item{ProjectedPoints}{
 [1:n,OutputDimension], n by OutputDimension matrix containing coordinates of the Projectio
}

  \item{Eigenvalues}{
 the eigenvalues of MDSvalues*MDSvalues'
}
  \item{Stress}{
 Shephard-Kruskal Stress  
}
}
\description{
 Classical multidimensional scaling of a data matrix. Also known as principal coordinates analysis}
\author{Michael Thrun}
\note{
A wrapper for \code{cmdscale}

 You can use the standard \code{ShepardScatterPlot} or the better approach through the \code{ShepardDensityPlot} of the CRAN package \code{DataVisualizations}.
}
 \details{
 An short overview of different types of projection methods can be found in [Thrun, 2018, p.42, Fig. 4.1] (\url{https://doi.org/10.1007/978-3-658-20540-9}).
 }
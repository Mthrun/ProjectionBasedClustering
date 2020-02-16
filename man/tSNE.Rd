\name{tSNE}
\alias{tSNE}
\title{  T-distributed Stochastic Neighbor Embedding}
\usage{
tSNE(DataOrDists,k,OutputDimension=2,Algorithm='tsne_cpp',

method="euclidean",Whitening=FALSE, Iterations=1000,PlotIt=FALSE,Cls,\dots)
}
\arguments{
\item{DataOrDists}{array of data: n cases in rows, d variables in columns, matrix is not symmetric or distance matrix, in this case matrix has to be symmetric}
\item{k}{number of k nearest neighbors=number of effective nearest neighbors("perplexity") Important parameter, if not given Settings of package t-SNE will be used}
  \item{OutputDimension}{Number of dimensions in the Outputspace, default=2}
	\item{Algorithm}{
	tsne_cpp': T-Distributed Stochastic Neighbor Embedding using a Barnes-HutImplementation in C++ of \pkg{Rtsne}
	
    'tsne_r': pure R implementation of the t-SNE algorithm of of \pkg{tsne}
}
\item{method}{	                    method specified by distance string: 
                          'euclidean','cityblock=manhatten','cosine','chebychev','jaccard','minkowski','manhattan','binary' 
}
\item{Whitening}{A boolean value indicating whether the matrix data should be whitened (tsne_r) or if pca should be used priorly 
(tsne_cpp)}
	
	\item{Iterations}{ maximum number of iterations to perform.}

	\item{PlotIt}{
		Default: FALSE, If TRUE: Plots the projection as a 2d visualization. 
		OutputDimension>2: only the first two dimensions will be shown
	}
	\item{Cls}{[1:n,1] Optional,: only relevant if PlotIt=TRUE. Numeric vector, given Classification in numbers: every element is the cluster number of a certain corresponding element of data.
}

\item{\dots}{
Further arguments passed on to either 'Rtsne' or 'tsne' }
}
\value{
List of 
 \item{ProjectedPoints}{[1:n,OutputDimension], n by OutputDimension matrix containing coordinates of the Projection}
  \item{ModelObject}{NULL for tsne_r, further information if tsne_cpp is selected }
}
\description{
  T-distributed Stochastic Neighbor Embedding   res = tSNE(Data, KNN=30,OutputDimension=2) 
	}
 
 \note{
A wrapper for \code{\link[Rtsne]{Rtsne}}

or  for \code{\link[tsne]{tsne}}

 You can use the standard \code{ShepardScatterPlot} or the better approach through the \code{ShepardDensityPlot} of the CRAN package \code{DataVisualizations}.
}
 \details{
 An short overview of different types of projection methods can be found in [Thrun, 2018, p.42, Fig. 4.1] (\url{https://doi.org/10.1007/978-3-658-20540-9}).
 }
 \examples{
 data('Hepta')
 projection=tSNE(Hepta$Data,PlotIt=TRUE,Cls=Hepta$Cls)
 }
 \author{Michael Thrun}


\name{CCA}
\alias{CCA}
\title{Curvilinear Component Analysis }
\usage{
CCA(DataOrDists,Epochs,OutputDimension=2,method='euclidean',

alpha0 = 0.5, lambda0,PlotIt=FALSE,Cls)
}
\arguments{
\item{DataOrDists}{array of data: n cases in rows, d variables in columns, matrix is not symmetric or distance matrix, in this case matrix has to be symmetric epochs                  (scalar) training length}
  
	\item{Epochs}{(scalar) training length}
	\item{OutputDimension}{Number of dimensions in the Outputspace, default=2}
	\item{method}{method specified by distance string. One of: 'euclidean','cityblock=manhatten','cosine','chebychev','jaccard','minkowski','manhattan','binary'}
	\item{alpha0}{(scalar) initial step size, 0.5 by default}
	\item{lambda0}{(scalar) initial radius of influence, 3*max(std(D)) by default}

	
	\item{PlotIt}{
	Default: FALSE, If TRUE: Plots the projection as a 2d visualization. 
	 OutputDimension>2: only the first two dimensions will be shown
	}
	\item{Cls}{[1:n,1] Optional,: only relevant if PlotIt=TRUE. Numeric vector, given Classification in numbers: every element is the cluster number of a certain corresponding element of data.
}
}
\value{A n by OutputDimension matrix containing coordinates of the projected points.}
\description{
  CCA Projects data vectors using Curvilinear Component Analysis.
 
   Unknown values (NaN's) in the data: projections of vectors with
 unknown components tend to drift towards the center of the
  projection distribution. Projections of totally unknown vectors are
  set to unknown (NaN).
	}
 \author{Florian Lerch}
 
 \note{
 Only Transfered from matlab to R. Matlabversion: Contributed to SOM Toolbox 2.0, February 2nd, 2000 by Juha Vesanto
  \url{http://www.cis.hut.fi/projects/somtoolbox/}

 You can use the standard \code{ShepardScatterPlot} or the better approach through the \code{ShepardDensityPlot} of the CRAN package \code{DataVisualizations}.
}
 \details{
 An short overview of different types of projection methods can be found in [Thrun, 2018, p.42, Fig. 4.1] (\url{https://doi.org/10.1007/978-3-658-20540-9}).
 }
 \references{
 Demartines, P., Herault, J., "Curvilinear Component
     Analysis: a Self-Organizing Neural Network for Nonlinear
    Mapping of Data Sets", IEEE Transactions on Neural Networks, 
    vol 8, no 1, 1997, pp. 148-154.
 }


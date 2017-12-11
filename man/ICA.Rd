\name{ICA}
\alias{ICA}
\title{ Independent Component Analysis)}
\usage{
ICA(Data,OutputDimension=2,Contrastfunction="logcosh",

Alpha=1,Iterations=200,PlotIt=FALSE,Cls)
}
\arguments{
\item{Data}{array of data: n cases in rows, d variables in columns, matrix is not symmetric or distance matrix, in this case matrix has to be symmetric}
  \item{OutputDimension}{Number of dimensions in the Outputspace, default=2}
	
	\item{Contrastfunction}{
	Maximierung der Negentropie ueber geeignete geeignete Kontrastfunktion
                             Default: 'logcosh' G(u)=1/a*log cosh(a*u)
                             'exp': G(u)=-exp(u^2/2)
	}
	\item{Alpha}{
	onstant with 1<=alpha<=2 used in approximation to neg-entropy when fun == "logcosh"
	}
	\item{Iterations}{
	 maximum number of iterations to perform.
	}
	\item{PlotIt}{
	Default: FALSE, If TRUE: Plots the projection as a 2d visualization. 
	 OutputDimension>2: only the first two dimensions will be shown
	}
	\item{Cls}{[1:n,1] Optional,: only relevant if PlotIt=TRUE. Numeric vector, given Classification in numbers: every element is the cluster number of a certain corresponding element of data.
}
}
\value{
 \item{ProjectedPoints}{
 [1:n,OutputDimension], n by OutputDimension matrix containing coordinates of the Projectio
}
 \item{Mixing}{
 [1:OutputDimension,1:d]             Mischungsmatrix s.d gilt Data=MixingMatrix*ProjectedPoints      
 }
  \item{Unmixing}{Entmischungsmatrix with Data*Unmixing=ProjectedPoints
 }
  \item{PCMatrix}{
   pre-whitening matrix that projects data onto the first n.comp principal components.
 }
}
\description{
 Independent Component Analysis:
 
 Negentropie: difference of entropy to a corresponding normally-distributed random variable
             J(y)=|E(G(y)-E(G(v)))|^2
 }
 \author{Michael Thrun}
 
 \note{
A wrapper for \code{\link[fastICA]{fastICA}}
}
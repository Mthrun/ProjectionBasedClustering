tSNE = function(DataOrDists,k,OutputDimension=2,method='euclidean',Whitening=TRUE,InitialDimensions=NULL, Iterations=1000,PlotIt=FALSE,Cls){
#  T-distributed Stochastic Neighbor Embedding
#  
#  res = tSNE(Data, k=30,OutputDimension=2)
#   
# INPUT
# DataOrDists[1:n,1:d]      array of data: n cases in rows, d variables in columns, matrix is not symmetric
#                           or distance matrix, in this case matrix has to be symmetric
# k                       number of k nearest neighbors=number of effective nearest neighbors("perplexity")
#                           Important parameter, if not given Settings of package t-SNE will be used
#
# OPTIONAL
# OutputDimension           data is projected onto a R^p where P is the maximum ( default ==2)
# method                    method specified by distance string: 
#                          'euclidean','cityblock=manhatten','cosine','chebychev','jaccard','minkowski','manhattan','binary' 
#
# InitialDimensions         The number of dimensions to use in reduction method.
# Iterations                maximum number of iterations to perform.
# Whitening                 A boolean value indicating whether the matrix data should be whitened
# PlotIt                    bool, defaut=FALSE, if =TRUE: ClassPlot of every current Position of Databots will be made.
#                           OutputDimension>2 only the first two dimensions will be shown
# cls                       vector, Classifikation of Data if available, ClassPlots will be colorized
# 
# OUTPUT is a list with following elements:
# ProjectedPoints[1:n,OutputDimension]                   n by OutputDimension matrix containing coordinates of the Projection: A matrix of the fitted configuration.
#
# Note: Details in http://lvdmaaten.github.io/tsne/
# like "Typical values for the perplexity range between 5 and 50."
# author: MT 06/2015 
  	if(missing(DataOrDists))
		stop('No DataOrDists given')
	DataOrDists;
	if(!is.matrix(DataOrDists))
		stop('DataOrDists has to be a matrix, maybe use as.matrix()')
		
#  requireRpackage('tsne')
requireNamespace('tsne')
  #if(missing(k)){
  #  warning('k - optimal number of neighbors value missing, setting k=30, see default value for perplexity in package tsne') 
  #  k=30
  #} 

  if(isSymmetric(DataOrDists)){
    DataDists=DataOrDists
    AnzVar=ncol(DataOrDists)
    AnzData=nrow(DataOrDists)
  }else{ #!isSymmetric
    AnzVar=ncol(DataOrDists)
    AnzData=nrow(DataOrDists)
	if(is.null(InitialDimensions))
		InitialDimensions=AnzVar
    #DataDists=DistanceMatrix(X=DataOrDists,method = method)
    DataDists = as.matrix(dist( x = DataOrDists, method = method))
  }# end if(isSymmetric(DataOrDists))
	
  if(!missing(k)){
  res=tsne::tsne(X=DataDists, initial_config = NULL, k = OutputDimension, whiten=Whitening,initial_dims = InitialDimensions, perplexity = k, max_iter = Iterations, min_cost = 0,epoch=100)
  }else{
	res=tsne::tsne(X=DataDists, initial_config = NULL, k = OutputDimension, whiten=Whitening,initial_dims = InitialDimensions, max_iter = Iterations, min_cost = 0,epoch=100)
  
	}
	ProjectedPoints=res
  if(PlotIt){
      if(missing(Cls)){
		AnzData=nrow(DataOrDists)
		Cls=rep(1,AnzData)
	}  
    if(!missing(k)){
    string=paste0('T-distributed SNE with perplexity ',k)
    #ClassPlot(ProjectedPoints[,1],ProjectedPoints[,2],Cls=Cls,Title=string)
    }else{
      string=paste0('T-distributed SNE with perplexity ',30)
    }
    PlotProjectedPoints(ProjectedPoints,Cls,main=string)
  } 
return(list(ProjectedPoints=ProjectedPoints))
}    

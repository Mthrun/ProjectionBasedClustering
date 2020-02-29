tSNE = function(DataOrDists,k,OutputDimension=2,Algorithm='tsne_cpp',method='euclidean',Whitening=FALSE, Iterations=1000,PlotIt=FALSE,Cls,...){
#  T-distributed Stochastic Neighbor Embedding
#  
#  res = tSNE(Data, k=30,OutputDimension=2)
#   
# INPUT
# DataOrDists[1:n,1:d]      array of data: n cases in rows, d variables in columns, matrix is not symmetric
#                           or distance matrix, in this case matrix has to be symmetric
# k                         number of k nearest neighbors=number of effective nearest neighbors("perplexity")
#                           Important parameter, if not given Settings of package t-SNE will be used
#
# OPTIONAL
# OutputDimension           data is projected onto a R^p where P is the maximum ( default ==2)
# Algorithm                 'tsne_cpp': T-Distributed Stochastic Neighbor Embedding using a Barnes-HutImplementation in C++
#                            'tsne_r': pure R implementation of the t-SNE algorithm
# method                    method specified by distance string: 
#                          'euclidean','cityblock=manhatten','cosine','chebychev','jaccard','minkowski','manhattan','binary' 
#
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

  #if(missing(k)){
  #  warning('k - optimal number of neighbors value missing, setting k=30, see default value for perplexity in package tsne') 
  #  k=30
  #} 
	is_distance=FALSE
  if(isSymmetric(unname(DataOrDists))){
    DataDists=DataOrDists
    AnzVar=ncol(DataOrDists)
    AnzData=nrow(DataOrDists)
    is_distance=TRUE
  }else{ #!isSymmetric
    AnzVar=ncol(DataOrDists)
    AnzData=nrow(DataOrDists)
    #DataDists=DistanceMatrix(X=DataOrDists,method = method)
    DataDists = as.matrix(dist( x = DataOrDists, method = method))
  }# end if(isSymmetric(DataOrDists))
	
  switch(Algorithm,
         tsne_cpp={
           if(is_distance){
             if(!missing(k)){
             ModelObject=Rtsne::Rtsne(X = DataDists,dims=OutputDimension,is_distance=is_distance,perplexity = k, max_iter = Iterations,pca=Whitening,...)#experimental
             }else{
               ModelObject=Rtsne::Rtsne(X = DataDists,dims=OutputDimension,is_distance=is_distance, max_iter = Iterations,pca=Whitening,...)#experimental
             }
           }else{
             if(!missing(k)){
               ModelObject=Rtsne::Rtsne(X = DataOrDists,dims=OutputDimension,is_distance=is_distance,perplexity = k, max_iter = Iterations,pca=Whitening,...)
             }else{
               ModelObject=Rtsne::Rtsne(X = DataOrDists,dims=OutputDimension,is_distance=is_distance, max_iter = Iterations,pca=Whitening,...)
             }
            
           }
           ProjectedPoints=ModelObject$Y
         },
  tsne_r={
    requireNamespace('tsne')  
    
    if(!missing(k)){
      res=tsne::tsne(X=DataDists, initial_config = NULL, k = OutputDimension, whiten=Whitening, perplexity = k, max_iter = Iterations, ...)
    }else{
      res=tsne::tsne(X=DataDists, initial_config = NULL, k = OutputDimension, whiten=Whitening, max_iter = Iterations, ...)
      
    }
    ProjectedPoints=res
    ModelObject=NULL
  },{stop('Please choose either "tsne_cpp" or "tsne_r".')})


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
return(list(ProjectedPoints=ProjectedPoints,ModelObject=ModelObject))
}    

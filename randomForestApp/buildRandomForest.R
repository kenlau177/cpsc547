
buildRandomForest = function(dat, ntree){
	ensembleFits = llply(1:ntree, .fun=function(x, y){
										return(randomForest(cl~., data=y, ntree=1, 
														proximity=T))}, 
									y=dat)
	rfObj = do.call("combine", ensembleFits)
	attr(rfObj, "ensembleFits") = ensembleFits
	return(rfObj)
}






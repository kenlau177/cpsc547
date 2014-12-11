
buildRandomForest = function(dat, ntree){
	withProgress(message="fitting random forest", value=0, {
		ensembleFits = llply(1:ntree, .fun=function(x, y){
			out = randomForest(cl~., data=y, ntree=1, proximity=T)
			if(x %% 5 == 0){
				incProgress(5/ntree, detail=paste("tree", x))
			}
			return(out)}, y=dat)
	})
	rfObj = do.call("combine", ensembleFits)
	attr(rfObj, "ensembleFits") = ensembleFits
	return(rfObj)
}






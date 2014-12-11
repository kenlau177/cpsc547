
computeHam = function(tree1, tree2){
	predTree1 = tree1$predicted
	predTree2 = tree2$predicted
	
	a1d1 = sum(predTree1 == predTree2, na.rm=T)
	b2c2d2 = sum(predTree1 != predTree2, na.rm=T)
	hStat = (a1d1 - b2c2d2)/(a1d1 + b2c2d2)
	
	return(hStat)
}
computeAvgHam = function(target, rest){
	predTarget = target$predicted
	hamsTarget = laply(rest, .fun=function(x,y){return(computeHam(x,y))}, 
										 y=target)
	avgHamTarget = mean(hamsTarget)
	return(avgHamTarget)
}
computeHamProcess = function(idxTarget, ensembleFits){
	target = ensembleFits[[idxTarget]]
	B = length(ensembleFits)
	idxRest = which(!(1:B %in% idxTarget))
	rest = ensembleFits[idxRest]
	avgHamTarget = computeAvgHam(target, rest)
	return(avgHamTarget)
}
deriveHam = function(rfObj){
	ensembleFits = attributes(rfObj)$ensembleFits	
	idxTargets = 1:length(ensembleFits)
	avgHamTargets = laply(idxTargets, .fun=function(x, y){
		return(computeHamProcess(x, y))}, y=ensembleFits)
}







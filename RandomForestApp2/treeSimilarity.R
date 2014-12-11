
library(randomForest)
library(plyr)

urlTrain = "https://raw.githubusercontent.com/kenlau177/cpsc547/master/Data/datTrn_small.txt"
trainDat <- read.delim(file = urlTrain, sep=",")
#urlTest = "https://raw.githubusercontent.com/kenlau177/cpsc547/master/Data/datTest_small.txt"
#testDat <- read.delim(file = urlTest, sep=",")
trainDat$cl = factor(trainDat$cl)
row.names(trainDat) = 1:nrow(trainDat)

#fit = randomForest(cl~., data=trainDat)
#a = llply(1:fit$ntree, .fun=function(x, rfObj){return(getTree(rfObj, k=x))}, #
#				rfObj=fit)

ensembleFits = llply(1:500, .fun=function(x, dat){
									return(randomForest(cl~., data=dat, ntree=1))}, 
								dat=trainDat)

idxTarget = 1
target = ensembleFits[[idxTarget]]
idxRest = which(!(1:500 %in% idxTarget))
rest = ensembleFits[idxRest]

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
idxTargets = 1:length(ensembleFits)
avgHamTargets = laply(idxTargets, .fun=function(x, y){
									return(computeHamProcess(x, y))}, y=ensembleFits)










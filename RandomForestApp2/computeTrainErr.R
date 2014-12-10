
computeTrainErr = function(obj){
	out = 1 - sum(obj$predicted == obj$y)/length(obj$y)
	return(out)
}






plotImportance = function(obj){
	
	importanceMtx = obj$importance
	importanceDf = data.frame("feature"=row.names(importanceMtx), 
									"importance"=c(importanceMtx))
	
	gg = ggplot(importanceDf, aes(y=importance, x=reorder(feature,importance))) + 
				geom_bar(stat="identity", width=.05) + coord_flip() + 
				geom_point(size=3) + xlab("Features") + 
				ylab("Importance") + 
				ggtitle("Variable Importance") 
	return(gg)
		
}


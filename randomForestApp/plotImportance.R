
plotImportance = function(obj){
	
	importanceMtx = obj$importance
	importanceDf = data.frame("feature"=row.names(importanceMtx), 
									"importance"=c(importanceMtx))
	
	gg = ggplot(importanceDf, aes(y=importance, x=reorder(feature,importance))) + 
				geom_bar(stat="identity", width=.05) + coord_flip() + 
				geom_point(size=3) + xlab("Importance") + 
				ylab("Features") + 
				ggtitle("Variable Importance Plot of Random Forest") 
	return(gg)
		
}


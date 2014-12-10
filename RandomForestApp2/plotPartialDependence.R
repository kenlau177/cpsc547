
plotPartialDependence = function(obj, dat, feature){
	
	classes = unique(dat$cl)
	featureInGlobal <<- feature
	computePd = function(cl, obj, dat){
		# x.var argument does not use the string but the actual variable name
		# https://github.com/dchudz/misc/wiki/partial-plot-global-scope
		out = partialPlot(x=obj, pred.data=dat, 
						x.var=get("featureInGlobal"), 
						which.class=as.character(cl), plot=F)
		names(out) = c(get("featureInGlobal"), as.character(cl))
		out = data.frame(out)
		return(out)
	}
	pds = llply(classes, .fun=computePd, obj=obj, dat=dat)
				
	pdDf = do.call("cbind", pds)		
	pdDf = melt(pdDf, id.vars=get("featureInGlobal"), variable.name="modulation", 
				value.name="marginalEffect")
	gg = ggplot(data=pdDf, aes_string(x=feature, y="marginalEffect")) + 
				geom_line() + facet_wrap(~modulation, nrow=2, ncol=3, scales="free") + 
				ggtitle(paste0("Partial Dependence Plots on ", feature)) + 
				xlab(feature) + ylab("Marginal Effect")
	return(gg)	
}



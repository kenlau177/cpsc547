
plotMDS = function(obj){
	# cite shiny app RF source
	mds = cmdscale(1-obj$proximity, eig=T)
	mdsDf = data.frame(mds$points, obj$y)
	names(mdsDf) = c("dim1", "dim2", "mod")

	gg = ggplot(mdsDf, aes(x=dim1, y=dim2, group=mod, colour=mod)) + 
				geom_jitter(size=2) + ggtitle("MDS Plot")
	return(gg)						
}


# d.new <- reactive({
# 	if(!is.null(rf1())){
# 		mds <- cmdscale(1 - rf1()$proximity, eig=TRUE) ## Do MDS on 1 - proximity
# 		d <- data.frame(mds$points, mds$eig,dat()[,1], as.numeric(margin(rf1())),outlier(rf1()))
# 		names(d) <- c("Dim.1", "Dim.2", "Eigen values",names(dat())[1],"Margin","Outliers")
# 	} else d <- NULL
# 	d
# })
# 
# mdsPlot <- function(d,fontsize){
# 	fontsize <- as.numeric(fontsize)
# 	g1 <- ggplot(data=d,aes_string(x=names(d)[1],y=names(d)[2],group=names(d)[3],colour=names(d)[3])) + theme_grey(base_size=fontsize) + theme(legend.position="top") +
# 		scale_fill_manual(values=c(clrs[1:(nlevels(d[,3]))])) + scale_colour_manual(values=c(clrs[1:(nlevels(d[,3]))])) +
# 		geom_point(size=3)
# 	print(g1)
# }



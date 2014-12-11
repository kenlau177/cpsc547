rm(list=ls())

library(randomForest)
library(reshape2)
library(ggplot2)
library(plyr)
library(dplyr)
library(knitr)

source("plotImportance.R")
source("buildRandomForest.R")
source("deriveHam.R")
source("plotMDS.R")
source("plotPartialDependence.R")
source("computeTrainErr.R")

inf = "data/datTrn_small.txt"
trainDat <- read.delim(file = inf, sep=",")
trainDat$cl = factor(trainDat$cl)
row.names(trainDat) = 1:nrow(trainDat)
hashTable = c("1"="OOK", "2"="BPSK", "3"="OQPSK", "4"="BFSKA", 
							"5"="BFSKB", "6"="BFSKR2")
trainDat$cl = revalue(trainDat$cl, hashTable)
allPlots = c("variable_importance", "proximity", "partial_dependence")

shinyServer(function(input, output) {
	
	output$chooseFeature = renderUI({
		if(!is.null(hamStats())){
			selectizeInput("feature", "Feature", trainDat %>% select(-cl) %>% names, 
				multiple=F)	
		} else {
			return(NULL)
		}
	})

	output$choosePlot = renderUI({
		if(!is.null(hamStats())){
			selectizeInput("plotType", "Select Plot:", 
			 choices=list("variable_importance", "proximity", 
			 "partial_dependence"), multiple=F)
		} else {
			return(NULL)
		}
	})
# 	output$choosePlotType1 = renderUI({
# 		if(!is.null(hamStats())){
# 			selectizeInput("plotType1", "Select Plot for Panel 1:", 
# 				choices=list("variable_importance", "proximity", 
# 				"partial_dependence"), multiple=F)
# 		} else {
# 			return(NULL)
# 		}
# 	})
	
# 	output$choosePlotType2 = renderUI({
# 		if(!is.null(hamStats())){
# 			selectizeInput("plotType2", "Select Plot for Panel 2:", 
# 				choices=list("variable_importance", "proximity", 
# 				"partial_dependence"), multiple=F)
# 		} else {
# 			return(NULL)
# 		}
# 	})
	
# 	output$ntreeTxt <- renderText({
# 		#cat(input$ntree)
# 		#input$actionFit
# 		paste("Number of Trees Selected:", input$ntree)
# 	})

	rfObj = reactive({
		if(!is.null(input$actionFit)){
			if(input$actionFit == 0){
				return()
			} else {
				isolate({
					rfObj = buildRandomForest(dat=trainDat, ntree=input$ntree)
				})
			}
		} else {
			return()
		}
	})
	
	filteredRfObj = reactive({
		if(!is.null(rfObj()) & !is.null(input$hamRange)){
			ensembleFits = attributes(rfObj())$ensembleFits
			idxInclude = which(hamStats() > input$hamRange[1] & 
										hamStats() < input$hamRange[2])
			filteredEnsembleFits = ensembleFits[idxInclude]
			filteredRfObj = do.call("combine", filteredEnsembleFits)
			attr(filteredRfObj, "ensembleFits") = filteredEnsembleFits
			return(filteredRfObj)
		} else {
			return()
		}
	})
	
 	hamStats = reactive({
 		if(!is.null(input$actionFit)){
 			if(input$actionFit == 0){
 				return()
 			} else {
 				isolate({
 					hamStats = deriveHam(rfObj())
 				})
 			}
 		}
	})

	output$hamRangeSlider = renderUI({
		if(!is.null(hamStats())){
			hs = hamStats()
			minHs = round(min(hs)-.001, digits=3)
			maxHs = round(max(hs)+.001, digits=3)
			sliderInput("hamRange", "Hamann Range", min=minHs,  
				max=maxHs, value=c(minHs, maxHs), step=.001, 
				format="#.##")	
		}	else {
			return(NULL)
		}
	})
	
	output$hamRangeMsg = renderText({
		if(!is.null(input$hamRange)){
			f = ecdf(hamStats())
			percentiles = paste0("(", as.character(f(input$hamRange)*100), "%)")
			rangeStrs = paste(input$hamRange, percentiles)
			paste(rangeStrs, collapse=" to ")
		} else {
			return(NULL)
		}
	})
	
	output$trainErrMsg = renderText({
		if(!is.null(filteredRfObj())){
			trainErr = computeTrainErr(filteredRfObj())
			as.character(round(trainErr, 2))
		} else {
			return(NULL)
		}
	})

	output$hamHistogram = renderPlot({
		if(!is.null(input$hamRange) & !is.null(hamStats())){
			ggdata = data.frame("hamStat"=hamStats())
			binwidth = (max(hamStats())-min(hamStats()))/20
			gg = ggplot(ggdata, aes(x=hamStat)) + 
						geom_histogram(fill="white", color="black", binwidth=binwidth) + 
						geom_histogram(data=subset(ggdata, hamStat>input$hamRange[1] & 
							hamStat<input$hamRange[2]), binwidth=binwidth) + 
							xlab("Hamann Similarity") + 
							ggtitle("Histogram of Hamann\nSimilarity of Tree Ensembles")
			return(gg)
		} else {
			return(NULL)
		}
	})
	
	output$fittedMsg = renderText({
		if(!is.null(filteredRfObj())){
			paste("Fitted on", rfObj()$ntree, "Trees")
		} else {
			return(NULL)
		}
	})
	
 	output$panelPlot = renderPlot({
 		if(!is.null(filteredRfObj()) & !is.null(input$hamRange) & 
 			 	length(input$plotType)>0){
 			plotType = input$plotType
 			#cat(plotType1)
 			out = switch(plotType, 
 				variable_importance={
 					plotImportance(filteredRfObj())
 				}, proximity={
 					plotMDS(filteredRfObj())
 				}, partial_dependence={
 					plotPartialDependence(filteredRfObj(), trainDat, 
 						input$feature)
 				}	
 			)
 			return(out)
 		} else {
 			return(NULL)
 		}
 	})
 
# 	output$panel2Plot = renderPlot({
# 		if(!is.null(filteredRfObj()) & !is.null(input$hamRange) & 
# 			 	length(input$plotType2)>0){
# 			plotType2 = input$plotType2
# 			#cat(plotType1)
# 			out = switch(plotType2, 
# 							variable_importance={
# 							 	plotImportance(filteredRfObj())
# 							}, proximity={
# 							 	plotMDS(filteredRfObj())
# 							}, partial_dependence={
# 							 	plotPartialDependence(filteredRfObj(), trainDat, 
# 									input$feature)
# 							}	
# 			)
# 			return(out)
# 		} else {
# 			return(NULL)
# 		}
# 	})
# 
# 

# 	output$importancePlot = renderPlot({
# 		if(!is.null(filteredRfObj()) & !is.null(input$hamRange)){
# #			cat(class(filteredRfObj()))
# 			#cat(input$plotTypes)
# 			plotImportance(filteredRfObj())
# 		} else {
# 			return(NULL)
# 		}
# 	})
	
# 	output$mdsPlot = renderPlot({
# 		if(!is.null(filteredRfObj()) & !is.null(input$hamRange)){
# 			isolate(plotMDS(filteredRfObj()))
# 		} else {
# 			return()
# 		}
# 	})
# 	
# 	output$partialPlot = renderPlot({
# 		#cat(input$chooseFeature)
# 		#cat(input$hamRange)
# 		#cat(filteredRfObj()$proximity)
# 		#cat(input$hamRange)
# 		#cat(input$)
# 		if(!is.null(filteredRfObj()) & !is.null(input$hamRange) & 
# 			 	!is.null(input$feature)){
# 			#cat(input$feature)
# 			#cat(input$hamRange)
# 			plotPartialDependence(filteredRfObj(), trainDat, input$feature)
# 		} else {
# 			return(NULL)
# 		}
# 	})

})

	#output$fitTxt = renderText({
#		cat(input$actionFit)
#		if(input$actionFit){	
#			isolate({paste("Random Forest Fitted on", input$ntree, "Trees")})
#		}
#	})

#multiple_country_data  <- reactive({
#	if(is.null(input$country_from_gapminder)){
#		return(NULL)
#	}
#	yearRange = as.numeric(format(input$year_range_date, "%Y"))
	#cat(paste0(yearRange, "\n"))
#	subset(gDat, country %in% input$country_from_gapminder & 
#				 	year >= yearRange[1] & year <= yearRange[2] )
#})

# output$glimpseData = renderTable({
# 	cat(input$glimpseData)
# 	if(input$glimpseData)
# 		head(trainDat)
# })


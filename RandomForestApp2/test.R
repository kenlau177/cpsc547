
urlTrain = "https://raw.githubusercontent.com/kenlau177/cpsc547/master/Data/datTrn_small.txt"
trainDat <- read.delim(file = urlTrain, sep=",")
trainDat$cl = factor(trainDat$cl)
row.names(trainDat) = 1:nrow(trainDat)
hashTable = c("1"="OOK", "2"="BPSK", "3"="OQPSK", "4"="BFSKA", 
							"5"="BFSKB", "6"="BFSKR2")
trainDat$cl = revalue(trainDat$cl, hashTable)

obj = randomForest(cl~., data=trainDat, proximity=T)

computePredErr = function(obj){
	out = 1 - sum(obj$predicted == obj$y)/length(obj$y)
	return(out)
}

#setwd("C://Users//Ken//Desktop//School//MSC2_Fall//stat545//shinyApp//randomForestApp")
#library(shinyapps)
#shinyapps::deployApp()

switch(plotType1, 
	variable_importance={
	print(1)
		}, proximity={
			 	print(2)
			 }, partial_dependence={
			 	renderPlot({
			 		plotPartialDependence(filteredRfObj(), trainDat, 
			 													input$feature)
			 	})
			 }	
)	


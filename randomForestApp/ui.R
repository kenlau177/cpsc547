
shinyUI(fluidPage(
	titlePanel("Random Forest Ensemble Interpretation App"),
	
	sidebarLayout(
		sidebarPanel("Random Forest Configurations:",
			checkboxInput("glimpseData", label="Glimpse Data"), 
			numericInput("ntree", label="Input number of trees:", 
				value=100, min=100, step=1), 
			actionButton("actionFit", label="Fit Random Forest"), 
			uiOutput("hamRangeSlider"), 
			uiOutput("chooseFeature")
		),	
		mainPanel( 
			tabsetPanel(
				tabPanel("About", includeMarkdown("about.md")),
				tabPanel("App",
					tableOutput("glimpseDat"),
					wellPanel(fluidRow(
							column(width=6, h4("Status:"), 
								textOutput("ntreeTxt"),
								textOutput("fittedMsg"), 
								textOutput("hamRangeMsg")), 
							column(width=6, 
								plotOutput("hamHistogram", height=250, width=250)))),
					plotOutput("importancePlot", height=300, width=500), 
					plotOutput("mdsPlot", height=300, width=500), 
					plotOutput("partialPlot", height=300, width=500))
			)
		)
	)
))


#actionButton("hamButton", label="Derive Hamann Similarity"), 

# I should modify glimpse data to glimpse Info with more information about
# the data set rather than just "head"
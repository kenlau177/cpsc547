
shinyUI(fluidPage(theme = "bootstrap.min.css", 
	titlePanel("Random Forest Ensemble Visualization App"),
	p(strong("Ken Lau")), 
	p(strong("Department of Statistics, UBC")),
	p(strong("ken_lau177@hotmail.com")),
	hr(),
	sidebarLayout(
		sidebarPanel(width=4, h5("Random Forest Configurations:"), 
			numericInput("ntree", label="Input number of trees:", 
				value=100, min=100, step=1), 
			hr(),
			actionButton("actionFit", label="Fit Random Forest"), 
			hr(),
			uiOutput("hamRangeSlider"), 
			uiOutput("choosePlot"),
			uiOutput("chooseFeature")),	
		mainPanel( 
			tabsetPanel(
				tabPanel("App",
					wellPanel(fluidRow(
						column(width=6, h5("Number of Trees:"), 
							textOutput("fittedMsg"), 
							h5("Hamann Range:"),
							textOutput("hamRangeMsg"), 
							h5("Training Error:"),
							textOutput("trainErrMsg")), 
						column(width=6, 
						plotOutput("hamHistogram", height=250, width=250)))),
					plotOutput("panelPlot", height=300, width=500)),
				tabPanel("About", includeMarkdown("about.md"))
			)
		)
	)
))


#actionButton("hamButton", label="Derive Hamann Similarity"), 

#checkboxInput("glimpseData", label="Glimpse Data")
#tableOutput("glimpseData")

# plotOutput("mdsPlot", height=300, width=300)), 
# column(width=6, 
# 			 plotOutput("partialPlot", height=300, width=300))))


navbarPage("Solo Drafting", 
		tabPanel("Drafting", fluid = TRUE, 
				fluidPage(
						
						# For debugging
#						actionLink(inputId = "debug_console", label = "Connect with console"),
						
						titlePanel(title = div(img(src = "logo.png", 
												float = "top", height = "100px", hspace = "0px"),
										"'Arena' Drafting"), 
								windowTitle = "Drafting Prototype"),
						
						
						fluidRow(
								selectInput("wedgeShard", "Select Wedge / Shard:",
										c(      "Random" = "random", 
												"Bant" = "GWUC",
												"Esper" = "WUBC",
												"Grixis" = "UBRC", 
												"Jund" = "BRGC", 
												"Naya" = "WRGC", 
												"Abzan" = "WBGC", 
												"Jeskai" = "WURC", 
												"Sultai" = "BGUC", 
												"Mardu" = "WBRC", 
												"Temur" = "RUGC"				
										
										))
						),
						
						fluidRow(
								column(8, 
										
										fluidRow(
												column(3, 
														plotOutput("plot1", height = "310px") #img(src=paste0(pack()[1], '.jpg') )
												),
												column(3, 
														plotOutput("plot2", height = "310px")
												), 
												column(3, 
														plotOutput("plot3", height = "310px")
												)
										
										), 
										
										fluidRow(
												div(style = "height:10px;width:100%;background-color: #FFFFFF;border-style: solid;border-color: #FFFFFF"
												)
										
										),
										
										fluidRow(
												column(3,
														actionButton("card1picked", label = "Pick Card 1")	
												),
												column(3,
														actionButton("card2picked", label = "Pick Card 2")	
												),
												column(3,
														actionButton("card3picked", label = "Pick Card 3")	
												)
										
										)
								
								),
								column(4,
										textOutput("selectedWedgeShard"),
										textOutput("picksMade")
								)
						
						), 
						fluidRow(
								column(8, 
										tableOutput(outputId = "decklist")	
								)
						)
				
				
				)
		), 
		tabPanel("Deckbuilding", fluid = TRUE, 
				
				fluidRow(
						column(5, 
								
								uiOutput("secondSelection")
						), 
						column(7, 
								verbatimTextOutput("selection"), 
								tableOutput(outputId = "finalDeck"), 
								fluidRow(
										column(4, 
												numericInput("nPlains", "Plains", value = 0),
												numericInput("nMountain", "Mountains", value = 0)
										), 
										column(4, 
												numericInput("nIsland", "Islands", value = 0),
												numericInput("nForest", "Forest", value = 0)
										), 
										column(4, 
												numericInput("nSwamp", "Swamp", value = 0)
										)
								), 
								downloadButton("downloadData", "Download .txt Deck File")
						
						)
				
				
				
				)
		)
)
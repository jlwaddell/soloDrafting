function(input, output, session) {
  
  # Advanced debugging
  observe({
        
        if (is.null(input$debug_console))
          return(NULL)
        
        if (input$debug_console > 0) {
          
          options(browserNLdisabled = TRUE)
          saved_console <- ".RDuetConsole"
          if (file.exists(saved_console)) {load(saved_console)}
          isolate(browser())
          save(file = saved_console, list = ls(environment()))
          
        }
        
      })


	colorID <- reactive({
			if(input$wedgeShard == "random") {
				colorID <- sample(c("GWUC", "WUBC", "UBRC", "BRGC", "WRGC", 
								"WBGC", "WURC", "BGUC", "WBRC", "RUGC"), 1) 
			} else {
				colorID <- input$wedgeShard
			}
			return(colorID)	   
		})

	colorIDprint <- reactive({
				ids <- c("GWUC", "WUBC", "UBRC", "BRGC", "WRGC", 
						"WBGC", "WURC", "BGUC", "WBRC", "RUGC")
				colorNames <- c("Bant", "Esper", "Grixis", "Jund", "Naya", 
						"Abzan", "Jeskai", "Sultai", "Mardu", "Temur")
				return(colorNames[which(ids == colorID())])
				
			})

   cardpool <- reactive({
			   
			   
			   selectedWedgeShard <- unlist(strsplit(colorID(), ""))
			   
			   keepVec <- rep(0, nrow(cube))
			   for(iColor in 1:nrow(cube)) {
				   if(all(colorList[[iColor]] %in% selectedWedgeShard))
					   keepVec[iColor] <- 1
			   }
			   
			   cubeKeep <- cube[which(keepVec == 1), ]
			   cubeKeep <- cubeKeep[sample(1:nrow(cubeKeep), maxPicks * 3, 
							   prob = cubeKeep$Weight, replace = FALSE), ]
			   cubeKeep <- sample(cubeKeep)
			   return(cubeKeep)
			   
		   })
   
	values <- reactiveValues(deck = character(), 
			cmc = numeric(),
			color = character(),
			type = character(),
			offset = 0, 
			maxReached = FALSE) 
	decklist <- reactive({
				tmp <- as.data.frame(values$deck)
				colnames(tmp) <- "Decklist"
				return(tmp)
			})
	

	
	output$plot1 <- renderImage({
				
				filename <- normalizePath(file.path('./www',
								paste0(cardpool()$Card[1+values$offset], '.jpg') ))
				list(src = filename, height = 310)
				
			}, deleteFile = FALSE)
	
	output$plot2 <- renderImage({
				
				filename <- normalizePath(file.path('./www',
								paste0(cardpool()$Card[2+values$offset], '.jpg') ))
				list(src = filename, height = 310)
				
			}, deleteFile = FALSE)
	
	output$plot3 <- renderImage({
				
				filename <- normalizePath(file.path('./www',
								paste0(cardpool()$Card[3+values$offset], '.jpg') ))
				list(src = filename, height = 310)
				
			}, deleteFile = FALSE)
	
	
	observeEvent(input$wedgeShard, {
				
					values$deck <- character()
					values$cmc <- numeric()
					values$offset <- 0
					values$type <- character()
					values$maxReached <- FALSE
				
			})
	
	output$picksMade <- renderText({ paste0("Picks Made: ", length(values$deck), "/", maxPicks) })
	output$selectedWedgeShard <- renderText({ paste0("Wedge/Shard: ", colorIDprint()) })
	
	observeEvent(input$card1picked, {
				if(!values$maxReached) {
					values$deck <- c(values$deck, cardpool()$Card[1+values$offset])
					values$cmc <- c(values$cmc, cardpool()$CMC[1+values$offset])
					values$type <- c(values$type, cardpool()$Type[1+values$offset])
					deck <<- values$deck
					
					if((values$offset + 3) == (maxPicks * 3)) {
						values$maxReached <- TRUE
					} else {
						values$offset <- values$offset + 3
					}
					
				}
									
			})
	
	observeEvent(input$card2picked, {
				if(values$offset < (3 * (maxPicks) )) {
					values$deck <- c(values$deck, cardpool()$Card[2+values$offset])
					values$cmc <- c(values$cmc, cardpool()$CMC[2+values$offset])
					values$type <- c(values$type, cardpool()$Type[2+values$offset])
					values$offset <- values$offset + 3
				}
			
			})
	
	observeEvent(input$card3picked, {
				if(values$offset < (3 * (maxPicks) )) {
					values$deck <- c(values$deck, cardpool()$Card[3+values$offset])
					values$cmc <- c(values$cmc, cardpool()$CMC[3+values$offset])
					values$type <- c(values$type, cardpool()$Type[3+values$offset])
					values$offset <- values$offset + 3
				}
			})
	
#	output$deck <- renderText(draftedCards)

	output$decklist <- renderTable(deckTable(deck = values$deck, cmc = values$cmc))
  
	# TODO add other basics
	output$secondSelection <- renderUI({
				chooserInput("mychooser", "Available frobs", "Selected frobs",
						values$deck[order(values$deck)], c(), size = 35, multiple = TRUE  # LETTERS[1:5]
				)
			})
	output$selection <- renderPrint(
			summarizeDeck(main = input$mychooser$left, 
					draftedCards = values$deck, cmc = values$cmc,
					type = values$type)
	)
	
	output$finalDeck <- renderTable(
			deckTableBuild(main = input$mychooser$left,    # cubeShiny::
					deck = values$deck, cmc = values$cmc))
	
	output$downloadData <- downloadHandler(
			filename = function() {
				paste0(input$wedgeShard, Sys.Date(), ".txt", sep="")
			},
			content = function(file) {
				main <- input$mychooser$left
				main <- sub(" // ", "/", main)
				main[1] <- paste0("1 ", main[1])
				
				if(input$nPlains > 0) main <- c(main, rep("Plains", input$nPlains))
				if(input$nIsland > 0) main <- c(main, rep("Island", input$nIsland))
				if(input$nSwamp > 0) main <- c(main, rep("Swamp", input$nSwamp))
				if(input$nMountain > 0) main <- c(main, rep("Mountain", input$nMountain))
				if(input$nForest > 0) main <- c(main, rep("Forest", input$nForest))
				
				cat(main, sep = "\n1 ", file = file) 	
				cat("\n", file = file, append = TRUE)
				
				side <- c(input$mychooser$right, "Academic Probation", "Basic Conjuration", 
						"Confront the Past", "Containment Breach", "Elemental Summoning", 
						"Environmental Sciences", "Expanded Anatomy", "Fractal Summoning", 
						"Illuminate History", "Inkling Summoning", "Introduction to Annihilation", 
						"Introduction to Prophecy", "Mascot Exhibition", "Mercurial Transformation", 
						"Necrotic Fumes", "Pest Summoning", "Reduce to Memory", "Spirit Summoning", 
						"Start from Scratch", "Teachings of the Archaics") # TODO expand
						
				side[1] <- paste0("1 ", side[1])
				cat(side, sep = "\n1 ", file = file, append = TRUE) 
			}
	)
	
  # Load code for all tabpages
#  source(file.path("serverFiles", "serverLoadData.R"), local = TRUE)
  
}
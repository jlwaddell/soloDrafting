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

   cardpool <- reactive({
			   if(input$wedgeShard == "random") {
				   colorID <- sample(c("GWU", "WUB", "UBR", "BRG", "WRG", 
								   "WBG", "WUR", "BGU", "WBR", "RUG"), 1) 
			   } else {
				   colorID <- input$wedgeShard
			   }
			   
			   selectedWedgeShard <- unlist(strsplit(colorID, ""))
			   
			   keepVec <- rep(0, nrow(cube))
			   for(iColor in 1:nrow(cube)) {
				   if(all(colorList[[iColor]] %in% selectedWedgeShard))
					   keepVec[iColor] <- 1
			   }
			   
			   cubeKeep <- cube[which(keepVec == 1), ]
			   cubeKeep <- cubeKeep[sample(1:nrow(cubeKeep), 
							   prob = cubeKeep$Weight, replace = FALSE), ]
			   cubeKeep <- sample(cubeKeep)
			   return(cubeKeep)
			   
		   })
  
#   offset <- reactive({
#			   input$wedgeShard
#			   return(0)
#		   })
#   
#   deck <- reactive({
#			   input$wedgeShard
#			   return(character())
#		   })
#   
#   cmc <- reactive({
#			   input$wedgeShard
#			   return(numeric())
#		   })
   
	values <- reactiveValues(deck = character(), 
			cmc = numeric(),
			color = character(),
			type = character(),
			offset = 0) 
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
				
			})
	
	output$picksMade <- renderText({ paste0("Picks Made: ", values$offset/3, "/", maxPicks) })

	observeEvent(input$card1picked, {
				if(values$offset < (3 * (maxPicks) )) {
					values$deck <- c(values$deck, cardpool()$Card[1+values$offset])
					values$cmc <- c(values$cmc, cardpool()$CMC[1+values$offset])
					values$type <- c(values$type, cardpool()$Type[1+values$offset])
					values$offset <- values$offset + 3
					deck <<- values$deck
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
						values$deck, c(), size = 35, multiple = TRUE  # LETTERS[1:5]
				)
			})
	output$selection <- renderPrint(
			summarizeDeck(main = input$mychooser$left[order(input$mychooser$left)], 
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
			}
	)
	
  # Load code for all tabpages
#  source(file.path("serverFiles", "serverLoadData.R"), local = TRUE)
  
}
#library(shinythemes)
library(cubeShiny)

# for initial tests, set wd to /ui/
# remove this code later
suppressWarnings({if(!require(cubeShiny)) {
        cat("Loading via devtools...")
        library(devtools)
        load_all("./../../../cubeShiny")
      } else {
        library(cubeShiny)
        cat("Loading the package...")
      }})




# (1) Copy the UI files & folders from "inst/ui" for local use
tmpDir <- tempdir()
setwd(tmpDir)

uiDir <- system.file("ui", package = "cubeShiny")
uiFiles <- list.files(path = uiDir, full.names = FALSE, recursive = TRUE)
uiFiles <- uiFiles[!(uiFiles %in% c("global.R"))]

offset <- 0

# assemble the cube (WIP)
cubeFile <- system.file("extdata", "shinyData.RData", package="cubeShiny")
load(cubeFile)
cube <- data
cube$IdentityHS <- as.character(cube$IdentityHS)
cube$Type <- as.character(cube$Type)
colorList <- strsplit(cube$IdentityHS, "")

cube$Card <- as.character(cube$Card)
for(jCard in 1:nrow(cube))
	if(grepl(" // ", cube$Card[jCard])) {
		cube$Card[jCard] <- gsub(" // ", "", cube$Card[jCard])  # TODO think of a better way to do this
	} 


maxPicks <- 40  # TODO change


chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
		size = 5, multiple = FALSE) {
	
	leftChoices <- lapply(leftChoices, tags$option)
	rightChoices <- lapply(rightChoices, tags$option)
	
	if (multiple)
		multiple <- "multiple"
	else
		multiple <- NULL
	
	tagList(
			singleton(tags$head(
							tags$script(src="chooser-binding.js"),
							tags$style(type="text/css",
									HTML(".chooser-container { display: inline-block; }")
							)
					)),
			div(id=inputId, class="chooser",
					div(class="chooser-container chooser-left-container",
							tags$select(class="left", size=size, multiple=multiple, leftChoices)
					),
					div(class="chooser-container chooser-center-container",
							icon("arrow-circle-o-right", "right-arrow fa-3x"),
							tags$br(),
							icon("arrow-circle-o-left", "left-arrow fa-3x")
					),
					div(class="chooser-container chooser-right-container",
							tags$select(class="right", size=size, multiple=multiple, rightChoices)
					)
			)
	)
}

registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
			if (is.null(data))
				NULL
			else
				list(left=as.character(data$left), right=as.character(data$right))
		}, force = TRUE)








sapply(uiFiles, function(from) {
      
      to <- file.path(tmpDir, from)
      toDir <- dirname(to)
      
      if (!dir.exists(toDir)) {
        
        dir.create(path = toDir, recursive = TRUE)
        
      }
      
      file.copy(from = file.path(uiDir, from), to = to, overwrite = TRUE)
      
    })  


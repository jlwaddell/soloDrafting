#' Run the shiny application
#' @param installDependencies boolean, whether to first install packages listed
#' in the Suggests field of DESCRIPTION; default value is FALSE
#' @param ... further arguments that can be passed to \code{\link[shiny]{runApp}}
#' @return no return value
#' @importFrom shiny runApp
#' @importFrom stats update
#' @export
runCubeShiny <- function(installDependencies = FALSE, ...){
	
	# (1) Install all suggested R packages (see DESCRIPTION)
	if (installDependencies) {
		
		## (a) CRAN packages
		if(requireNamespace("devtools"))
			update(
					devtools::dev_package_deps(
							pkg = system.file("ui", package = "cubeShiny"), dependencies = "Suggests")
			)
		
		
	}
	
	
	# (2) Copy the UI files & folders from "inst/ui" for local use
	
	tmpDir <- tempdir()
	oldDir <- setwd(tmpDir)
	on.exit(setwd(oldDir))
	
	uiDir <- system.file("ui", package = "cubeShiny")
	uiFiles <- list.files(path = uiDir, full.names = FALSE, recursive = TRUE)
	
	sapply(uiFiles, function(from) {
				
				to <- file.path(tmpDir, from)
				toDir <- dirname(to)
				
				if (!dir.exists(toDir)) {
					
					dir.create(path = toDir, recursive = TRUE)
					
				}
				
				file.copy(from = file.path(uiDir, from), to = to, overwrite = TRUE)
				
			})  
	
	
	# (3) Run the application
	runApp(appDir = tmpDir, ...)
	
}





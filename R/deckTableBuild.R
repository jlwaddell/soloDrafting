#' function to generate a table of drafted cards with main deck, with columns corresponding to mana value
#' @param main the cards in the main deck
#' @param deck the drafted card vector
#' @param cmc the mana value vector
#' @returnType matrix
#' @return a matrix of the card names by mana value
#' 
#' @author Jason Waddell
#' @export
deckTableBuild <- function(main, deck, cmc) {
	
	# main <- c("Deathrite Shaman", "Akoum Hellhound")
	# deck <- c("Deathrite Shaman", "Dismember", "Akoum Hellhound")
	# cmc <- c(1, 1, 2)
	
	out <- matrix("", nrow = length(main)+1, ncol = 7)
	if(length(main) > 0) {
		for(i in seq_along(main)) {
			
			if(main[i] %in% deck) {
				cmcTmp <- cmc[which(deck == main[i])[1]]
			} else 
				cmcTmp <- 0
			
			if(cmcTmp < 6) {
				idx <- which(out[, cmcTmp+1 ] == "")[1]
				out[idx, cmcTmp+1] <- main[i]
			} else {
				idx <- which(out[, 7 ] == "")[1]
				out[idx, 7] <- main[i]
			}
			
		}
		
		colnames(out) <- c(as.character(0:5), "6+")
		maxRows <- max(which(rowSums(out == "") == ncol(out) )[1]-1, 3)
		if(length(deck) > 0)
			out <- out[1:(maxRows),  ]
	}
	
	
	
	return(out)
}

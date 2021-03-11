#' Function to summarize the current deck
#' @param main list of cards in the main deck
#' @param draftedCards list of cards in the drafted cardpool
#' @param cmc converted mana cost vector
#' @param type vector of card types (e.g. c("Spell", "Land", ...) )
#' @returnType list
#' @return 
#' 
#' @author Jason Waddell
#' @export
summarizeDeck <- function(main, draftedCards, cmc, type) {
	
	# main <- c("Deathrite Shaman", "Akoum Hellhound", "Mountain")
	# deck <- c("Deathrite Shaman", "Dismember", "Akoum Hellhound")
	# cmc <- c(1, 1, 2)
	
	out <- list()
	typeOut <- rep("", length(main))
	
	for(i in seq_along(main)) {
		if(main[i] %in% draftedCards) {
			idx <- which(draftedCards == main[i])
			typeOut[i] <- type[idx]
		} else
		 	typeOut[i] <- "Land"
	}
	
	out$numberOfLands <- sum(typeOut == "Land")
	out$numberOfSpells <- sum(typeOut == "Spell")
	out$deckSize <- length(main)
	
	return(out)
}
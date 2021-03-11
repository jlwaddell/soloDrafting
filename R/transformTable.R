


#' function to generate a table of drafted cards, with columns corresponding to mana value
#' @param deck the drafted card vector
#' @param cmc the mana value vector
#' @returnType matrix
#' @return a matrix of the card names by mana value
#' 
#' @author Jason Waddell
#' @export
deckTable <- function(deck, cmc) {
	
	out <- matrix("", nrow = 40, ncol = 7)
	if(length(deck) > 0) {
		for(i in 1:length(deck)) {
			
			
			if(cmc[i] < 6) {
				idx <- which(out[, cmc[i]+1 ] == "")[1]
				out[idx, cmc[i]+1] <- deck[i]
			} else {
				idx <- which(out[, 7 ] == "")[1]
				out[idx, 7] <- deck[i]
			}
			
		}
	}
	
	colnames(out) <- c(as.character(0:5), "6+")
	maxRows <- max(which(rowSums(out == "") == ncol(out) )[1]-1, 3)
	if(length(deck) > 0)
		out <- out[1:(maxRows),  ]
	
	return(out)
}


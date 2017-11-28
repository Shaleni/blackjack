# Player functions
# one function for each strategy
# Takes a card to add to the hand, and a boolean to create a new hand
# Returns a list containing the player's hand, and boolean if the player is standing


######################
######Strategy 4######
######################
#Always play to > 17, if total is 18 or more,
#stand, else take a card. Dealer's rule

playerS4 <- function(card, reset) {
	#init/reset card vector
	if(reset) {
		playerCards <<- vector()
	}

	# check for 21, to hit, or to stand
	# you might bust here, only hit when <= 17
	#account for if an ace is valued at 11
	playerCards <<- append(playerCards, card)

	#only want ace to equal 11 if card is 6 or less
	if(length(which(playerCards == -1)) == 1) {
		#1st ace in hand: valued 11
		#additional aces: valued 1
		if(length(which(playerCards == 11)) == 1) {
			playerCards[which(playerCards == -1)] <- 1 #assign value of 1
		} else {
			playerCards[which(playerCards == -1)] <- 11 #assign 11 to ace
		}	
	}
	#check if busted & have an ace that is valued at 11, if so then change value to 1
	if(sum(playerCards) > 21 && length(which(playerCards == 11)) == 1) {
		playerCards[which(playerCards == 11)] <- 1
	}
	return(list(playerCards, sum(playerCards) > 17))
}


#strategy 4 with split hand
playerS4Split <- function(card, reset){
	#init/reset card vector
	if(reset) {
		playerCardsSplit <<- vector()
	}

	# check for 21, to hit, or to stand
	# you might bust here, only hit when <= 16
	#account for if an ace is valued at 11
	playerCardsSplit <<- append(playerCardsSplit, card)

	#only want ace to equal 11 if card is 6 or less
	if(length(which(playerCardsSplit == -1)) == 1) {
		#1st ace in hand: valued 11
		#additional aces: valued 1
		if(length(which(playerCardsSplit == 11)) == 1) {
			playerCards[which(playerCardsSplit == -1)] <- 1 #assign value of 1
		} else {
			playerCards[which(playerCardsSplit == -1)] <- 11 #assign 11 to ace
		}	
	}
	#check if busted & have an ace that is valued at 11, if so then change value to 1
	if(sum(playerCardsSplit) > 21 && length(which(playerCardsSplit == 11)) == 1) {
		playerCardsSplit[which(playerCardsSplit == 11)] <- 1
	}
	return(list(playerCardsSplit, sum(playerCardsSplit) > 17))
}

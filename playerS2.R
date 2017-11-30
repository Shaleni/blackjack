# Player functions
# one function for each strategy
# Takes a card to add to the hand, and a boolean to create a new hand
# Returns a list containing the player's hand, and boolean if the player is standing


######################
######Strategy 2######
######################
#Always play to > 16, if total is 17 or more,
#stand, else take a card. Dealer's rule

playerS2 <- function(card,reset){
  #stand if >16, otherwise hit. Soft 17 stands.
  #create a new card vector (new hand)
  if (reset){
    playerCards <<- vector()
  }
  
  playerCards <<- append(playerCards,card)
  #handle aces
  if(length(which(playerCards==-1))==1){
    #1st ace in hand: valued 11
    #Additional aces: valued 1
    if(length(which(playerCards==11))==1){
      playerCards[which(playerCards==-1)] <<- 1
    } else {
      playerCards[which(playerCards==-1)] <<- 11
    }
  } 
  #check if busted and have an ace valued 11 - change ace that was 11 to 1
  if(sum(playerCards)>21 && length(which(playerCards==11))==1){
    playerCards[which(playerCards==11)] <<- 1
  }
  return(list(playerCards,sum(playerCards)>16))
}


#strategy 2 with split hand
playerS2Split <- function(card, reset){
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
			playerCardsSplit[which(playerCardsSplit == -1)] <<- 1 #assign value of 1
		} else {
			playerCardsSplit[which(playerCardsSplit == -1)] <<- 11 #assign 11 to ace
		}	
	}
	#check if busted & have an ace that is valued at 11, if so then change value to 1
	if(sum(playerCardsSplit) > 21 && length(which(playerCardsSplit == 11)) == 1) {
		playerCardsSplit[which(playerCardsSplit == 11)] <<- 1
	}
	return(list(playerCardsSplit, sum(playerCardsSplit) > 16))
}



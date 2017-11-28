# Player functions
# one function for each strategy
# Takes a card to add to the hand, and a boolean to create a new hand
# Returns a list containing the player's hand, and boolean if the player is standing


######################
######Strategy 5######
######################
#Always play to > 18, if total is 19 or more,
#stand, else take a card. Dealer's rule

playerS5 <- function(card, reset) {
	#init/reset card vector
	if(reset) {
		playerCards <<- vector()
	}

	# check for 21, to hit, or to stand
	# you might bust here, only hit when <= 18
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
	return(list(playerCards, sum(playerCards) > 18))
}


#strategy 2 with split hand
playerS5Split <- function(card, reset){
  #initialize/reset card vector
  if (reset){
    playerCardsSplit <<- vector()
  }
  
  #check for 21, to hit, or to stay
  #want to never bust, so only hit when <=11
  #account for if an ace valued 11 is causing
  #the total value of the hand to be >=11
  playerCardsSplit <<- append(playerCardsSplit,card)
  #check for the one case of ace and a card >=6
  #only time ace would be 11
  if(length(which(playerCardsSplit==-1))==1 && length(which(playerCardsSplit>=6))==1){
    #set ace to 11
    playerCardsSplit[which(playerCardsSplit==-1)] <<- 11
  }else {
    #handle aces
    #set aces to 1
    #otherwise have 11 or 12 and won't hit
    if(length(which(playerCardsSplit==-1))>=1){
      #set aces to 1
      playerCardsSplit[which(playerCardsSplit==-1)] <<- 1
    } 
  }
  #check if busted and still have an ace that's 11 - change ace that was 11 to 1
  if(sum(playerCardsSplit)>21 && length(which(playerCardsSplit==11))==1){
    playerCardsSplit[which(playerCardsSplit==11)] <<- 1
  }
  return(list(playerCardsSplit,sum(playerCardsSplit)> 18))
}

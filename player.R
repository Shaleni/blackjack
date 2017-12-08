# Player functions
# one function for each strategy
# Takes a card to add to the hand, and a boolean to create a new hand
# Returns a list containing the player's hand, and boolean if the player is standing


######################
######Strategy 1######
######################
#Never bust, i.e. never hit when >10
#set Ace to 11 when the other card is >5
#(lowest possible with an ace is 17)
playerS1 <- function(card, reset){
  #initialize/reset card vector
  if (reset){
    playerCards <<- vector()
  }
  
  #check for 21, to hit, or to stay
  #want to never bust, so only hit when <=11
  #account for if an ace valued 11 is causing
  #the total value of the hand to be >=11
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
  #check if busted and still have an ace that's 11 - change ace that was 11 to 1
  if(sum(playerCards)>21 && length(which(playerCards==11))==1){
    playerCards[which(playerCards==11)] <<- 1
  }
  return(list(playerCards,sum(playerCards)>=11))
}

#strategy 1 with split hand
playerS1Split <- function(card, reset){
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
  #check if busted and still have an ace that's 11 - change ace that was 11 to 1
  if(sum(playerCardsSplit)>21 && length(which(playerCardsSplit==11))==1){
    playerCardsSplit[which(playerCardsSplit==11)] <<- 1
  }
  return(list(playerCardsSplit,sum(playerCardsSplit)>=11))
}


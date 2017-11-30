# Player functions
# one function for each strategy
# Takes a card to add to the hand, and a boolean to create a new hand
# Returns a list containing the player's hand, and boolean if the player is standing


######################
######Strategy 3######
######################
#Draw whenever dealer's up card + 10 has you beat
playerS3 <- function(card, reset){
  #initialize/reset card vector
  if (reset){
    playerCards <<- vector()
  }
  
  #check for 21, to hit, or to stay
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
  return(list(playerCards))
}

#strategy 1 with split hand
playerS3Split <- function(card, reset){
  #initialize/reset card vector
  if (reset){
    playerCardsSplit <<- vector()
  }
  
  #check for 21, to hit, or to stay
  #want to never bust, so only hit when <=11
  #account for if an ace valued 11 is causing
  #the total value of the hand to be >=11
  playerCardsSplit <<- append(playerCardsSplit,card)
  #handle aces
  if(length(which(playerCardsSplit==-1))==1){
    #1st ace in hand: valued 11
    #Additional aces: valued 1
    if(length(which(playerCardsSplit==11))==1){
      playerCardsSplit[which(playerCardsSplit==-1)] <<- 1
    } else {
      playerCardsSplit[which(playerCardsSplit==-1)] <<- 11
    }
  } 
  #check if busted and still have an ace that's 11 - change ace that was 11 to 1
  if(sum(playerCardsSplit)>21 && length(which(playerCardsSplit==11))==1){
    playerCardsSplit[which(playerCardsSplit==11)] <<- 1
  }
  return(list(playerCardsSplit))
}


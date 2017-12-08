# Player functions
# one function for each strategy
# Takes a card to add to the hand, and a boolean to create a new hand
# Returns a list containing the player's hand, and boolean if the player is standing


######################
######Strategy 6######
######################
#uses S1 OR S2
#S1:
#Never bust, i.e. never hit when >10
#set Ace to 11 when the other card is >5
#(lowest possible with an ace is 17)
#S2:
#always play to >16 (dealer's rule)
#only thing that changes per strategy is ace handling
playerS6 <- function(card, reset, strategy){
  #initialize/reset card vector
  if (reset){
    playerCards <<- vector()
  }
  
  if(missing(strategy)){
    strategy=F
  }
  playerCards <<- append(playerCards,card)
    #S1
    #check for 21, to hit, or to stay
    #want to never bust, so only hit when <=11
    #account for if an ace valued 11 is causing
    #the total value of the hand to be >=11

    #check for the one case of ace and a card >=6
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
  if(strategy){
    return(list(playerCards,sum(playerCards)>=11))
  } else {
    return(list(playerCards,sum(playerCards)>16))
  }
  
}

#strategy 1 with split hand
playerS6Split <- function(card, reset, strategy){
  #initialize/reset card vector
  if (reset){
    playerCardsSplit <<- vector()
  }
  if(missing(strategy)){
    strategy=F
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
  if(strategy){
    return(list(playerCardsSplit,sum(playerCardsSplit)>=11))
  } else {
    return(list(playerCardsSplit,sum(playerCardsSplit)>16))
  }
}


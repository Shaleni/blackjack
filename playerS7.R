######################
######Strategy 7######
######################


playerS7 <- function(card, reset, strategy){
  #initialize/reset card vector
  if (reset){
    playerCards <<- vector()
  }
  
  if(missing(strategy)){
    strategy=F
  }
  playerCards <<- append(playerCards,card)
  #if strategy = T, use s1
  if(strategy){
    #S1
    #check for 21, to hit, or to stay
    #want to never bust, so only hit when <=11
    #account for if an ace valued 11 is causing
    #the total value of the hand to be >=11

    #check for the one case of ace and a card >=6
    #only time ace would be 11
    if(length(which(playerCards==-1))==1 && length(which(playerCards>=6))==1){
      #set ace to 11
      playerCards[which(playerCards==-1)] <<- 11
    }else {
      #handle aces
      #set aces to 1
      #otherwise have 11 or 12 and won't hit
      if(length(which(playerCards==-1))>=1){
        #set aces to 1
        playerCards[which(playerCards==-1)] <<- 1
      } 
    }
  } else {
    #S2
    #stand if >16, otherwise hit. Soft 17 stands.
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
playerS7Split <- function(card, reset, strategy){
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
  if(strategy){
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
  } else{
    #S2
    #stand if >16, otherwise hit. Soft 17 stands.
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

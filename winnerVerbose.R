# Determine Game Winner - with print statements
# Takes the player's hand, the dealer's hand, split hand (optional)
# Returns a list containing the dealer's hand, and boolean if the dealer is standing

checkForWinnerVerbose<-function(playerCards,dealerCards,splitPlayerCards){
  #initalize values
  numBlackJack <- numOtherWin <- 0
  numTie <- numLoss<- numBust <-0
  amtLeft <- totalBet <-0
  
  #set flags
  pBust <- sum(playerCards)>21
  dBust <- sum(dealerCards)>21
  pBlackjack <- sum(playerCards)==21
  dBlackjack <- sum(dealerCards)==21
  
  #if it was not a split hand, run as normal
  if(missing(splitPlayerCards)){
    #Determine winner and assign money
    #if player is bust, loses money
    if(pBust){
      amtLeft <- amtLeft - 1
      numBust <- numBust +1
      cat("Player Busted with",sum(playerCards), "try again\n")
    } else if (dBlackjack){
      if (!pBlackjack){
        amtLeft <- amtLeft - 1
        numLoss <- numLoss +1
        cat("House got blackjack, player had",sum(playerCards),"\nTry again\n")
      } else{
        #if both get blackjack, no money exchanged
        numTie <- numTie + 1
        cat("House and player both got blackjack, push\n")
      }
    } else if (pBlackjack){
      #player wins
      amtLeft <- amtLeft + 2.5
      numBlackJack <- numBlackJack + 1
      cat("Player got Blackjack!!! House had",sum(dealerCards),"\nCongrats!\n")
    } else if (dBust){
      #player wins
      amtLeft <- amtLeft + 2
      numOtherWin <- numOtherWin + 1
      cat("House busted with",sum(dealerCards),"\nYou had",sum(playerCards),"! Congrats!\n")
    } else if (sum(playerCards)>sum(dealerCards)){
      #player wins
      amtLeft <- amtLeft + 2
      numOtherWin <- numOtherWin + 1
      cat("Player beat house,",sum(playerCards)," to ",sum(dealerCards),"\n")
    } else if (sum(playerCards)==sum(dealerCards)){
      #if both get same amount, no money exchanged
      numTie <- numTie + 1
      cat("House and player both got",sum(playerCards),", push\n")
    }else{
      #player loses
      amtLeft <- amtLeft - 1
      numLoss <- numLoss + 1
      cat("House beat player, ",sum(dealerCards)," to ",sum(playerCards),"\n")
    }
    ###################################
    #output hands
    cat("Dealer hand: ",dealerCards,"\n")
    cat("Player hand: ",playerCards,"\n")
    ###################################
    
    return(cbind(numBlackJack,numOtherWin,numTie,numLoss,numBust,totalBet,amtLeft))
  } else{
    cat("Have a split hand\n")
    cat("Hand 1: ",playerCards,"\n")
    cat("Hand 2: ",splitPlayerCards,"\n")
    cat("Dealer hand: ",dealerCards,"\n")
    #given a split hand:
    #check each hand for an ace and a 10 value; this combination will only win the bet amount
    #rather than the normal 1 1/2 times
    #otherwise, call checkForWinner with each hand and return the combined result
    if(playerCards[1]==11 && sum(playerCards)==21){
      cat("Hand 1 blackjack\n")
      amtLeft <- amtLeft + 2
      numBlackJack <- numBlackJack + 1
    } else {
      cat("Sending hand 1 to checkforwinnerverbose\n")
      checkForWinnerVerbose(playerCards,dealerCards)
    }
    if(splitPlayerCards[1]==11 && sum(splitPlayerCards)==21){
      cat("Hand 2 blackjack\n")
      amtLeft <- amtLeft + 2
      numBlackJack <- numBlackJack + 1
    } else {
      cat("Sending hand 2 to checkforwinnerverbose\n")
      checkForWinnerVerbose(splitPlayerCards,dealerCards)
    }
    
  }
  
  
}

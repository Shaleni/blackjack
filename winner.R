# Determine Game Winner - without print statements
# Takes the player's hand, the dealer's hand, split hand (optional), if using doubling down logic (optional)
# Returns a list containing the dealer's hand, and boolean if the dealer is standing

checkForWinner<-function(playerCards,dealerCards,splitPlayerCards, doublingdown){
  #initalize values
  numBlackJack <- numOtherWin <- 0
  numTie <- numLoss<- numBust <-0
  amtLeft <- totalBet <-0
  
  #set flags
  pBust <- sum(playerCards)>21
  dBust <- sum(dealerCards)>21
  pBlackjack <- sum(playerCards)==21
  dBlackjack <- sum(dealerCards)==21

  if(missing(doublingdown)){
  	doublingdown=FALSE
  }
  
  doubleResult = F
  #check if need to award double cost or nt or not
  if(doublingdown){
  	 if(playerCards[1]+playerCards[2]==10 || playerCards[1]+playerCards[2]==11){
  	doubleResult=T
  	}
  }
 
  #if it was not a split hand, run as normal
  if(missing(splitPlayerCards)){
    #Determine winner and assign money
    #if player is bust, loses money
    #if doubling down lose/win twice as much
    if(pBust){
      amtLeft <- amtLeft - 1
      numBust <- numBust +1
      if(doubleResult){
      	amtLeft <- amtLeft - 1
      }
    } else if (dBlackjack){
      if (!pBlackjack){
        amtLeft <- amtLeft - 1
        numLoss <- numLoss +1
        if(doubleResult){
        	amtLeft <- amtLeft - 1
        }
      } else{
        #if both get blackjack, no money exchanged
        numTie <- numTie + 1
      }
    } else if (pBlackjack){
      #player wins
      amtLeft <- amtLeft + 2.5
      if(doubleResult){
      	amtLeft <- amtLeft + 2.5
      }
      numBlackJack <- numBlackJack + 1
    } else if (dBust){
      #player wins
      amtLeft <- amtLeft + 2
      if(doubleResult){
      	numOtherWin <- numOtherWin + 1
      }
      numOtherWin <- numOtherWin + 1
    } else if (sum(playerCards)>sum(dealerCards)){
      #player wins
      amtLeft <- amtLeft + 2
      if(doubleResult){
      	numOtherWin <- numOtherWin + 1
      }
      numOtherWin <- numOtherWin + 1
    } else if (sum(playerCards)==sum(dealerCards)){
      #if both get same amount, no money exchanged
      numTie <- numTie + 1
    }else{
      #player loses
      amtLeft <- amtLeft - 1
      if(doubleResult){
      	amtLeft <- amtLeft - 1
      }
      numLoss <- numLoss + 1
    }

    return(cbind(numBlackJack,numOtherWin,numTie,numLoss,numBust,totalBet,amtLeft))
  } else{
    #given a split hand:
    #check each hand for an ace and a 10 value; this combination will only win the bet amount
    #rather than the normal 1 1/2 times
    #otherwise, call checkForWinner with each hand and return the combined result
    if(playerCards[1]==11 && sum(playerCards)==21){
      amtLeft <- amtLeft + 2
      numBlackJack <- numBlackJack + 1
    } else {
    	if(doublingdown){
    		checkForWinner(playerCards,dealerCards, doublingdown=TRUE)
    		} else{
    			checkForWinner(playerCards,dealerCards)
    		}   
    }
    if(splitPlayerCards[1]==11 && sum(splitPlayerCards)==21){
      amtLeft <- amtLeft + 2
      numBlackJack <- numBlackJack + 1
    } else {
    	if(doublingdown){
    		checkForWinner(splitPlayerCards,dealerCards, doublingdown=TRUE)
    	} else{
    		checkForWinner(splitPlayerCards,dealerCards)
    	}    
    }
    return(cbind(numBlackJack,numOtherWin,numTie,numLoss,numBust,totalBet,amtLeft))
    
  } 
}

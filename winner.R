# Determine Game Winner - without print statements
# Takes the player's hand, the dealer's hand, split hand (optional), if using doubling down logic (optional)
# Returns a list containing the dealer's hand, and boolean if the dealer is standing

checkForWinner<-function(playerCards,dealerCards,splitPlayerCards, doublingdown, tc){
  #initalize values
  numBlackJack <- numOtherWin <- 0
  numTie <- numLoss<- numBust <-0
  amtLeft <- totalBet <-0
  
  #normal bet is $1/game
  betAmt <- 1

  #set flags
  pBust <- sum(playerCards)>21
  dBust <- sum(dealerCards)>21
  pBlackjack <- sum(playerCards)==21
  dBlackjack <- sum(dealerCards)==21

  if(missing(doublingdown)){
  	doublingdown=FALSE
  }
  
  #see if bet needs to change b/c counting cards
  #anything negative will be normal bets
  if(missing(tc)){
    tc <- -1
  }
 
  #if it was not a split hand, run as normal
  if(missing(splitPlayerCards)){
    #if tc is non-negative, betAmt will be $(tc+2)
    if(tc>-1){
      betAmt <- tc+2
    }

    #check if betting double ($2)
    if(doublingdown){
       if(playerCards[1]+playerCards[2]==10 || playerCards[1]+playerCards[2]==11){
        betAmt <- betAmt * 2
      }
    }

    #Determine winner and assign money
    #if player is bust, loses money
    #if doubling down lose/win twice as much
    if(pBust){
      amtLeft <- amtLeft - betAmt
      numBust <- numBust +1
    } else if (dBlackjack){
      if (!pBlackjack){
        amtLeft <- amtLeft - betAmt
        numLoss <- numLoss +1
      } else{
        #if both get blackjack, no money exchanged
        numTie <- numTie + 1
      }
    } else if (pBlackjack){
      #player wins
      amtLeft <- amtLeft + (1.5*betAmt)
      numBlackJack <- numBlackJack + 1
    } else if (dBust){
      #player wins
      amtLeft <- amtLeft + betAmt
      numOtherWin <- numOtherWin + 1
    } else if (sum(playerCards)>sum(dealerCards)){
      #player wins
      amtLeft <- amtLeft + betAmt
      numOtherWin <- numOtherWin + 1
    } else if (sum(playerCards)==sum(dealerCards)){
      #if both get same amount, no money exchanged
      numTie <- numTie + 1
    }else{
      #player loses
      amtLeft <- amtLeft - betAmt
      numLoss <- numLoss + 1
    }

    return(cbind(numBlackJack,numOtherWin,numTie,numLoss,numBust,totalBet,amtLeft))
  } else{
    #given a split hand:
    #check each hand for an ace and a 10 value; this combination will only win the bet amount
    #rather than the normal 1 1/2 times
    #otherwise, call checkForWinner with each hand and return the combined result
    if(playerCards[1]==11 && sum(playerCards)==21){
      amtLeft <- amtLeft + betAmt
      numBlackJack <- numBlackJack + 1
    } else {
    	if(doublingdown){
    		checkForWinner(playerCards,dealerCards, doublingdown=TRUE, tc=tc)
    		} else{
    			checkForWinner(playerCards,dealerCards, tc=tc)
    		}   
    }
    if(splitPlayerCards[1]==11 && sum(splitPlayerCards)==21){
      amtLeft <- amtLeft + betAmt
      numBlackJack <- numBlackJack + 1
    } else {
    	if(doublingdown){
    		checkForWinner(splitPlayerCards,dealerCards, doublingdown=TRUE, tc=tc)
    	} else{
    		checkForWinner(splitPlayerCards,dealerCards, tc=tc)
    	}    
    }
    return(cbind(numBlackJack,numOtherWin,numTie,numLoss,numBust,totalBet,amtLeft))
    
  } 
}

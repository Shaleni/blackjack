#Blackjack

#one suit
cards <- c(2:10,10,10,10,-1)
names(cards) <- c(2:10,"J","Q","K","A")
#one deck (4 suits)
cards <- rep(cards,4)
#6 decks
cards <- rep(cards,6)

#function to shuffle deck of cards, 
#and randomly pick termination card between 218 and 250
#takes vector input of the cards
#shuffles cards in the vector
#returns a vector that is a subset of shuffled original deck
#based on the termination card number
shuffleCards <- function(c){
  #set.seed(6324)
  #shuffle deck
  c<-cards
  c <- sample(c,length(c),replace=F)
  #pick termination number
  terminate <- trunc(runif(1,218,250))
  #return subset deck to use in session
  return(list(c,terminate))
}

###################################
##############Player###############
###################################
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
  #check for the one case of ace and a card >=6
  #only time ace would be 11
  if(length(which(playerCards==-1))==1 && length(which(playerCards>=6))==1){
    #set ace to 11
    playerCards[which(playerCards==-1)] <- 11
  }else {
    #handle aces
    #set aces to 1
    #otherwise have 11 or 12 and won't hit
    if(length(which(playerCards==-1))>=1){
      #set aces to 1
      playerCards[which(playerCards==-1)] <- 1
    } 
  }
  #check if busted and still have an ace that's 11 - change ace that was 11 to 1
  if(sum(playerCards)>21 && length(which(playerCards==11))==1){
    playerCards[which(playerCards==11)] <- 1
  }
  return(list(playerCards,sum(playerCards)>=11))
}

###################################
##############Dealer###############
###################################
dealer <- function(card,reset){
  #stand if >=17, otherwise hit. Soft 17 stands.
  #initialize/reset card vector
  if (reset){
    dealerCards <<- vector()
  }
  
  dealerCards <<- append(dealerCards,card)
  #handle aces
  #if both the initial cards are aces, one has to be 1, the other could be 11
  if(length(which(dealerCards==-1))==1){
    #check if there's already an ace that's 11
    if(length(which(dealerCards==11))==1){
      dealerCards[which(dealerCards==-1)] <- 1
    } else {
      dealerCards[which(dealerCards==-1)] <- 11
    }
  } 
    #check if busted and still have an ace that's 11 - change ace that was 11 to 1
    if(sum(dealerCards)>21 && length(which(dealerCards==11))==1){
      dealerCards[which(dealerCards==11)] <- 1
    }
  return(list(dealerCards,sum(dealerCards)>16))
}
###################################
##############Winner###############
###################################
checkForWinner<-function(playerCards,dealerCards){
  numBlackJack <- 0
  numOtherWin <- 0
  numTie <- 0
  numLoss<-0
  numBust <-0
  amtLeft <-0
  totalBet <-0
  
  pBust <- sum(playerCards)>21
  dBust <- sum(dealerCards)>21
  pBlackjack <- sum(playerCards)==21
  dBlackjack <- sum(dealerCards)==21
  
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
}

###################################
############Simulations############
###################################
  s1 <- function(){
    #S1. Never bust: Never take a card when you could possibly go over 21.
    #e.g., hit if <= 11, otherwise stand
    
    ###################################
  #initialize values to bind to return
  numBlackJack <- 0
  numOtherWin <- 0
  numTie <- 0
  numLoss<-0
  numBust <-0
  totalBet <-0
  amtLeft <-0
  
  deck <- shuffleCards(cards)
  terminate <- deck[2]
  terminate <- unlist(terminate)
  deck <- deck[1]
  deck<-unlist(deck)
  
  results<-(cbind(numBlackJack,numOtherWin,numTie,numLoss,numBust,totalBet,amtLeft))
  
  counter <- 1
  #deal first card to player and dealer
  playerS1(deck[counter],T)
  counter <- counter + 1
  dealer(deck[counter],T)
  counter <- counter + 1
  
  
  while(counter<=terminate){
    totalBet <- totalBet +1
    
    #deal second card to player and dealer
    p<-playerS1(deck[counter],F)
    counter <- counter + 1
    d<-dealer(deck[counter],F)
    counter <- counter + 1
    
    #finish hands of player and dealer
    while(!(p[[2]])){
      p<-playerS1(deck[counter],F)
      counter <- counter + 1
    }
    while(!unlist(d[2])){
      d<-dealer(deck[counter],F)
      counter <- counter + 1
    }
    #get the results of the game
    r<-checkForWinner(unlist(p[1]),unlist(d[1]))
    results <- results+r
    
    #deal first card of next game to player and dealer
    p<-playerS1(deck[counter],T)
    counter <- counter + 1
    d<-dealer(deck[counter],T)
    counter <- counter + 1
  }
  
  results[6] <- totalBet
  return(results)
}

result <- s1()

finalResults <- rowSums(sapply(1:10,function(i) sapply(X=s1(),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
#finalResultsProb <- finalResults/finalResults[6]
finalResults
#finalResultsProb

simulationRunner <- function(){
  finalResults <- vector()
  for (i in 1:100){
    result <- s1()
    result<-as.vector(result)
    finalResults <-finalResults+result
  }
  
  return(finalResults/finalResults[6])
}




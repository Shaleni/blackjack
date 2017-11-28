#blackjackSimulator.R
#version of blackjack.R that uses different files for the functions

#setwd("C:/Users/shale_000/OneDrive/SMU/Fall2017/STAT 6324/blackjack")
setwd("/Users/quincyschurr/Documents/blackjack")

######################
##Constant Functions##
######################
#shuffleCards, dealer, and checkForWinner do not change from simulation to simulation

#import shuffleCards
#args: vector of cards
#returns: list(shuffledcards vector, termination index)
source("shuffle.R")

#import dealer
#args: card to add to hand, boolean (T-create new hand|F-add to existing hand)
#returns: list(dealer's hand vector, boolean (T-dealer is standing|F-dealer's hand <16))
source("dealer.R")

#import checkForWinner
#args:
#returns:
#winnerVerbose contains print statements with dealer and player hands, outcome, and totals per game
#winner runs the same code without the print statements
source("winnerVerbose.R")
source("winner.R")

######################
######Player File#####
######################
#contains a player function for each strategy
#Each function has the same arguments/returns
#args: card to add to hand, boolean (T-create new hand|F-add to existing hand)
#returns: list(player's hand vector, boolean (T-player is standing|F-based on strategy))
source("player.R")
source("playerS2.R")
source("playerS4.R")
source("playerS5.R")

#################
##Generate Deck##
#################
#this is the base deck that will be shuffled when needed
#cards will not be changed

#one suit
cards <- c(2:10,10,10,10,-1)
names(cards) <- c(2:10,"J","Q","K","A")
#one deck (4 suits)
cards <- rep(cards,4)
#6 decks
cards <- rep(cards,6)

#######################
######Simulations######
#######################
#strategy = 1: base strategy
#strategy = 7: splitting
#strategy = 13: splitting and doubling down
s1 <- function(results,deck,terminate, strategy){
  if(strategy==1){
    #play with split flag
    pws = F
    dDown = F
  } else if (strategy==7){
    pws=T
    dDown=F
  } else{
    pws=T
    dDown=T
  }
  
  totalBet<-0
  
  counter <- 1
  #deal first card to player and dealer
  p<-playerS1(deck[counter],T)
  counter <- counter + 1
  dealer(deck[counter],T)
  counter <- counter + 1
  
  #go through the deck, last game is with the termination card
  while(counter<=terminate){
    totalBet <- totalBet +1
    #split hand flag
    split=F
    splitWithAce=F
    
    #if playing with a split hand, check next card for possible split before dealing it
    if(pws){
      
      if(names(deck[counter])==names(p[[1]]) && p[[1]]!=5 && p[[1]]!=10){
        #split the deck
        split=T
        totalBet <- totalBet +1
        p2<-playerS1Split(deck[counter],T)
        
        #check for aces
        if(names(deck[counter])=="A"){
          #set ace value to 11
          playerCards[1]<<-11
          playerCardsSplit[1]<<-11
          splitWithAce=T
        }
        counter <- counter + 1
        #deal second card to player hands and dealer
        p<-playerS1(deck[counter],F)
        counter <- counter + 1
        p2<-playerS1Split(deck[counter],F)
        counter <- counter + 1
        d<-dealer(deck[counter],F)
        counter <- counter + 1
        
      } else{
        #no split, play normally
        #deal second card to player and dealer
        p<-playerS1(deck[counter],F)
        counter <- counter + 1
        d<-dealer(deck[counter],F)
        counter <- counter + 1
      }
    } else{
      #not playing with splitting
      #deal second card to player and dealer
      p<-playerS1(deck[counter],F)
      counter <- counter + 1
      d<-dealer(deck[counter],F)
      counter <- counter + 1
    }

    #finish hand(s) of player
    if(!splitWithAce){
      #original hand
      while(!(p[[2]])){
        p<-playerS1(deck[counter],F)
        counter <- counter + 1
      }
      #if there is also a split hand
      if(split){
        while(!(p2[[2]])){
          p2<-playerS1Split(deck[counter],F)
          counter <- counter + 1
        }
      }
    }
  
    #finish hand of dealer
    while(!d[[2]]){
      d<-dealer(deck[counter],F)
      counter <- counter + 1
    }
    #get the results of the game
    if(split){
      r<-checkForWinner(unlist(p[1]),unlist(d[1]),p2[[1]])
      #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]),p2[[1]])
      results <- results+r
    } else{
      r<-checkForWinner(unlist(p[1]),unlist(d[1]))
      #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]))
      results <- results+r
    }
    
    #deal first card of next game to player and dealer
    p<-playerS1(deck[counter],T)
    counter <- counter + 1
    d<-dealer(deck[counter],T)
    counter <- counter + 1
  }
  
  results[6] <- totalBet
  return(results)
}

s2 <- function(results, deck, terminate, strategy) {
  if(strategy==2) {
    #play with split flag - don't have these implemented for S2 yet
    pws = F
    dDown = F
  } else if (strategy==8) { #want to play S2 with split
    pws = T
    dDown = F
  } else {#means playing with double down
    pws = T
    dDown = T
  }

  totalBet <- 0
  counter <- 1
  #deal first card to player and dealer
  p <- playerS2(deck[counter], T)
  counter <- counter + 1
  dealer(deck[counter], T)
  counter <- counter + 1

  #iterate through deck, last game is with the termination card
  while(counter <= terminate) {
    totalBet <- totalBet + 1
    #split the hand flags
    split = F
    splitWithAce = F

    #if playing with a split hand, check next card for possible split before dealing
    if(pws) {
      if(names(deck[counter]) == names(p[[1]]) && p[[1]] != 5 && p[[1]] != 10) {
        #now can split deck
        split = T
        totalBet <- totalBet + 1
        p2 <-playerS2Split(deck[counter], T)

        #check for aces
        if(names(deck[counter]) == "A"){
          #now set ace value to 11
          playerCards[1] <<- 11
          playerCardsSplit[1] <<- 11
          splitWithAce = T
        }
        counter <- counter + 1
        #now deal a second card to player and dealer
        p <- playerS2(deck[counter], F)
        counter <- counter + 1
        p2 <- playerS2Split(deck[counter], F)
        counter <- counter + 1
        d <- dealer(deck[counter], F)
        counter <- counter + 1
      } else {
        #there is no split and play continues as normal
        #deal second card to player and dealer
        p <- playerS2(deck[counter], F)
        counter <- counter + 1
        d <- dealer(deck[counter], F)
        counter <- counter + 1
      }
    } else {
      #not playing with a split
      #deal second card
      p <- playerS2(deck[counter], F)
      counter <- counter + 1
      d <- dealer(deck[counter], F)
      counter <- counter + 1
    }

    #finish hand(s) of the player
    if(!splitWithAce) {
      #original hand
      while(!(p[[2]])) {
        p <- playerS2(deck[counter], F)
        counter <- counter + 1
      }
      #if there is also a split hand
      if(split) {
        while(!(p2[[2]])) {
          p2 <- playerS2Split(deck[counter], F)
          counter <- counter + 1
        }
      }
    }

    #finish dealer hand
    while(!d[[2]]) {
      d <- dealer(deck[counter], F)
      counter <- counter + 1
    }

    #get results of game
    if(split) {
      r <- checkForWinner(unlist(p[1]), unlist(d[1]), p2[[1]])
      #r <- checkForWinnerVerbose(unlist(p[1]),unlist(d[1]),p2[[1]])
      results <- results + r
    } else {
      r <- checkForWinner(unlist(p[1]), unlist(d[1]))
      #r <- checkForWinnerVerbose(unlist(p[1]),unlist(d[1]))
      results <- results + r
    }

    #deal first card of next game to player and dealer
    p <- playerS2(deck[counter], T)
    counter <- counter + 1
    d <- dealer(deck[counter], T)
    counter <- counter + 1
  }

  results[6] <- totalBet
  return(results)
}

#######################
##Running Simulations##
#######################
#runs the different simulations. 
#takes an argument x that shows which strategy to run
#1-6: strategies 1-6
#7-12: strategies 1-6 with splitting
#13-18: strategies 1-6 with splitting and doubling down

runSimulations <- function(x){
  #initialize values to bind to return
  numBlackJack <- numOtherWin <- 0
  numTie <- numLoss<-numBust <-0
  totalBet <-amtLeft <-0
  
  #get the shuffled deck and termination card
  deck <- shuffleCards(cards)
  terminate <- deck[2]
  terminate <- unlist(terminate)
  deck <- deck[1]
  deck<-unlist(deck)
  
  results<-(cbind(numBlackJack,numOtherWin,numTie,numLoss,numBust,totalBet,amtLeft))
  
  if(x==1 || x==7){
    results <- s1(results,deck,terminate, x)
  } else if (x==2) {
    results <- s2(results,deck,terminate, x)
  } else if (x==4) {
    results <- s4(results,deck,terminate, x)
  } else if (x==5) {
    results <- s5(results,deck,terminate, x)
  }
  
  
  return(results)
}


#finalResults <- rowSums(sapply(1:10000,function(i) sapply(X=runSimulations(1),FUN="+")))
finalResults <- rowSums(sapply(1:10000,function(i) sapply(X=runSimulations(2),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults



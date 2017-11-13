#blackjackSimulator.R
#version of blackjack.R that uses different files for the functions

setwd("C:/Users/shale_000/OneDrive/SMU/Fall2017/STAT 6324/blackjack")

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
s1 <- function(results,deck,terminate){
  totalBet<-0
  
  counter <- 1
  #deal first card to player and dealer
  playerS1(deck[counter],T)
  counter <- counter + 1
  dealer(deck[counter],T)
  counter <- counter + 1
  
  #go through the deck, last game is with the termination card
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
    r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]))
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
  
  if(x==1){
    results <- s1(results,deck,terminate)
  }
  
  
  return(results)
}

result <- runSimulations(1)

finalResults <- rowSums(sapply(1:2,function(i) sapply(X=runSimulations(1),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults



#blackjackSimulator.R
#version of blackjack.R that uses different files for the functions

setwd("C:/Users/shale_000/Documents/SMU/Fall2017/STAT 6324/blackjack")
#setwd("/Users/quincyschurr/Documents/blackjack")

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
source("playerS3.R")
source("playerS4.R")
source("playerS5.R")
source("playerS6.R")
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
#strategy = 7: doubling down
#strategy = 13: splitting and doubling down
s1 <- function(results,deck,terminate, strategy){
  if(strategy==1){
    #play with split flag
    pws = F
    dDown = F
  } else if (strategy==7){
    pws=F
    dDown=T
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
    #now have 2 cards in all hand(s)
    #if doubling down, check before drawing any more cards
    if (dDown){
      #if the sum of the hand is 10 or 11, double the bet and only draw one card
      if(sum(p[[1]])==11 || sum(p[[1]])==10){
        #cat("doubled down: hand total",sum(p[[1]]),"\n")
        totalBet <- totalBet + 1
        p<-playerS1(deck[counter],F)
        counter <- counter + 1
      }
      #if split, need to check the other hand too
      if(split){
        if(sum(p2[[1]])==11 || sum(p2[[1]])==10){
          #cat("doubled down on split hand: hand total",sum(p2[[1]]),"\n")
          totalBet <- totalBet + 1
          p2<-playerS1Split(deck[counter],F)
          counter <- counter + 1
        }
      }
    } 
      #finish hands as usual
      #even if doubled down, with S1 will be >=11 so player hands will be standing
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
      if(dDown){
          r<-checkForWinner(unlist(p[1]),unlist(d[1]),p2[[1]], doublingdown=TRUE)
          #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]),p2[[1]], doublingdown=TRUE)
      } else{
        r<-checkForWinner(unlist(p[1]),unlist(d[1]),p2[[1]])
        #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]),p2[[1]])
      }
      results <- results+r
    } else{
      if(dDown){
        r<-checkForWinner(unlist(p[1]),unlist(d[1]), doublingdown=T)
        #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]), doublingdown=T)
      } else {
        r<-checkForWinner(unlist(p[1]),unlist(d[1]))
        #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]))
      }
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

#strategy = 2: base strategy
#strategy = 8: doubling down
#strategy = 14: splitting and doubling down
s2 <- function(results,deck,terminate, strategy){
  if(strategy==2){
    #play with split flag
    pws = F
    dDown = F
  } else if (strategy==8){
    pws=F
    dDown=T
  } else{
    pws=T
    dDown=T
  }
  
  totalBet<-0
  
  counter <- 1
  #deal first card to player and dealer
  p<-playerS2(deck[counter],T)
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
        p2<-playerS2Split(deck[counter],T)
        
        #check for aces
        if(names(deck[counter])=="A"){
          #set ace value to 11
          playerCards[1]<<-11
          playerCardsSplit[1]<<-11
          splitWithAce=T
        }
        counter <- counter + 1
        #deal second card to player hands and dealer
        p<-playerS2(deck[counter],F)
        counter <- counter + 1
        p2<-playerS2Split(deck[counter],F)
        counter <- counter + 1
        d<-dealer(deck[counter],F)
        counter <- counter + 1
        
      } else{
        #no split, play normally
        #deal second card to player and dealer
        p<-playerS2(deck[counter],F)
        counter <- counter + 1
        d<-dealer(deck[counter],F)
        counter <- counter + 1
      }
    } else{
      #not playing with splitting
      #deal second card to player and dealer
      p<-playerS2(deck[counter],F)
      counter <- counter + 1
      d<-dealer(deck[counter],F)
      counter <- counter + 1
    }
    #now have 2 cards in all hand(s)
    #if doubling down, check before drawing any more cards
    if (dDown){
      #if the sum of the hand is 10 or 11, double the bet and only draw one card
      if(sum(p[[1]])==11 || sum(p[[1]])==10){
        #cat("doubled down: hand total",sum(p[[1]]),"\n")
        totalBet <- totalBet + 1
        p<-playerS2(deck[counter],F)
        counter <- counter + 1
      }
      #if split, need to check the other hand too
      if(split){
        if(sum(p2[[1]])==11 || sum(p2[[1]])==10){
          #cat("doubled down on split hand: hand total",sum(p2[[1]]),"\n")
          totalBet <- totalBet + 1
          p2<-playerS2Split(deck[counter],F)
          counter <- counter + 1
        }
      }
    } 
      #finish hands as usual
      #even if doubled down, with S1 will be >=11 so player hands will be standing
      #finish hand(s) of player
      if(!splitWithAce){
        #original hand
        while(!(p[[2]])){
          p<-playerS2(deck[counter],F)
          counter <- counter + 1
        }
        #if there is also a split hand
        if(split){
          while(!(p2[[2]])){
            p2<-playerS2Split(deck[counter],F)
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
      if(dDown){
          r<-checkForWinner(unlist(p[1]),unlist(d[1]),p2[[1]], doublingdown=TRUE)
          #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]),p2[[1]], doublingdown=TRUE)
      } else{
        r<-checkForWinner(unlist(p[1]),unlist(d[1]),p2[[1]])
        #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]),p2[[1]])
      }
      results <- results+r
    } else{
      if(dDown){
        r<-checkForWinner(unlist(p[1]),unlist(d[1]), doublingdown=T)
        #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]), doublingdown=T)
      } else {
        r<-checkForWinner(unlist(p[1]),unlist(d[1]))
        #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]))
      }
      results <- results+r
    }
    
    #deal first card of next game to player and dealer
    p<-playerS2(deck[counter],T)
    counter <- counter + 1
    d<-dealer(deck[counter],T)
    counter <- counter + 1
  }
  
  results[6] <- totalBet
  return(results)
}

#strategy = 3: base strategy
#strategy = 9: doubling down
#strategy = 15: splitting and doubling down
s3 <- function(results,deck,terminate,strategy){
  if(strategy==3){
    #play with split flag
    pws = F
    dDown = F
  } else if (strategy==9){
    pws=F
    dDown=T
  } else{
    pws=T
    dDown=T
  }
  
  totalBet<-0
  
  counter <- 1
  #deal first card to player and dealer
  p<-playerS3(deck[counter],T)
  counter <- counter + 1
  d<-dealer(deck[counter],T)
  counter <- counter + 1
  
  #go through the deck, last game is with the termination card
  while(counter<=terminate){
    totalBet <- totalBet +1
    #split hand flag
    split=F
    splitWithAce=F
    doubledDown = F
    doubledDownSplit = F
    
    #if playing with a split hand, check next card for possible split before dealing it
    if(pws){
      
      if(names(deck[counter])==names(p[[1]]) && p[[1]]!=5 && p[[1]]!=10){
        #split the deck
        split=T
        totalBet <- totalBet +1
        p2<-playerS3Split(deck[counter],T)
        
        #check for aces
        if(names(deck[counter])=="A"){
          #set ace value to 11
          playerCards[1]<<-11
          playerCardsSplit[1]<<-11
          splitWithAce=T
        }
        counter <- counter + 1
        #deal second card to player hands and dealer
        p<-playerS3(deck[counter],F)
        counter <- counter + 1
        p2<-playerS3Split(deck[counter],F)
        counter <- counter + 1
        d<-dealer(deck[counter],F)
        counter <- counter + 1
        
      } else{
        #no split, play normally
        #deal second card to player and dealer
        p<-playerS3(deck[counter],F)
        counter <- counter + 1
        d<-dealer(deck[counter],F)
        counter <- counter + 1
      }
    } else{
      #not playing with splitting
      #deal second card to player and dealer
      p<-playerS3(deck[counter],F)
      counter <- counter + 1
      d<-dealer(deck[counter],F)
      counter <- counter + 1
    }
    #now have 2 cards in all hand(s)
    #if doubling down, check before drawing any more cards
    if (dDown){
      #if the sum of the hand is 10 or 11, double the bet and only draw one card
      if(sum(p[[1]])==11 || sum(p[[1]])==10){
        #cat("doubled down: hand total",sum(p[[1]]),"\n")
        totalBet <- totalBet + 1
        p<-playerS1(deck[counter],F)
        counter <- counter + 1
        doubledDown = T
      }
      #if split, need to check the other hand too
      if(split){
        if(sum(p2[[1]])==11 || sum(p2[[1]])==10){
          #cat("doubled down on split hand: hand total",sum(p2[[1]]),"\n")
          totalBet <- totalBet + 1
          p2<-playerS1Split(deck[counter],F)
          counter <- counter + 1
          doubledDownSplit = T
        }
      }
    } 
    #finish hands as usual
    #finish hand(s) of player
    if(!splitWithAce){
      #original hand
      if(!doubledDown){
        #while the sum of your hand is < dealer's first card+10
        while(sum(p[[1]])<(d[[1]][1]+10)){
          p<-playerS3(deck[counter],F)
          counter <- counter + 1
        }
      }
      #if there is also a split hand
      if(split){
        if(!doubledDownSplit){
          while(sum(p2[[1]])<(d[[1]][1]+10)){
            p2<-playerS3Split(deck[counter],F)
            counter <- counter + 1
          }
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
      if(dDown){
        #r<-checkForWinner(unlist(p[1]),unlist(d[1]),p2[[1]], doublingdown=TRUE)
        r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]),p2[[1]], doublingdown=TRUE)
      } else{
        #r<-checkForWinner(unlist(p[1]),unlist(d[1]),p2[[1]])
        r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]),p2[[1]])
      }
      results <- results+r
    } else{
      if(dDown){
        #r<-checkForWinner(unlist(p[1]),unlist(d[1]), doublingdown=T)
        r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]), doublingdown=T)
      } else {
        #r<-checkForWinner(unlist(p[1]),unlist(d[1]))
        r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]))
      }
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

#strategy = 4: base strategy
#strategy = 10: doubling down
#strategy = 16: splitting and doubling down
s4 <- function(results,deck,terminate, strategy){
  if(strategy==4){
    #play with split flag
    pws = F
    dDown = F
  } else if (strategy==10){
    pws=F
    dDown=T
  } else{
    pws=T
    dDown=T
  }
  
  totalBet<-0
  
  counter <- 1
  #deal first card to player and dealer
  p<-playerS4(deck[counter],T)
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
        p2<-playerS4Split(deck[counter],T)
        
        #check for aces
        if(names(deck[counter])=="A"){
          #set ace value to 11
          playerCards[1]<<-11
          playerCardsSplit[1]<<-11
          splitWithAce=T
        }
        counter <- counter + 1
        #deal second card to player hands and dealer
        p<-playerS4(deck[counter],F)
        counter <- counter + 1
        p2<-playerS4Split(deck[counter],F)
        counter <- counter + 1
        d<-dealer(deck[counter],F)
        counter <- counter + 1
        
      } else{
        #no split, play normally
        #deal second card to player and dealer
        p<-playerS4(deck[counter],F)
        counter <- counter + 1
        d<-dealer(deck[counter],F)
        counter <- counter + 1
      }
    } else{
      #not playing with splitting
      #deal second card to player and dealer
      p<-playerS4(deck[counter],F)
      counter <- counter + 1
      d<-dealer(deck[counter],F)
      counter <- counter + 1
    }
    #now have 2 cards in all hand(s)
    #if doubling down, check before drawing any more cards
    if (dDown){
      #if the sum of the hand is 10 or 11, double the bet and only draw one card
      if(sum(p[[1]])==11 || sum(p[[1]])==10){
        #cat("doubled down: hand total",sum(p[[1]]),"\n")
        totalBet <- totalBet + 1
        p<-playerS4(deck[counter],F)
        counter <- counter + 1
      }
      #if split, need to check the other hand too
      if(split){
        if(sum(p2[[1]])==11 || sum(p2[[1]])==10){
          #cat("doubled down on split hand: hand total",sum(p2[[1]]),"\n")
          totalBet <- totalBet + 1
          p2<-playerS4Split(deck[counter],F)
          counter <- counter + 1
        }
      }
    } 
      #finish hands as usual
      #even if doubled down, with S1 will be >=11 so player hands will be standing
      #finish hand(s) of player
      if(!splitWithAce){
        #original hand
        while(!(p[[2]])){
          p<-playerS4(deck[counter],F)
          counter <- counter + 1
        }
        #if there is also a split hand
        if(split){
          while(!(p2[[2]])){
            p2<-playerS4Split(deck[counter],F)
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
      if(dDown){
          r<-checkForWinner(unlist(p[1]),unlist(d[1]),p2[[1]], doublingdown=TRUE)
          #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]),p2[[1]], doublingdown=TRUE)
      } else{
        r<-checkForWinner(unlist(p[1]),unlist(d[1]),p2[[1]])
        #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]),p2[[1]])
      }
      results <- results+r
    } else{
      if(dDown){
        r<-checkForWinner(unlist(p[1]),unlist(d[1]), doublingdown=T)
        #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]), doublingdown=T)
      } else {
        r<-checkForWinner(unlist(p[1]),unlist(d[1]))
        #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]))
      }
      results <- results+r
    }
    
    #deal first card of next game to player and dealer
    p<-playerS4(deck[counter],T)
    counter <- counter + 1
    d<-dealer(deck[counter],T)
    counter <- counter + 1
  }
  
  results[6] <- totalBet
  return(results)
}

#strategy = 5: base strategy
#strategy = 11: doubling down
#strategy = 17: splitting and doubling down
s5 <- function(results,deck,terminate, strategy){
  if(strategy==5){
    #play with split flag
    pws = F
    dDown = F
  } else if (strategy==11){
    pws=F
    dDown=T
  } else{
    pws=T
    dDown=T
  }
  
  totalBet<-0
  
  counter <- 1
  #deal first card to player and dealer
  p<-playerS5(deck[counter],T)
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
        p2<-playerS5Split(deck[counter],T)
        
        #check for aces
        if(names(deck[counter])=="A"){
          #set ace value to 11
          playerCards[1]<<-11
          playerCardsSplit[1]<<-11
          splitWithAce=T
        }
        counter <- counter + 1
        #deal second card to player hands and dealer
        p<-playerS5(deck[counter],F)
        counter <- counter + 1
        p2<-playerS5Split(deck[counter],F)
        counter <- counter + 1
        d<-dealer(deck[counter],F)
        counter <- counter + 1
        
      } else{
        #no split, play normally
        #deal second card to player and dealer
        p<-playerS5(deck[counter],F)
        counter <- counter + 1
        d<-dealer(deck[counter],F)
        counter <- counter + 1
      }
    } else{
      #not playing with splitting
      #deal second card to player and dealer
      p<-playerS5(deck[counter],F)
      counter <- counter + 1
      d<-dealer(deck[counter],F)
      counter <- counter + 1
    }
    #now have 2 cards in all hand(s)
    #if doubling down, check before drawing any more cards
    if (dDown){
      #if the sum of the hand is 10 or 11, double the bet and only draw one card
      if(sum(p[[1]])==11 || sum(p[[1]])==10){
        #cat("doubled down: hand total",sum(p[[1]]),"\n")
        totalBet <- totalBet + 1
        p<-playerS5(deck[counter],F)
        counter <- counter + 1
      }
      #if split, need to check the other hand too
      if(split){
        if(sum(p2[[1]])==11 || sum(p2[[1]])==10){
          #cat("doubled down on split hand: hand total",sum(p2[[1]]),"\n")
          totalBet <- totalBet + 1
          p2<-playerS5Split(deck[counter],F)
          counter <- counter + 1
        }
      }
    } 
      #finish hands as usual
      #even if doubled down, with S1 will be >=11 so player hands will be standing
      #finish hand(s) of player
      if(!splitWithAce){
        #original hand
        while(!(p[[2]])){
          p<-playerS5(deck[counter],F)
          counter <- counter + 1
        }
        #if there is also a split hand
        if(split){
          while(!(p2[[2]])){
            p2<-playerS5Split(deck[counter],F)
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
      if(dDown){
          r<-checkForWinner(unlist(p[1]),unlist(d[1]),p2[[1]], doublingdown=TRUE)
          #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]),p2[[1]], doublingdown=TRUE)
      } else{
        r<-checkForWinner(unlist(p[1]),unlist(d[1]),p2[[1]])
        #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]),p2[[1]])
      }
      results <- results+r
    } else{
      if(dDown){
        r<-checkForWinner(unlist(p[1]),unlist(d[1]), doublingdown=T)
        #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]), doublingdown=T)
      } else {
        r<-checkForWinner(unlist(p[1]),unlist(d[1]))
        #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]))
      }
      results <- results+r
    }
    
    #deal first card of next game to player and dealer
    p<-playerS5(deck[counter],T)
    counter <- counter + 1
    d<-dealer(deck[counter],T)
    counter <- counter + 1
  }
  
  results[6] <- totalBet
  return(results)
}

#strategy = 6: base strategy
#strategy = 12: doubling down
#strategy = 18: splitting and doubling down
s6 <- function(results, deck, terminate, strategy){
  #when dealer's up card is 3,4,5,6, S1
  #otherwise s2
  
  if(strategy==6){
    #play with split flag
    pws = F
    dDown = F
  } else if (strategy==12){
    pws=F
    dDown=T
  } else{
    pws=T
    dDown=T
  }
  
  totalBet<-0
  
  counter <- 1
  #deal first card to player and dealer
  p<-playerS6(deck[counter],T, T)
  counter <- counter + 1
  d<-dealer(deck[counter],T)
  counter <- counter + 1
  
  #flag for whether to use s1 or s2
  useS1 = F
  
  #go through the deck, last game is with the termination card
  while(counter<=terminate){
    totalBet <- totalBet +1
    #split hand flag
    split=F
    splitWithAce=F
    doubledDown = F
    doubledDownSplit = F
    #if playing with a split hand, check next card for possible split before dealing it
    if(pws){
      
      if(names(deck[counter])==names(p[[1]]) && p[[1]]!=5 && p[[1]]!=10){
        #split the deck
        split=T
        totalBet <- totalBet +1
        p2<-playerS6Split(deck[counter],T, F)
        
        #check for aces
        if(names(deck[counter])=="A"){
          #set ace value to 11
          playerCards[1]<<-11
          playerCardsSplit[1]<<-11
          splitWithAce=T
        }
        counter <- counter + 1
        
        #check dealer's up card and determine strategy to use
        if(d[[1]]==3|d[[1]]==4|d[[1]]==5|d[[1]]==6){
          #use s1
          useS1 = T
        } else {
          useS1 = F
        }
        
        #deal second card to player hands and dealer
        p<-playerS6(deck[counter],F, useS1)
        counter <- counter + 1
        p2<-playerS6Split(deck[counter],F, useS1)
        counter <- counter + 1
        d<-dealer(deck[counter],F)
        counter <- counter + 1
        
      } else{
        #check dealer's up card and determine strategy to use
        if(d[[1]]==3|d[[1]]==4|d[[1]]==5|d[[1]]==6){
          #use s1
          useS1 = T
        } else {
          useS1 = F
        }
        
        #no split, play normally
        #deal second card to player and dealer
        p<-playerS6(deck[counter],F, useS1)
        counter <- counter + 1
        d<-dealer(deck[counter],F)
        counter <- counter + 1
      }
    } else{
      #check dealer's up card and determine strategy to use
      if(d[[1]]==3|d[[1]]==4|d[[1]]==5|d[[1]]==6){
        #use s1
        useS1 = T
      } else {
        useS1 = F
      }
      
      #not playing with splitting
      #deal second card to player and dealer
      p<-playerS6(deck[counter],F, useS1)
      counter <- counter + 1
      d<-dealer(deck[counter],F)
      counter <- counter + 1
    }
    #now have 2 cards in all hand(s)
    #if doubling down, check before drawing any more cards
    if (dDown){
      #if the sum of the hand is 10 or 11, double the bet and only draw one card
      if(sum(p[[1]])==11 || sum(p[[1]])==10){
        cat("doubled down: hand total",sum(p[[1]]),"\n")
        totalBet <- totalBet + 1
        p<-playerS6(deck[counter],F, useS1)
        counter <- counter + 1
        doubledDown=T
      }
      #if split, need to check the other hand too
      if(split){
        if(sum(p2[[1]])==11 || sum(p2[[1]])==10){
          cat("doubled down on split hand: hand total",sum(p2[[1]]),"\n")
          totalBet <- totalBet + 1
          p2<-playerS6Split(deck[counter],F, useS1)
          counter <- counter + 1
          doubledDownSplit=T
        }
      }
    } 
    #finish hands, send boolean to determine which strategy in player to use
    #finish hand(s) of player
    if(!splitWithAce){
      #original hand
      if(!doubledDown){
        while(!(p[[2]])){
          p<-playerS6(deck[counter],F, useS1)
          counter <- counter + 1
        }
      }
      #if there is also a split hand
      if(split){
        if(!doubledDownSplit){
          while(!(p2[[2]])){
            p2<-playerS6Split(deck[counter],F, useS1)
            counter <- counter + 1
          }
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
      if(dDown){
        #r<-checkForWinner(unlist(p[1]),unlist(d[1]),p2[[1]], doublingdown=TRUE)
        r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]),p2[[1]], doublingdown=TRUE)
      } else{
        #r<-checkForWinner(unlist(p[1]),unlist(d[1]),p2[[1]])
        r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]),p2[[1]])
      }
      results <- results+r
    } else{
      if(dDown){
        r<-checkForWinner(unlist(p[1]),unlist(d[1]), doublingdown=T)
        #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]), doublingdown=T)
      } else {
        r<-checkForWinner(unlist(p[1]),unlist(d[1]))
        #r<-checkForWinnerVerbose(unlist(p[1]),unlist(d[1]))
      }
      results <- results+r
    }
    
    #deal first card of next game to player and dealer
    p<-playerS6(deck[counter],T, T)
    counter <- counter + 1
    d<-dealer(deck[counter],T)
    counter <- counter + 1
  }
  
  results[6] <- totalBet
  return(results)
}

# s4 <- function(results, deck, terminate, strategy) {
#   if(strategy==4) {
#     #play with split flag - don't have these implemented for S4 yet
#     pws = F
#     dDown = F
#   } else if (strategy==10) { #want to play S4 with split
#     pws = T
#     dDown = F
#   } else {#means playing with double down
#     pws = T
#     dDown = T
#   }

#   totalBet <- 0
#   counter <- 1
#   #deal first card to player and dealer
#   p <- playerS4(deck[counter], T)
#   counter <- counter + 1
#   dealer(deck[counter], T)
#   counter <- counter + 1

#   #iterate through deck, last game is with the termination card
#   while(counter <= terminate) {
#     totalBet <- totalBet + 1
#     #split the hand flags
#     split = F
#     splitWithAce = F

#     #if playing with a split hand, check next card for possible split before dealing
#     if(pws) {
#       if(names(deck[counter]) == names(p[[1]]) && p[[1]] != 5 && p[[1]] != 10) {
#         #now can split deck
#         split = T
#         totalBet <- totalBet + 1
#         p2 <-playerS4Split(deck[counter], T)

#         #check for aces
#         if(names(deck[counter]) == "A"){
#           #now set ace value to 11
#           playerCards[1] <<- 11
#           playerCardsSplit[1] <<- 11
#           splitWithAce = T
#         }
#         counter <- counter + 1
#         #now deal a second card to player and dealer
#         p <- playerS4(deck[counter], F)
#         counter <- counter + 1
#         p2 <- playerS4Split(deck[counter], F)
#         counter <- counter + 1
#         d <- dealer(deck[counter], F)
#         counter <- counter + 1
#       } else {
#         #there is no split and play continues as normal
#         #deal second card to player and dealer
#         p <- playerS4(deck[counter], F)
#         counter <- counter + 1
#         d <- dealer(deck[counter], F)
#         counter <- counter + 1
#       }
#     } else {
#       #not playing with a split
#       #deal second card
#       p <- playerS4(deck[counter], F)
#       counter <- counter + 1
#       d <- dealer(deck[counter], F)
#       counter <- counter + 1
#     }

#     #finish hand(s) of the player
#     if(!splitWithAce) {
#       #original hand
#       while(!(p[[2]])) {
#         p <- playerS4(deck[counter], F)
#         counter <- counter + 1
#       }
#       #if there is also a split hand
#       if(split) {
#         while(!(p2[[2]])) {
#           p2 <- playerS4Split(deck[counter], F)
#           counter <- counter + 1
#         }
#       }
#     }

#     #finish dealer hand
#     while(!d[[2]]) {
#       d <- dealer(deck[counter], F)
#       counter <- counter + 1
#     }

#     #get results of game
#     if(split) {
#       r <- checkForWinner(unlist(p[1]), unlist(d[1]), p2[[1]])
#       #r <- checkForWinnerVerbose(unlist(p[1]),unlist(d[1]),p2[[1]])
#       results <- results + r
#     } else {
#       r <- checkForWinner(unlist(p[1]), unlist(d[1]))
#       #r <- checkForWinnerVerbose(unlist(p[1]),unlist(d[1]))
#       results <- results + r
#     }

#     #deal first card of next game to player and dealer
#     p <- playerS4(deck[counter], T)
#     counter <- counter + 1
#     d <- dealer(deck[counter], T)
#     counter <- counter + 1
#   }

#   results[6] <- totalBet
#   return(results)
# }

# s5 <- function(results, deck, terminate, strategy) {
#   if(strategy==5) {
#     #play with split flag - don't have these implemented for S5 yet
#     pws = F
#     dDown = F
#   } else if (strategy==11) { #want to play S5 with split
#     pws = T
#     dDown = F
#   } else {#means playing with double down
#     pws = T
#     dDown = T
#   }

#   totalBet <- 0
#   counter <- 1
#   #deal first card to player and dealer
#   p <- playerS5(deck[counter], T)
#   counter <- counter + 1
#   dealer(deck[counter], T)
#   counter <- counter + 1

#   #iterate through deck, last game is with the termination card
#   while(counter <= terminate) {
#     totalBet <- totalBet + 1
#     #split the hand flags
#     split = F
#     splitWithAce = F

#     #if playing with a split hand, check next card for possible split before dealing
#     if(pws) {
#       if(names(deck[counter]) == names(p[[1]]) && p[[1]] != 5 && p[[1]] != 10) {
#         #now can split deck
#         split = T
#         totalBet <- totalBet + 1
#         p2 <-playerS5Split(deck[counter], T)

#         #check for aces
#         if(names(deck[counter]) == "A"){
#           #now set ace value to 11
#           playerCards[1] <<- 11
#           playerCardsSplit[1] <<- 11
#           splitWithAce = T
#         }
#         counter <- counter + 1
#         #now deal a second card to player and dealer
#         p <- playerS5(deck[counter], F)
#         counter <- counter + 1
#         p2 <- playerS5Split(deck[counter], F)
#         counter <- counter + 1
#         d <- dealer(deck[counter], F)
#         counter <- counter + 1
#       } else {
#         #there is no split and play continues as normal
#         #deal second card to player and dealer
#         p <- playerS5(deck[counter], F)
#         counter <- counter + 1
#         d <- dealer(deck[counter], F)
#         counter <- counter + 1
#       }
#     } else {
#       #not playing with a split
#       #deal second card
#       p <- playerS5(deck[counter], F)
#       counter <- counter + 1
#       d <- dealer(deck[counter], F)
#       counter <- counter + 1
#     }

#     #finish hand(s) of the player
#     if(!splitWithAce) {
#       #original hand
#       while(!(p[[2]])) {
#         p <- playerS5(deck[counter], F)
#         counter <- counter + 1
#       }
#       #if there is also a split hand
#       if(split) {
#         while(!(p2[[2]])) {
#           p2 <- playerS5Split(deck[counter], F)
#           counter <- counter + 1
#         }
#       }
#     }

#     #finish dealer hand
#     while(!d[[2]]) {
#       d <- dealer(deck[counter], F)
#       counter <- counter + 1
#     }

#     #get results of game
#     if(split) {
#       r <- checkForWinner(unlist(p[1]), unlist(d[1]), p2[[1]])
#       #r <- checkForWinnerVerbose(unlist(p[1]),unlist(d[1]),p2[[1]])
#       results <- results + r
#     } else {
#       r <- checkForWinner(unlist(p[1]), unlist(d[1]))
#       #r <- checkForWinnerVerbose(unlist(p[1]),unlist(d[1]))
#       results <- results + r
#     }

#     #deal first card of next game to player and dealer
#     p <- playerS5(deck[counter], T)
#     counter <- counter + 1
#     d <- dealer(deck[counter], T)
#     counter <- counter + 1
#   }

#   results[6] <- totalBet
#   return(results)
# }




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
  
  if(x==1 || x==7 || x==13){
    results <- s1(results,deck,terminate, x)
  } else if (x==2 || x==8 || x==14) {
    results <- s2(results,deck,terminate, x)
  } else if (x==3 || x==9 || x==15){
    results <- s3(results,deck,terminate, x)
  }else if (x==4 || x==10 || x==16) {
    results <- s4(results,deck,terminate, x)
  } else if (x==5 || x==11 || x==17) {
    results <- s5(results,deck,terminate, x)
  } else if (x==6 || x==12 || x==18){
    results <- s6(results, deck, terminate, x)
  }
  
  
  return(results)
}

simRun <- 1


cat("Running Strategy 1 \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(1),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 1 with Doubling Down \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(7),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 1 with Splitting and Doubling Down \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(13),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 2 \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(2),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 2 with Doubling Down \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(8),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 2 with Doubling Down & Splitting \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(14),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 3 \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(3),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 3 with Doubling Down \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(9),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 3 with Splitting and Doubling Down \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(15),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 4 \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(4),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 4 with Doubling Down \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(10),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 4 with Doubling Down & Splitting \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(16),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 5 \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(5),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 5 with Doubling Down \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(11),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 5 with Doubling Down & Splitting \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(17),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 6 \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(6),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 6 with Doubling Down \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(12),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")

cat("Running Strategy 6 with Splitting and Doubling Down \n")
finalResults <- rowSums(sapply(1:simRun,function(i) sapply(X=runSimulations(18),FUN="+")))
names(finalResults) <- c("BlackJack","OtherWin","Tie","Loss","Bust","TotalBet","AmtLeft")
finalResults
cat("\n")


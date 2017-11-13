# Shuffle 6 Decks of Cards
# Shuffle a given vector of cards and generate a termination index for blackjack.
# Takes a Vector of cards to shuffle
# Returns a list containing a shuffled version of x and a termination index (218-250).

shuffleCards <- function(x){
  #set.seed(6324)
  #shuffle cards and store in cards
  cards <- sample(x,length(x),replace=F)
  #pick termination number
  terminate <- trunc(runif(1,218,250))
  #return subset deck to use in session
  return(list(x,terminate))
}


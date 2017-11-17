# Simulate the Dealer's Play
# Create the dealer's hand, standing on a soft 17
# Takes a card to add to the hand, and a boolean to create a new hand
# Returns a list containing the dealer's hand, and boolean if the dealer is standing

dealer <- function(card,reset){
  #stand if >16, otherwise hit. Soft 17 stands.
  #create a new card vector (new hand)
  if (reset){
    dealerCards <<- vector()
  }
  
  dealerCards <<- append(dealerCards,card)
  #handle aces
  if(length(which(dealerCards==-1))==1){
    #1st ace in hand: valued 11
    #Additional aces: valued 1
    if(length(which(dealerCards==11))==1){
      dealerCards[which(dealerCards==-1)] <<- 1
    } else {
      dealerCards[which(dealerCards==-1)] <<- 11
    }
  } 
  #check if busted and have an ace valued 11 - change ace that was 11 to 1
  if(sum(dealerCards)>21 && length(which(dealerCards==11))==1){
    dealerCards[which(dealerCards==11)] <<- 1
  }
  return(list(dealerCards,sum(dealerCards)>16))
}
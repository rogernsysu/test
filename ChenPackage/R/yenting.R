#' Permutation test
#'
#' Take any two vector you want to do the permutation test
#' @param dataA  The first vector.
#' @param dataB  The second vector.
#' @param n  The times of simulation.
#' @param alpha  Significant level.
#' @return reject or not reject
#' @export
#'
#' @example
#' A <- c(24,43,58,67,61,44,67,49,59,52,62,50)
#' B <- c(42,43,65,26,33,41,19,54,42,20,17,60,37,42,55,28)
#' PermuTest(A,B,1000,0.05)
PermuTest <- function(data_A,data_B,n,alpha){
  result <- c()
  statistic <- mean(data_A)-mean(data_B)
  for (i in 1:n) {
    group <- c(data_A,data_B)
    A_set <- sample(group,size = length(data_A))
    B_set <- group[-A_set]
    s_mean <- mean(A_set)-mean(B_set)
    result[i] <- s_mean
  }
  P_value <- sum(result > statistic)/n
  if(P_value < alpha){
    print("reject")
  }else{
    print("not reject")
  }
}




#' Get a card(number) from poker.
#'
#' You will get a number from number 1 to 13
#' @return value
#' @export
#'
#' @example
#' card_get()
card_get <- function(n=1){
  poker <- c(rep(1:13))
  x <- sample(poker,size = n,prob = rep(1/13,13))
  x <- ifelse(x>10,0.5,x)
  return(x)
}





#' Get the card repeatedly
#'
#' You will get several card randomly until the total sum is larger than you setting in the beginning.
#' @param card  The first vector.
#' @param point The max total which you want stop to pick new one card
#' @return All of your card and whether you win the game or not
#' @export
#'
#' @example
#' first_card <- card_get
#' game(first_card,7)
game <- function(card,point){
  pick <- c(card)
  hand <- pick
  card_sum <- sum(hand)
  while (card_sum <= point) {
    pick <- c(pick,card_get())
    hand <- pick
    card_sum <- sum(hand)
    if(length(hand)==5){
      break
    }
  }
  if(card_sum > 10.5){
    return(c(hand,"You lose"))
  }else if(length(hand)==5 && card_sum <= 10.5){
    return(c(hand,"You win"))
  }else if(length(hand)==2 && card_sum == 10.5){
    return(c(hand,"You win"))
  }else{
    return(hand)
  }
}

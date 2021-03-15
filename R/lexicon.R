#' Lexicon
#'
#' Creates a lexicon for a vector of tokens, especially useful for \link{parse}
#'
#' @param tokens Vector of tokens
#' @param zipf if TRUE will obtain token cost from zipf distribution.
#' @export
lexicon <- function(tokens, zipf=TRUE){
  freq.list <- table(tokens)
  if(zipf){
    L <- log(c(1:length(freq.list))*log(length(tokens),2),2)
    names(L) <- names(sort(freq.list,decreasing=TRUE))
  } else {
    L <- -log(freq.list/sum(freq.list),2)
  }
  sort(L)
}

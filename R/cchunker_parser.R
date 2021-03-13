# parse
#
# lexicon must be a vector of costs with names as names of word/morph
#
#
# NOTE: this is a R implementation of the algorithm suggested by Generic Human
# at StackExchange for Python. Kudos to him!
# See: https://stackoverflow.com/questions/8870261/how-to-split-text-without-spaces-into-list-of-words/11642687#11642687

parse <- function(lexicon, str){
  # Assert lexicon is a vector with named elements
  if(is.null(names(lexicon))) stop('Lexicon must be a vector with named elements')

  # Maximum length of lexicon element
  maxword <- max(nchar(names(lexicon)))

  # Initialize cost vector
  cost <- c(0)

  # Best Match
  #
  # i is the position in the text string (str)
  #
  # Returns a vector c(match_cost, match_length)
  best.match <- function(i){
    candidates <- rev(cost[max(1,i-maxword):i]) #check if i+1 needed
    substrings <- sapply(c(1:length(candidates)),
                         function(x) substr(str, i-x+1,i))
    substring.costs <- lexicon[match(substrings, names(lexicon), nomatch=NA)]
    substring.costs[is.na(substring.costs)] <- Inf
    substring.costs <- substring.costs+candidates
    c(min(substring.costs), match(min(substring.costs),substring.costs))
  }

  # Fills cost vector
  for(i in c(1:nchar(str))){
    cost <- c(cost, best.match(i)[1])
  }

  # Backtrack and fill output
  out <- c()
  i <- nchar(str)
  while(i>1){
    bm <- best.match(i)
    # Assert cost of best match is the same cost of the cost vector
    if(bm[1]!=cost[i+1]) stop("Mismatch error!")
    out <- c(out, substr(str,i-bm[2]+1,i))
    i <- i-bm[2]
  }

  ## work on this
  print(rev(out))
}


dm_chunker <- function(text, zipfian=FALSE,rate=25,sep="[.,;?!\n]"){

  # Prepare text: everything to lowercase, split in fullstops, commas, semicolumns.
  text <- gsub("[ ]", '', text)
  text <- unlist(strsplit(tolower(text), sep))

  # Create alphabet in uniform distribution
  alphabet <- lexicon(unique(unlist(strsplit(text, ""))), zipf=FALSE)

  # Create the grammar:
  grammar <- list(
    alphabet=alphabet,
    alphabet.cost=c(),
    multigrams=c(),
    multigrams.cost=c()
  )

  # Calculates multigram bitlength
  length.multigram <- function(multigram) sum(alphabet[unlist(strsplit(multigram,""))])

  # Calculates grammar bitlength
  length.grammar <- function(g) sum(g$alphabet) + sum(g$multigrams)

  # First parse
  tokens <- c()
  for(t in text) {
    tokens <- c(tokens, parse(grammar$alphabet, t)$tokens)
  }
  grammar$alphabet.cost <- lexicon(tokens, zipf=FALSE)
  grammar$u <- sum(sapply(tokens, function(x) grammar$alphabet.cost[x]))
  grammar$parse <- tokens

  # First description length
  grammar$dl <- length.grammar(grammar) + grammar$u

  # Parsing step
  pstep <- function(g) {
    # join each pair of tokens in the text
    bigrams <- lexicon(paste(g$parse[-length(g$parse)],g$parse[-1],sep=""),zipf=FALSE)
    newtokens <- bigrams[1:rate]

    #n.newtokens <- ceiling(rate * length(bigrams))
    #newtokens <- if(n.newtokens > 0 ) bigrams[1:n.newtokens] else NULL


    g$attempt <- c(g$alphabet.cost, g$multigrams.cost,newtokens)

    # parse with attempt
    tokens <- c()
    for(t in text) {
      tokens <- c(tokens, parse(g$attempt, t)$tokens)

    }

    # new lexicon
    nlexicon <- lexicon(tokens,zipf=zipfian)
    #n.deltokens <- ceiling(rate*length(nlexicon))
    #nlexicon <- nlexicon[1:(length(nlexicon)-n.deltokens)]
    g$alphabet.cost[intersect(names(nlexicon), names(g$alphabet))] <- nlexicon[intersect(names(nlexicon), names(g$alphabet))]
    g$multigrams.cost <- nlexicon[setdiff(names(nlexicon), names(g$alphabet))]
    g$multigrams <- nchar(names(g$multigrams.cost)) * g$alphabet[1]
    names(g$multigrams) <- names(g$multigrams.cost)

    # parse with new lexicon
    tokens <- c()
    for(t in text) {
      tokens <- c(tokens, parse(c(g$alphabet.cost, g$multigrams.cost), t)$tokens)
    }

    full <- c(g$alphabet.cost, g$multigrams.cost)
    g$u <- sum(sapply(tokens, function(x) full[x]))
    g$parse <- tokens
    g$length <- sum(g$multigrams) + sum(g$alphabet)
    g$dl <- sum(g$multigrams) + sum(g$alphabet) + g$u

    return(g)
  }

  # 10 attempts

  gra <- grammar
  attempt <- vector("list",10)
  for(i in c(1:10)){
    dln <- gra$dl
    gra <- pstep(gra)
    attempt[[i]] <- gra
    cat(paste("\r Iteration ", i, "DLdiff: ", gra$dl-dln))
    if(gra$dl-dln >= 0) break()
  }
  attempt
}

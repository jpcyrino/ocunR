#' Vector of Morphs
#'
#' Requires a column from an Ocun Data dataframe, normally generated with ocunR::read_txt.
#'
#' @param ocun.data Ocun dataframe column
#' @param sentence.separators FALSE if not to consider sentence breaks, TRUE if otherwise
#' @export
morphs <- function(ocun.data, sentence.separators=FALSE){

  if(sentence.separators){
    ss <- rep("##",length(ocun.data))
    mm <- c('##',
             ocun.data %>%
               stringr::str_c(ss,sep=" ") %>%
               stringr::str_replace_all("\\s+","-") %>%
               stringr::str_split("-") %>%
               unlist()
    )
  } else {
    mm <- ocun.data %>%
      stringr::str_c(sep=" ") %>%
      stringr::str_replace_all("\\s+","-") %>%
      stringr::str_split("-") %>%
      unlist()
  }

  mm
}


#' Vector of Words
#'
#' Requires a column from an Ocun Data dataframe, normally generated with ocunR::read_txt.
#'
#' @param ocun.data Ocun dataframe column
#' @param sentence.separators FALSE if not to consider sentence breaks, TRUE if otherwise
#' @export
words <- function(ocun.data, sentence.separators=FALSE){

  if(sentence.separators){
    ss <- rep("##",length(ocun.data))
    mm <- c('##',
            ocun.data %>%
              stringr::str_c(ss,sep=" ") %>%
              stringr::str_replace_all("-","") %>%
              stringr::str_replace_all("\\s+","-") %>%
              stringr::str_split("-") %>%
              unlist()
    )
  } else {
    mm <- ocun.data %>%
      stringr::str_c(sep=" ") %>%
      stringr::str_replace_all("-","") %>%
      stringr::str_replace_all("\\s+","-") %>%
      stringr::str_split("-") %>%
      unlist()
  }

  mm
}

#' Dataframe of Bigrams
#'
#' Requires a column from an Ocun Data dataframe, normally generated with ocunR::read_txt.
#'
#' @param ocun.data Ocun dataframe column
#' @param morphs TRUE if considering unigrams as morphs, FALSE if considering unigrams as words
#' @param sentence.separators FALSE if not to consider sentence breaks, TRUE if otherwise
#' @export
bigrams <- function(ocun.data, morphs=TRUE, sentence.separators=FALSE){
  unigrams <- if(morphs) morphs(ocun.data, sentence.separators=TRUE) else words(ocun.data, sentence.separators=TRUE)
  bigrams <- data.frame(i = unigrams[-length(unigrams)],
                        j = unigrams[-1])
  #if sentence.separators is TRUE, remove all rows with separator symbol
  if(!sentence.separators){
    bigrams <- bigrams %>%
      dplyr::filter(i!='##',j!='##')
  }
  bigrams
}

#' Ajacency Matrix
#'
#' Creates an adjacency matrix from a bigram dataframe
#' (see \code{\link{bigrams}}). The matrix's rows are the nodes and the columns
#' are the neighbors.
#'
#' @param bigrams Bigram dataframe form ocunR::bigrams()
#' @param binary TRUE if binary, FALSE if frequency of bigram
#' @param left TRUE if left direction, FALSE if right direction
#' @return A matrix with the names of each morph/word as row/column names.
#' @export
adjacency_matrix <- function(bigrams, binary=TRUE, left=FALSE){
  colnames(bigrams) <- c("i__","j__")
  bigrams.list <- bigrams %>%
    dplyr::group_by(i__,j__) %>%
    dplyr::summarise(n=dplyr::n()) %>%
    dplyr::ungroup()

  if(binary) {
    bigrams.list <- bigrams.list %>%
      dplyr::mutate(n = dplyr::if_else(n==0, 0, 1))
  }

  if(left){
    pivot <- bigrams.list %>%
      tidyr::pivot_wider(names_from=i__, values_from=n, values_fill=0)
  } else {
    pivot <- bigrams.list %>%
      tidyr::pivot_wider(names_from=j__, values_from=n, values_fill=0)
  }

  adj.matrix <- as.matrix(pivot[,-1])
  rownames(adj.matrix) <- pivot$i__
  return(adj.matrix)
}

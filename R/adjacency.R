#' Ajacency Matrix
#'
#' @param bigrams Bigram dataframe form ocunR::bigrams()
#' @param binary TRUE if binary, FALSE if frequency of bigram
#' @param left TRUE if left direction, FALSE if right direction
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
    return(bigrams.list %>%
      tidyr::pivot_wider(names_from=i__, values_from=n, values_fill=0))
  } else {
    return(bigrams.list %>%
      tidyr::pivot_wider(names_from=j__, values_from=n, values_fill=0))
  }
}


#' Normalized Information Distance Matrix
#'
#' Note, this is really slow.
NID_matrix <- function(bigrams, left=FALSE){
  ad.mat <- adjacency_matrix(bigrams, binary=TRUE, left=left)
  mat <- as.matrix(ad.mat[,-1])
  nidm <- mat
  for(i in c(1:ncol(mat))){
    nidm[,i] <- apply(mat,1,aricode::NID,c2=mat[,i])
    print(paste("Column ", i))
  }
  rownames(nidm) <- ad.mat[,1]
  nidm
}

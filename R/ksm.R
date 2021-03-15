#' Kernel Similarity Matrix (KSM)
#'
#' Builds a kernel similarity matrix fom adjacency matrix
#' @param adj.mat the adjacency matrix (see \link{adjacency_matrix}).
#' @export
ksm <- function(adj.mat) {
  M <- cbind()
  print("Building Kernel Similarity Matrix, processing columns...")
  for(i in c(1:nrow(adj.mat))){
    M <- cbind(M, apply(adj.mat,1, function(x) x %*% adj.mat[i,]))
    cat("\r", paste(i, "of ", nrow(adj.mat)))
  }
  print("Done!")
  M
}

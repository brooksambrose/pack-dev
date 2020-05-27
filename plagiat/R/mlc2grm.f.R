#' Multilevel Corpus 2 Gram Matrix
#'
#' @param mlc
#' @param K
#' @param verbose
#' @param maxV
#' @param lev
#' @import stm
#'
#' @return
#' @export
#'
#' @examples
mlc2grm.f<-function(mlc,lev=c('doc','par','sen'),verbose =T){
  documents<-mlc[[lev[1]]]$spm
  vocab<-mlc$voc
  data<-mlc[[lev[1]]][[1]]
  N <- length(documents)

  if (verbose) cat("\t Calculating the gram matrix...\n")
  docs <- stm:::doc.to.ijv(documents)
  mat <- Matrix::sparseMatrix(docs$i, docs$j, x = docs$v)
  rm(docs)


  nd <- Matrix::rowSums(mat)
  mat <- mat[nd >= 2, ]
  nd <- nd[nd >= 2]
  divisor <- nd * (nd - 1)
  Q <- Matrix::crossprod(mat/sqrt(divisor)) - Matrix::Diagonal(x=Matrix::colSums(mat/divisor))
  Q <- as.matrix(Q)
  rownames(Q)<-mlc$voc
  Qsums <- rowSums(Q)
  #temp remove zero rows, can't divide by zero
  if (any(Qsums == 0)) {
    temp.remove <- which(Qsums == 0)
    Q <- Q[-temp.remove, -temp.remove]
    Qsums <- Qsums[-temp.remove]
  }
  Q <- Q/Qsums
  Q
}

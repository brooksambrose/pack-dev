# K<-5
# Qbar<-Q$doc
# basis <- c()
# rowSquaredSums <- rowSums(Qbar^2)
# for (i in 1:K) {
#   basis[i] <- which.max(rowSquaredSums)
#   maxval <- rowSquaredSums[basis[i]]
#   normalizer <- 1/sqrt(maxval)
#   Qbar[basis[i], ] <- Qbar[basis[i], ] * normalizer
#   innerproducts <- Qbar %*% Qbar[basis[i], ]
#   project <- as.numeric(innerproducts) %o% Qbar[basis[i],
#                                                 ]
#   project[basis, ] <- 0
#   Qbar <- Qbar - project
#   rowSquaredSums <- rowSums(Qbar^2)
#   rowSquaredSums[basis] <- 0
#   if (verbose)
#     cat(".")
# }

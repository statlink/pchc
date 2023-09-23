pcor <- function(R, indx, indy, indz, n) {
  if ( length(indz) == 1 ) {
    a1 <- R[indx, indy]   ;    a2 <- R[indx, indz]
    a3 <- R[indy, indz]
    r <- (a1 - a2 * a3) / sqrt( (1 - a3^2) * (1 - a2^2) )
  } else if ( length(indz) > 1 ) {
    rho <- try( solve( R[c(indx, indy, indz), c(indx, indy, indz)] ), silent = TRUE)
    if ( !identical( class(rho), "try-error" ) ) {
      r <-  - rho[1, 2] / sqrt(rho[1, 1] * rho[2, 2])
    } else r <- 0.99999
  }
  if ( abs(r) >= 1 ) r <- 0.99999
  z <- 0.5 * log( (1 + r) / (1 - r) ) * sqrt( n - sum(indz > 0) - 3 )
  pval <- log(2) + pt( abs(z), n - sum(indz > 0) - 3, lower.tail = FALSE, log.p = TRUE )
  res <- c(r, pval)
  names(res) <- c( "partial correlation", "logged p-value")
  res
}

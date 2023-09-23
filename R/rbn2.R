rbn2 <- function(n, G = NULL, p, nei, low = 0.1, up = 1) {

  if ( is.null(G) ) {
    prob <- nei/(p - 1)
    G <- matrix(0, p, p)
    qa <- rbinom( 0.5 * p * (p - 1), 1,  prob )
    G[upper.tri(G)] <- qa
  }
  nam <- colnames(G)
  if ( is.null(nam) )   nam <- paste("X", 1:p, sep = "")

  x <- matrix(0, n, p)
  x[, 1] <- Rfast::Rnorm(n)
   for (i in 2:p) {
    if ( sum( G[, i] != 0 ) == 0 ) {
      x[, i] <- Rfast::Rnorm(n)
    } else {
      id <- which(G[, i] == 1)
      wa <- x[, id, drop = FALSE]
      ub <- runif( dim(wa)[2] )
      b <- runif( dim(wa)[2], -up, -low) * (ub < 0.5) + runif( dim(wa)[2], low, up) * (ub > 0.5)
      x[, i] <- rnorm(n, wa %*% b, 1)
      x[, i] <- ( x[, i] - mean(x[, i]) ) / Rfast::Var(x[, i], std = TRUE)
    }
  }
  colnames(G) <- rownames(G) <- nam
  colnames(x) <- nam
  list(G = G, x = x)
}

rbn3 <- function(n, p, s, a = 0, m = NULL, G = NULL, seed = FALSE) {

  if ( is.null(G) ) { ## no adjacency matrix is given
    if ( s > 1 || s < 0 )  s <- 0.5
    if ( a > 1 || a < 0 )  a <- 0
    if ( seed )  set.seed(1234567)
    G <- matrix( 0, p, p )
    nu <- 0.5 * p * (p - 1)
    G[ lower.tri(G) ] <- rbinom(nu, 1, s)
    G[ G == 1 ] <- runif( sum(G), 0.1, 1 )
  } else {
    G <- G
    p <- ncol(G)
  }

  Ip <- diag(p)
  sigma <- solve( Ip - G )
  sigma <- tcrossprod( sigma )
  nout <- 0
  if ( seed )  set.seed(1234567)

  if (a > 0) {
    y <- Rfast::rmvnorm( n - nout, numeric(p), sigma)
    nout <- round( a * n )
    yout <- Rfast::rmvnorm(nout, m, sigma)
    x <- rbind(y, yout)
  } else  x <- Rfast::rmvnorm(n, numeric(p), sigma)

  G <- t( G )
  G[ G > 0 ] <- 2
  ind <- which( t(G) == 2 )
  G[ind] <- 3

  nam <- colnames(G)
  if ( is.null(nam) )   nam <- paste("X", 1:p, sep = "")
  colnames(x) <- nam
  colnames(G) <- rownames(G) <- nam
  list(nout = nout, G = G, x = x)
}

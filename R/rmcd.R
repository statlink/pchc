rmcd <-function(x, alpha = NULL) {

  dm <- dim(x)
  n <- dm[1]    ;   p <- dm[2]
  if ( is.null(alpha) )  alpha <- ceiling( 0.5 * (n + p + 1) )/n
  mod <- robustbase::covMcd( x, alpha = alpha )
  w <- sum( mod$mcd.wt )
  d1 <- w / (w - 1)^2 * mod$mah[mod$mcd.wt == 1]
  d0 <- w / (w + 1) * (w - p) / ( (w - 1) * p ) * mod$mah[mod$mcd.wt == 0]
  ep1 <- which( d1 > qbeta(0.975, 0.5 * p, 0.5 * (w - p - 1) ) )
  ep0 <- which( d0 > qf(0.975, p, w - p) )
  poia <- c( which(mod$mcd.wt == 1)[ep1],  which(mod$mcd.wt == 0)[ep0] )
  x <- x[-poia, ]
  list(poia = poia, x = x)
}

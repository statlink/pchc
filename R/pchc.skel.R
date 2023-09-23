pchc.skel <- function (x, method = "pearson", alpha = 0.05, robust = FALSE,
          ini.stat = NULL, R = NULL) {

  dm <- dim(x)
  n <- dm[1]   ;    d <- dm[2]

  if ( robust  &  method == "pearson" ) {
    mod <- robustbase::covMcd( x, alpha = ceiling( 0.5 * (n + d + 1) )/n )
    w <- sum( mod$mcd.wt )
    d1 <- w / (w - 1)^2 * mod$mah[mod$mcd.wt == 1]
    d0 <- w / (w + 1) * (w - d) / ( (w - 1) * d ) * mod$mah[mod$mcd.wt == 0]
    ep1 <- which( d1 > qbeta(0.975, 0.5 * d, 0.5 * (w - d - 1) ) )
    ep0 <- which( d0 > qf(0.975, d, w - d) )
    poia <- c( which(mod$mcd.wt == 1)[ep1],  which(mod$mcd.wt == 0)[ep0] )
    x <- x[-poia, ]
	  n <- dim(x)[1]
	  R <- cor(x)
    ini.stat <- 0.5 * log( (1 + R)/( (1 - R) ) ) * sqrt(n - 3)
  }

  if ( method == "cat"  &  !is.matrix(x) )  {
    for ( i in 1:dim(x)[2] )  x[, i] <- as.numeric(x[, i]) - 1
    x <- Rfast::data.frame.to_matrix(x, col.names = colnames(x) )
  }

  if ( method == "pearson"  &  is.null(ini.stat)  &  !is.null(R) ) {
    ini.stat <- 0.5 * log( (1 + R)/( (1 - R) ) ) * sqrt(n - 3)
  }

  mod <- Rfast::pc.skel(x, method, alpha, R = 1, stat = ini.stat)
}

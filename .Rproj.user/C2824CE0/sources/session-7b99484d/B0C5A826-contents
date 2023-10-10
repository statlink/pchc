dcor.mmhc.skel <- function(x, max_k = 3, alpha = 0.05, ini.pvalue = NULL, B = 999) {

  dm <- dim(x)
  n <- dm[1]   ;    d <- dm[2]

  G <- matrix(0, d, d)
  ntests <- 0
  nam <- colnames(x)
  if ( is.null(nam) )  nam <- paste("X", 1:d, sep = "")
  colnames(G) <- nam    ;   rownames(G) <- nam
  la <- log(alpha)

  oop <- options(warn = -1)
  on.exit( options(oop) )
  pvalue <- G

  runtime <- proc.time()

  ini.pvalue <- matrix(0, d, d)
  for ( i in 1:(d - 1) ) {
    for ( j in i:d )  {
      ini.pvalue[i, j] <- dcov::dcor.test(x[, i], x[, j], R = B, type = "U")$p.values
      ntests <- ntests + d * (d - 1) / 2
    }
  }
  ini.pvalue <- ini.pvalue + t(ini.pvalue)

  for (k in 1:d) {
    pval <- ini.pvalue[k, ]
    vars <- which(pval < la)
    if ( length(vars) > 0 ) {
      sela <- which.min(pval)
    } else  sela <- vars
    vars <- setdiff(vars, sela)

    while ( length(vars) > 0 ) {
      pval2 <- numeric(d)
      for ( i in 1:min( max_k, length(sela) ) ) {
        if ( length(sela) == 1 ) {
          cand <- matrix(sela, nrow = 1)
        } else  cand <- Rfast::comb_n(sort(sela), i)
        j <- 1
        while ( length(vars) > 0  &  j <= dim(cand)[2] ) {
          for (vim in vars)   pval2[vim] <- dcov::pdcor.test(x[, vim], x[, k], x[, cand[, j]], R = B, type = "U" )$p.values
          ## sp <- Rfast::g2Test(x, vim, k, cand, dc )
          ## pval2[vim] <- pchisq(sp$statistic, sp$df, lower.tail = FALSE, log.p = TRUE)
          ntests <- ntests + length(vars)
          pval[vars] <- pmax(pval[vars], pval2[vars])
          ide <- which(pval[vars] < la)
          vars <- vars[ide]
          j <- j + 1
        }  ## end  while ( length(vars) > 0  &  j <= dim(cand)[2] ) {
      }  ## end  for ( i in 2:max_k ) {
      sel <- which.min(pval[vars])
      sela <- c(sela, vars[sel] )
      vars <- setdiff(vars, vars[sel])
    } ## end  while ( length(vars) > 0 ) {

    G[k, sela] <- 1
    pvalue[k, ] <- pval
  }  ##  end   for (k in 1:d) {

  a <- which( G == 1  &  t(G) == 1 )
  G[ -a ] <- 0
  pvalue <- pmax( pvalue, t(pvalue) )

  runtime <- proc.time() - runtime

  list(ini.pvalue = ini.pvalue, pvalue = pvalue, runtime = runtime[3], n.tests = ntests, G = G)
}

#

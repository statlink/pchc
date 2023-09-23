fedhc.boot <- function(x, method = "pearson", alpha = 0.05, ini.stat = NULL,
                       R = NULL, restart = 10, score = "bic-g", blacklist = NULL,
                       whitelist = NULL, B = 200, ncores = 1) {

  mod <- pchc::fedhc(x, method = method, alpha = alpha, ini.stat = ini.stat, R = R, restart = restart,
                     score = score, blacklist = blacklist, whitelist = whitelist)
  dm <- dim(x)
  n <- dm[1]   ;     p <- dm[2]

  runtime <- proc.time()
  if ( ncores == 1 ) {
    Gboot <- matrix(0, p, p)
    for (i in 1:B) {
      id <- Rfast2::Sample.int(n, n, replace = TRUE)
      gb <- pchc::fedhc(x[id, ], method = method, alpha = alpha, restart = restart,
                        score = score, blacklist = blacklist, whitelist = whitelist)
      Gboot <- Gboot + pchc::bnmat(gb$dag)
    }  ## end for (i in 1:B)
  } else {
    cl <- parallel::makePSOCKcluster(ncores)
    doParallel::registerDoParallel(cl)
    Gboot <- foreach::foreach(i = 1:B, .combine = rbind,
                     .export = c("fedhc", "Sample.int"),
                     .packages = c("pchc", "Rfast2") ) %dopar% {
      id <- Rfast2::Sample.int(n, n, replace = TRUE)
      gb <- pchc::fedhc(x[id, ], method = method, alpha = alpha, restart = restart,
                        score = score, blacklist = blacklist, whitelist = whitelist)
      return( as.vector( pchc::bnmat(gb$dag) ) )
    }
    parallel::stopCluster(cl)
    Gboot <- Rfast::colsums(Gboot)
    Gboot <- matrix(Gboot, nrow = p, ncol = p)
  }
  runtime <- proc.time() - runtime

  list(mod = mod, Gboot = Gboot/B, runtime = runtime)
}

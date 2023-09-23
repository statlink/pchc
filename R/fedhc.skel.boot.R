fedhc.skel.boot <- function(x, method = "pearson", alpha = 0.05, B = 200) {
  G <- pchc::fedhc.skel(x = x, method = method, alpha = alpha)$G
  dm <- dim(x)
  n <- dm[1]   ;     p <- dm[2]

  runtime <- proc.time()
  Gboot <- matrix(0, p, p)
  for (i in 1:B) {
    id <- Rfast2::Sample.int(n, n, replace = TRUE)
    Gboot <- Gboot + pchc::fedhc.skel(x = x[id, ], method = method, alpha = alpha)$G
  }  ## end for (i in 1:B)
  runtime <- proc.time() - runtime

  list(G = G, Gboot = Gboot/B, runtime = runtime)
}

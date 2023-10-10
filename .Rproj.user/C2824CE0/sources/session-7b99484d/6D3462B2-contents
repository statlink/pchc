mmhc.skel.boot <- function(x, max_k = 3, method = "pearson", alpha = 0.05, B = 200) {
  G <- pchc::mmhc.skel(x = x, max_k = max_k, method = method, alpha = alpha)$G
  dm <- dim(x)
  n <- dm[1]   ;     p <- dm[2]

  runtime <- proc.time()
  Gboot <- matrix(0, p, p)
  for (i in 1:B) {
    id <- Rfast2::Sample.int(n, n, replace = TRUE)
    Gboot <- Gboot + pchc::mmhc.skel(x = x[id, ], max_k = max_k, method = method, alpha = alpha)$G
  }  ## end for (i in 1:B)
  runtime <- proc.time() - runtime

  list(G = G, Gboot = Gboot/B, runtime = runtime)
}

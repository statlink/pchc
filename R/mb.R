mb <- function(bn, node) {

  if ( identical( class(bn), "bn" ) ) {
    G <- bnlearn::amat(bn)
  } else G <- bn
  nama <- colnames(G)
  parents <- which(G[, node] == 1)
  children <- which(G[node, ] == 1)

  spouses <- NULL
  if ( length(children) > 0 ) {
    for ( i in 1:length(children) ) {
      spouses <- c( spouses, which( G[, children[i] ]  == 1 ) )
    }
  }

  spouses <- setdiff(spouses, c(node, parents, children))

  names( parents ) <- nama[parents]
  names( children ) <- nama[children]
  names( spouses ) <- nama[spouses]

  list( parents = parents, children = children, spouses= spouses, markov.blanket = c(parents, children, spouses) )

}

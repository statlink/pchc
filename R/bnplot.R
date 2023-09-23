bnplot <- function(dag, shape = "ellipse", main = NULL, sub = NULL, highlight = NULL) {
  bnlearn::graphviz.plot(x = dag, shape = shape, main = main, sub = sub, highlight = highlight)
}

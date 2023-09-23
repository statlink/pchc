cor2pcor <- function(R){
  a <- solve(R)
  d <- dim(R)[1]
  com <- sqrt( diag(a) )
  a <- a / outer(-com, com, "*")
  diag(a) <- 1
  a
}
big_read <- function(big_path, select, header = TRUE, sep = ",") {
  x <- bigstatsr::big_read(big_path, header = header, sep = sep, type = "double", select = select )
  bigstatsr::big_copy(x)
}

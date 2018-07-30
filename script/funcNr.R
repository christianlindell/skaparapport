funcNr <- function(x, decim=0) {
  x = as.numeric(x)
  format(round(x, decim), decimal.mark = "," , big.mark = " ", nsmall=decim, scientific = FALSE)
}
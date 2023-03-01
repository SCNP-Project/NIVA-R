fakeSpline <- function(x, y) {
  return(list(
    form = 'pp',
    breaks = x,
    coefs = c(sprintf("%sx4", length(x)-1)  , 'double'),
    pieces = length(x)-1,
    order= 4,
    dim = 1
  ))
}
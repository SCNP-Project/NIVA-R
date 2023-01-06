differential <- function(y,s,r) {
  require('pspline')
  require('pracma')

  n <- length(y)
  t <- (1:n-1)/s
  f <- smooth.Pspline(t, y, norder = 2, method = 1, spar=(1-r)/r)
  y2 <- predict(f, t)
  d1 <- gradient(as.vector(y2))
  d2 <- gradient(as.vector(d1))

  listReturn <- list(y2 = y2, d1 = d1, d2 = d2)

  return(listReturn)
}


# differential <- function(y, s, r) {
#   n <- length(y)
#   # t <- (0:1:(n-1))/s
#   t <- (1:n-1)/s
#   f <- smooth.spline(t, y, spar = r)
#   y2 <- predict(f, t)
#   d1 <- diff(y2)
#   d2 <- diff(d1)
#   return(list(y2 = y2, d1 = d1, d2 = d2))
# }

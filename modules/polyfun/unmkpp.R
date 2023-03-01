unmkpp <- function(pp) {
  d <- pp$dim
  l <- pp$pieces
  breaks <- pp$breaks
  coefs <- pp$coefs
  k <- pp$order
  
  return(list(
    d = d,
    l = l,
    breaks = breaks,
    coefs = coefs,
    k = k
  ))
}
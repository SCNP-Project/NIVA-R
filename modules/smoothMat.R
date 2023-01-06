smoothMat <- function(x, y, span) {
  
  # moving average of the data.
  
  ynan <- is.nan(y)
  span <- floor(span)
  n <- length(y)
  span <- min(span, n)
  width <- span - 1 + span %% 2 # force it to be odd
  xreps <- any(diff(x) == 0)
  if (width == 1 && !xreps && !any(ynan)) {
    return(y)
  }
  if (!xreps && !any(ynan)) {
    # simplest method for most common case
    c <- filter(rep(1/width, width), 1, y)
    cbegin <- cumsum(y[1:(width - 2)])
    cbegin <- cbegin[seq(1, (width - 2), by = 2)] / seq(1, (width - 2), by = 2)
    cend <- cumsum(y[n:1:(n - width + 3)])
    cend <- cend[seq(end, 1, by = -2)] / seq((width - 2), by = -2, 1)
    c <- c(cbegin, c[width:end], cend)
  } 
 
  return(c)
}
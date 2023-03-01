resampleQ <- function(x, p, q, i=1) {
  xIn <- x
  dimIn <- c(1, 2)
  cat(paste0("resampleQ => ", "p = ", p, "; q = ", q, "; loop = ", i, "; \n"))
  yUniv <- uniformResampleQ(xIn, dimIn, x, p, q)
  return(yUniv)
}

# ================================================================================================

uniformResampleQ <- function(x, dimIn, xTrue, p, q) {
  # Parse and validate variables.
  N <- 10
  bta <- 5
  
  # reduce to lowest terms
  reduced <- reduce_to_lowest_terms(p, q)
  p <- reduced$p
  q <- reduced$q
  
  if (p == 1 & q == 1) {
    # cat(paste0("This Working = ", "\n"))
    # yTmp <- x
    # if (length(x) == length(xTrue)) {
    #   # y <- array(yTmp, dim = dimTrue)
    #   # y <- array(yTmp, dim = rev(dim(xTrue)))
    #   # y <- aperm(yTmp, dimIn)
    #   # y <- array(y, dim = dim(xTrue))
    #   y = reshape(yTmp,size(xTrue));
    # } else {
    #   y <- array(yTmp, dim = dim(x))
    #   y <- aperm(y, perm = dimIn)
    # }
    # hout <- 1
    return(x)
  }

  Lx <- length(x)

  out <- createFilterQ(x, p, q, N, bta)
  h <- out$h
  hout <- out$ht
  delay <- out$delay

  Ly <- ceiling(Lx * p / q)
  syTrue <- dim(xTrue)
  syTrue[2] <- Ly

  yVec <- upfirdnQ(x, h, p, q)
  indV <- (delay-1) +( 1:Ly)

  yV <- yVec[indV]
  
  return(yV)
}

# =========================================================================

reduce_to_lowest_terms <- function(p, q) {
  # Find the greatest common divisor of p and q using the Euclidean algorithm
  a <- max(p, q)
  b <- min(p, q)
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  gcd <- a
  
  # Divide p and q by the gcd to reduce the fraction to lowest terms
  p <- p / gcd
  q <- q / gcd
  
  return(list(p = p, q = q))
}

# =========================================================================

createFilterQ <- function(x, p, q, n, bta) {
  prototype <- as.numeric(x[numeric(0)])
  
  pqmax <- max(p, q)
  
  fc <- 1/(2*pqmax)
  L <- floor(2*n[1]*pqmax + 1)
  
  h <- firlsQ(L-1, c(0, 2*fc, 2*fc, 1), c(1, 1, 0, 0)) * kaiser(L, bta)
  # cat(paste0("h = ", h, "\n"))
  
  h <- p*h/sum(h)
  # cat(paste0("h = ", h, "\n"))
  
  
  Lhalf <- floor((L-1)/2)
  
  nZeroBegin <- floor(q - (Lhalf %% q))
  z <- rep(0, nZeroBegin)
  
  h <- c(z, h)
  Lhalf <- Lhalf + nZeroBegin
  
  delay <- floor(ceiling(Lhalf)/q)
  
  nZeroEnd <- 0
  
  h <- c(h, rep(0, nZeroEnd))
  
  ht <- h[(nZeroBegin+1):(length(h)-nZeroEnd)]
  
  
  return(list(h=h, ht=ht, delay=delay))
}

# =========================================================================

kaiser <- function(n, beta)  { 
  
  if ( !(length(n) == 1 && (n == round(n)) && (n > 0))) 
    stop("kaiser:  n has to be a positive integer")
  
  if ( !(length(beta) == 1 && (beta == as.double(beta))))
    stop("kaiser:  beta has to be a real scalar")
  
  if (n == 1)
    w = 1
  else {
    m = n - 1
    k = 0:m
    k = 2 * beta / m * sqrt (k * (m - k))
    w = besselI(k, 0) / besselI(beta, 0)
  }
  w
}

# =========================================================================


# Fix Filter
firlsQ <- function(N, freq, amp) {
  weight <- rep(1, floor(length(freq)/2))
  
  N <- N + 1
  F <- freq/2
  A <- amp
  wt <- abs(sqrt(weight))
  L <- (N-1)/2
  m <- 0:L
  k <- m[-1]
  b0 <- 0
  b <- numeric(length(k))
  
  for (s in seq(1, length(F), by=2)) {
    m_s <- (A[s+1] - A[s]) / (F[s+1] - F[s])
    b1 <- A[s] - m_s * F[s]
    b0 <- b0 + (b1 * (F[s+1] - F[s]) + m_s/2 * (F[s+1]^2 - F[s]^2)) * (wt[(s+1)/2]^2)
    b <- b + (m_s/(4*pi^2) * (cos(2*pi*k*F[s+1]) - cos(2*pi*k*F[s]))/(k^2)) * (wt[(s+1)/2]^2)
    b <- b + (F[s+1] * (m_s * F[s+1] + b1) * sincQ(2*k*F[s+1]) - F[s] * (m_s * F[s] + b1) * sincQ(2*k*F[s])) * (wt[(s+1)/2]^2)
  }
  b <- c(b0, b)
  a <- (wt[1]^2) * 4 * b
  a[1] <- a[1]/2
  
  h <- c(a[(L+1):2]/2, a[1], a[2:(L+1)]/2)
  # cat(paste0("h = ", h, "\n"))
  return(h)
}


# =========================================================================

sincQ <- function(x) {
  ifelse(x == 0, 1, sin(pi*x)/(pi*x))
}

# =========================================================================

upfirdnQ <- function(x, h, p, q) {
  # Upsample x by inserting p-1 zeros between samples
  upsampled_x <- rep(0, length(x) * p)
  upsampled_x[seq(1, length(upsampled_x), p)] <- x
  
  # Filter the upsampled x with h
  filtered_x <- convolve(upsampled_x, h, type = "open")
  
  # Downsample the filtered signal by selecting every qth sample
  y <- filtered_x[seq(1, length(filtered_x), q)]
  
  return(y)
}


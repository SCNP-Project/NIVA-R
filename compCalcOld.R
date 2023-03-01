compCalcOld <- function(comp,sys,dia,height) {
  Psis <- sys
  Pdia <- dia
  subjectHeight <- height
  s <- 1000
  R <- 1.125
  r <- 0.9999;
  
  Pmean <- Pdia + (Psis - Pdia)/3
  u <- comp[19490:25320]
  
  
  data1 <- comp
  
  differential_data <- differential(u, s, r)
  y <- differential_data$y2
  
  # ddata1 <- differential_data$d1
  # d2data1 <- differential_data$d2
  
  Loc <- peakFinder(y);
  
  # Mencari Lembah
  y2 <- y
  yy <- c()
  for (c in 1:length(y2)) {
    y2low <- y2[c] - 2*y2[c]
    yy <- c(yy, y2low)
  }
  yy2 <- matrix(yy, ncol = 1)
  Loc2 = peakFinder(yy2)
  
  # Menentukan titik-titik yang akan digunakan untuk membuat spline UP
  xup <- Loc
  yup <- y[Loc]
  
  # Menentukan rentang nilai yang akan digunakan untuk menghitung spline
  xx <- 1:length(y)
  xx <- as.vector(xx)
  
  
  idxmaxdata1 <- which(data1 == max(data1))
  startDia <- which(d2data1 == max(d2data1[idxmaxdata1:idxmaxdata1 + floor(length(data1)/3)]))
  
  # Fix foot to foot => use stright line between first and last data
  ndata <- length(data1)
  m <- (data1[ndata] - data1[1]) / (ndata - 1)
  Pdata <- data1 - (data1[1] + m * (1:ndata-1))
  
  # Scale the waveform in between Sistolic and Diastolic Pressure
  Pmax <- max(Pdata)
  scl <- (Psis - Pdia) / Pmax
  Pdata <- Pdia + scl * Pdata
  
  # Pick diastolic wave, start from ds
  y <- Pdata[startDia:ndata]
  x <- c(1 : length(y)-1) / s
  
  
  # Set fit fariable
  fo <- y ~ a1 * exp(-a2 * x) + a3 * exp(-a4 * x) * cos(a5 * x + a6)
  fun <- function(a) {
    a[1] * exp(-a[2] * x) + a[3] * exp(-a[4] * x) * cos(a[5] * x + a[6])
  }
  
  startA1 <- 1
  startA2 <-  1 / 1.125
  startA3 <- 1
  startA4 <-  1 / (2 * 1.125 * 0.1)
  startA5 <- 15
  startA6 <- 0
  
  lowerA1 <- 0
  lowerA2 <- 0
  lowerA3 <- 0
  lowerA4 <- 0
  lowerA5 <- 0
  lowerA6 <- -50
  
  upperA1 <- 200
  upperA2 <- 10
  upperA3 <- 100
  upperA4 <- 100
  upperA5 <- 50
  upperA6 <- 50
  
  start <- c(a1 = startA1, a2 = startA2, a3 = startA3, a4 = startA4, a5 = startA5, a6 = startA6)
  lower <- c(a1 = lowerA1, a2 = lowerA2, a3 = lowerA3, a4 = lowerA4, a5 = lowerA5, a6 = lowerA6)
  upper <- c(a1 = upperA1, a2 = upperA2, a3 = upperA3, a4 = upperA4, a5 = upperA5, a6 = upperA6)
  
  require(nlsr)
  f <- nlxb(fo, data = data.frame(x,y), start = start, lower = lower, upper = upper)
  f <- f$coefficients
  
  a1 <- f[1];
  a2 <- f[2];
  a3 <- f[3];
  a4 <- f[4];
  a5 <- f[5];
  a6 <- f[6];
  
  C1 <- 2*a4*((a2+a4)^2+a5^2)/(R*a2*(2*a4+a2)*(a4^2+a5^2))
  C2 <- 1/(R*(2*a4+a2))
  
  listReturn <- list(
    C1 = C1,
    C2 = C2
  )
  
  return(listReturn)
  
}

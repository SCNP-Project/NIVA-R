compCalc <- function(comp,sys,dia,height) {
  Psis <- sys
  Pdia <- dia
  subjectHeight <- height
  s <- 1000
  R <- 1.125
  r <- 0.9999;
  
  data1 <-comp
  
  differential_data <- differential(data1, s, r)
  y2 <- differential_data$y2
  ddata1 <- differential_data$d1
  d2data1 <- differential_data$d2

  idxmaxdata1 <- which(data1 == max(data1))
  # startDia <- which(d2data1 == max(d2data1[idxmaxdata1:idxmaxdata1 + floor(length(data1)/3)]))
  startDia <- which.max(d2data1[idxmaxdata1:(idxmaxdata1 + floor(length(data1)/3))]) + idxmaxdata1 - 1
  
  # Fix foot to foot => use stright line between first and last data
  ndata <- length(data1)
  m <- (data1[ndata] - data1[1]) / (ndata - 1)
  # Pdata <- data1 - (data1[1] + m * (1:ndata-1))
  Pdata <- data1 - (data1[1] + m * (0:(ndata-1)))
  
  # Scale the waveform in between Sistolic and Diastolic Pressure
  Pmax <- max(Pdata)
  scl <- (Psis - Pdia) / Pmax
  Pdata <- Pdia + scl * Pdata
  
  # Pick diastolic wave, start from ds
  y <- Pdata[startDia:ndata]
  x <- c(1 : length(y)-1) / s
  
  test <- y
  
  # 11111111
  #===========================================================================
  
  # a1 <- 1
  # a2 <-  1/R
  # a3 <- 1
  # a4 <-  1/(2*R*0.1)
  # a5 <- 15
  # a6 <- 0
  # 
  # o <- list(Method = "NonlinearLeastSquares",
  #           # Startpoint = c(1, 1/R, 1, 1/(2*R*0.1), 15, 0),
  #           Startpoint = c(a1, a2, a3, a4, a5, a6),
  #           Lower = c(0, 0, 0, 0, 0, -50),
  #           Upper = c(200, 10, 100, 100, 50, 50))
  # g <- y ~ a1*exp(-a2*t)+a3*exp(-a4*t)*cos(a5*t+a6)
  # f <- nls(g, data = data.frame(x = x, y = y), start = o)
  
  
  # a1 <- coef(f)[1]
  # a2 <- coef(f)[2]
  # a3 <- coef(f)[3]
  # a4 <- coef(f)[4]
  # a5 <- coef(f)[5]
  # a6 <- coef(f)[6]
  
  
  # 2 ========== Working
  #===========================================================================
  
  # fo <- y ~ a1 * exp(-a2 * x) + a3 * exp(-a4 * x) * cos(a5 * x + a6)
  # 
  # start <- c(a1 = 1, a2 = 1/R, a3 = 1, a4 = 1/(2*R*0.1), a5 = 15, a6 = 0)
  # lower <- c(0, 0, 0, 0, 0, -50)
  # upper <- c(200, 10, 100, 100, 50, 50)
  # 
  # require(nlsr)
  # f <- nlxb(fo, data = data.frame(x,y), start = start, lower = lower, upper = upper)
  # f <- f$coefficients
  # 
  # a1 <- f[1];
  # a2 <- f[2];
  # a3 <- f[3];
  # a4 <- f[4];
  # a5 <- f[5];
  # a6 <- f[6];

  

  # 333333333333333
  #===========================================================================

  # library(nlme)
  # g <- nls(y ~ a1 * exp(-a2 * t) + a3 * exp(-a4 * t) * cos(a5 * t + a6),
  #          data = data.frame(t = x, y = y),
  #          start = c(a1 = 1, a2 = 1/R, a3 = 1, a4 = 1/(2*R*0.1), a5 = 15, a6 = 0),
  #          lower = c(0, 0, 0, 0, 0, -50),
  #          upper = c(200, 10, 100, 100, 50, 50))
  # 
  # # g <- nls(y ~ a1 * exp(-a2 * t) + a3 * exp(-a4 * t) * cos(a5 * t + a6),
  # #          data = data.frame(t = x, y = y),
  # #          start = c(a1 = 1, a2 = 1/R, a3 = 1, a4 = 1/(2*R*0.1), a5 = 15, a6 = 0))
  # 
  # 
  # yf <- predict(g)
  # 
  # 
  # # calculate compliance
  # a1 <- coef(g)["a1"]
  # a2 <- coef(g)["a2"]
  # a3 <- coef(g)["a3"]
  # a4 <- coef(g)["a4"]
  # a5 <- coef(g)["a5"]
  # a6 <- coef(g)["a6"]
  # C1 <- 2 * a4 * ((a2 + a4)^2 + a5^2) / (R * a2 * (2 * a4 + a2) * (a4^2 + a5^2))
  # C2 <- 1 / (R * (2 * a4 + a2))
  

  
  
  # 4444444444
  
  # library(nlme)
  # o <- nls.control(maxiter = 100, warnOnly = TRUE, minFactor = 1/1024)
  # g <- nls(y ~ a1*exp(-a2*t) + a3*exp(-a4*t)*cos(a5*t+a6),
  #          data = data.frame(x,y),
  #          start = list(a1 = 1, a2 = 1/R, a3 = 1, a4 = 1/(2*R*0.1), a5 = 15, a6 = 0),
  #          lower = list(a1 = 0, a2 = 0, a3 = 0, a4 = 0, a5 = 0, a6 = -50),
  #          upper = list(a1 = 200, a2 = 10, a3 = 100, a4 = 100, a5 = 50, a6 = 50),
  #          control = o)
  # yf <- predict(g)
  # 
  # # calculate compliance
  # a1 <- coef(g)[1]
  # a2 <- coef(g)[2]
  # a3 <- coef(g)[3]
  # a4 <- coef(g)[4]
  # a5 <- coef(g)[5]
  # a6 <- coef(g)[6]
  # C1 <- 2*a4*((a2+a4)^2+a5^2)/(R*a2*(2*a4+a2)*(a4^2+a5^2))
  # C2 <- 1/(R*(2*a4+a2))
  
  
  # 55555555
  # library(nlsr)
  # o <- list(
  #   algorithm = "port",
  #   start = c(a1 = 1, a2 = 1/R, a3 = 1, a4 = 1/(2*R*0.1), a5 = 15, a6 = 0),
  #   lower = c(0, 0, 0, 0, 0, -50),
  #   upper = c(200, 10, 100, 100, 50, 50)
  # )
  # g <- nlsr(y ~ a1*exp(-a2*t) + a3*exp(-a4*t)*cos(a5*t+a6),
  #           start = o$start,
  #           algorithm = o$algorithm,
  #           lower = o$lower,
  #           upper = o$upper,
  #           data = data.frame(t = x, y = y))
  # f <- summary(g)
  # yf <- predict(g)
  # 
  # # calculate compliance
  # a1 <- f$parameters[1]
  # a2 <- f$parameters[2]
  # a3 <- f$parameters[3]
  # a4 <- f$parameters[4]
  # a5 <- f$parameters[5]
  # a6 <- f$parameters[6]
  # C1 <- 2*a4*((a2+a4)^2+a5^2)/(R*a2*(2*a4+a2)*(a4^2+a5^2))
  # C2 <- 1/(R*(2*a4+a2))
  
  
  # 666666
  # Define the model
  # library(minpack.lm)
  # model <- function(t, a1, a2, a3, a4, a5, a6) {
  #   a1 * exp(-a2 * t) + a3 * exp(-a4 * t) * cos(a5 * t + a6)
  # }
  # 
  # # Define the starting values, lower and upper bounds
  # start <- c(a1 = 1, a2 = 1/R, a3 = 1, a4 = 1/(2*R*0.1), a5 = 15, a6 = 0)
  # lower <- c(0, 0, 0, 0, 0, -50)
  # upper <- c(200, 10, 100, 100, 50, 50)
  # 
  # # Fit the model
  # fit <- nlsLM(y ~ model(t, a1, a2, a3, a4, a5, a6),
  #              start = start,
  #              lower = lower,
  #              upper = upper,
  #              data = data.frame(t = x, y = y))
  # 
  # # Get the parameter estimates
  # a1 <- coef(fit)["a1"]
  # a2 <- coef(fit)["a2"]
  # a3 <- coef(fit)["a3"]
  # a4 <- coef(fit)["a4"]
  # a5 <- coef(fit)["a5"]
  # a6 <- coef(fit)["a6"]
  # 
  # # Calculate C1 and C2
  # C1 <- 2 * a4 * ((a2 + a4)^2 + a5^2) / (R * a2 * (2 * a4 + a2) * (a4^2 + a5^2))
  # C2 <- 1 / (R * (2 * a4 + a2))
  
  
  # 7 ========== Working
  library(minpack.lm)
  require(nlsr)
  

      fo <- y ~ a1 * exp(-a2 * x) + a3 * exp(-a4 * x) * cos(a5 * x + a6)
      
      start <- c(a1 = 1, a2 = 1/R, a3 = 1, a4 = 1/(2*R*0.1), a5 = 15, a6 = 0)
      lower <- c(0, 0, 0, 0, 0, -50)
      upper <- c(200, 10, 100, 100, 50, 50)

  
  tryCatch({
    
    try({
      
      g <- nlsLM(fo, data = data.frame(x,y), start = start, lower = lower, upper = upper)
      
      # calculate compliance
      a1 <- coef(g)["a1"]
      a2 <- coef(g)["a2"]
      a3 <- coef(g)["a3"]
      a4 <- coef(g)["a4"]
      a5 <- coef(g)["a5"]
      a6 <- coef(g)["a6"]
      
   
     
    })
    
    on.error <- try({
      message("Don't Worry - Program Not Kill")
      f <- nlxb(fo, data = data.frame(x,y), start = start, lower = lower, upper = upper)
      f <- f$coefficients
      
      a1 <- f[1];
      a2 <- f[2];
      a3 <- f[3];
      a4 <- f[4];
      a5 <- f[5];
      a6 <- f[6];
    
    }, silent = TRUE)
    
  },
  error = function(e) {
    message("An error occurred: ", e)
  }
  )
  
  
  c <- 2 * a4 * ((a2 + a4)^2 + a5^2) / (R * a2 * (2 * a4 + a2) * (a4^2 + a5^2))
  cc <- 1 / (R * (2 * a4 + a2))
  
  C1 <- max(cc, c)
  C2 <- min(cc, c)
  
  listReturn <- list(
    C1 = C1,
    C2 = abs(C2),
    a1 = a1,
    a2 = a2,
    a3 = a3,
    a4 = a4,
    a5 = a5,
    a6 = a6
  )

  return(listReturn)
  
}

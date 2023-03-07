waveselect <- function(comp) {
  
  library(signal)
  
  # smoothing by fitting using smoothing spline
  s <- 1000
  r <- 0.9999
  
  scomp <- smoothTest2(comp,2000)
  comp <- comp - scomp
  
  # smoothing data
  selection <- c()
  florLength <- floor(length(comp)/10000)-1
  for (b in 1:florLength) {
    u <- comp[(10000*b-300):(10000*(b+1)+300)]
    # print(u)
    
    differential_u <- differential(u, s, r)
    y <- differential_u$y2
    # n <- length(u)
    # t <- (0:(n-1))/s
    # f <- smooth.spline(t, u, spar = r)
    # y <- predict(f, t)
    if (b == 1) selection <- y[300:10299]
    else selection <- c(selection, y[300:10299])
  }
  
  # Mencari Puncak
  y <- matrix(selection, ncol = 1)
  Loc = peakFinder(y)
  
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
  
  library(splines)
  
  # Membuat spline dari titik-titik yang diberikan
  # ppup <- spline(xup, yup)
  # ppup <- fakeSpline(xup,yup)
  # yyup <- predict(ppup, x = xx)
  # yyup <- v(ppup, xx)
  
  yyup <- cubicspline(xup, yup, xx)
  
  
  # Menentukan titik-titik yang akan digunakan untuk membuat spline DOWN
  xdown <- Loc2
  ydown <- y[Loc2]

  yydown <- cubicspline(xdown, ydown, xx)
  
  # testYYDOWN <- yydown ## Test
  
  
  yr <- numeric(length(y))
  for (i in 1:length(y)) {
    yr[i] <- (y[i] - yydown[i]) / (yyup[i] - yydown[i])
  }
  
  y <- yr[Loc2[2]:(Loc2[length(Loc2)-1])] # y
  
  # Mencari Puncak
  Loc = peakFinder(y)
  
  # Mencari Lembah
  y2 <- y
  yy <- c()
  for (c in 1:length(y2)) {
    y2low <- y2[c] - 2*y2[c]
    yy <- c(yy, y2low)
  }
  yy2 <- matrix(yy, ncol = 1)
  Loc2 = peakFinder(yy2)
  
  xup <- Loc
  yup <- y[Loc]
  
  xx <- 1:length(y)
  xx <- as.vector(xx)
  
  yyup <- cubicspline(xup, yup, xx)
  
  xdown <- Loc2
  ydown <- y[Loc2]
  
  yydown <- cubicspline(xdown, ydown, xx)
  
  testYYDOWN <- yydown
  
  yr <- numeric(length(y))
  for (i in 1:length(y)) {
    yr[i] <- (y[i] - yydown[i]) / (yyup[i] - yydown[i])
  }
  
  yr <- yr[Loc2[2]:(Loc2[length(Loc2)-1])] # yr
  
  # Mencari Lembah
  y2 <- yr
  yy <- c()
  for (c in 1:length(y2)) {
    y2low <- y2[c] - 2*y2[c]
    yy <- c(yy, y2low)
  }
  yy2 <- matrix(yy, ncol = 1)
  
  feet = peakFinder(yy2)
  
  lf <- diff(feet)
  rf <- round(mean(lf),0)
  ysum <- rep(0, rf)
  
  # testYseum <- ysum

  ysTest <- yr[feet[1]:feet[1 + 1]] # COBA
  ysTest <- resample(ysTest, rf, length(ysTest))  # COBA

  plot(ysTest, type = "l", xlab = "Index", ylab = "Value", main = "Feet and Average Full Selection")
  
  
  
  # for (i in 1:(length(feet) - 1)) {
  #   ys <- yr[feet[i]:feet[i + 1]]
  #   ys <- resampleQ(ys, rf, length(ys), i)
  #   ysum <- ysum + ys
  #   points(ys, col = "green", pch = ".")
  # }
  # 
  # selection <- ysum / (length(feet) - 1)
  # lines(selection, col = "red")
  # 
  # ysTest2 <- yr[feet[1]:feet[1 + 1]]
  # ysTest2 <- resampleQ(ysTest2, rf, length(ysTest2))
  # 
  # cat(paste0("Test Loop = ", ysTest == ysTest2, "\n"))
  # 
  # # calculate different
  # ydiff <- c()
  # for (i in 1:(length(feet) - 1)) {
  #   ys <- yr[feet[i]:feet[i + 1]]
  #   ys <- resampleQ(ys, rf, length(ys), i)
  #   ydiff[i] <- sqrt(mean((selection - ys)^2))
  #   # points(ys, col = "green", pch = ".")
  # }
  
  ys_arr <- vector(mode = "list", length = length(feet) - 1)

  for (i in 1:(length(feet) - 1)) {
    ys <- yr[feet[i]:feet[i + 1]]
    ys_arr[[i]] <- resample(ys, rf, length(ys))
    ysum <- ysum + ys_arr[[i]]
    points(ys, col = "green", pch = ".")
  }
  
  selection <- ysum / (length(feet) - 1)
  lines(selection, col = "red")

  ydiff <- sapply(ys_arr, function(ys) sqrt(mean((selection - ys)^2)))
  
  
  
  yl <- order(ydiff)
  yd <- ydiff[yl]

  ysum <- rep(0, rf)

  # CobaYS <- yr[feet[yl[1]] : feet[yl[1]+1]]
  cat(paste0("feet = ", feet, "\n"))
  
  ysTest2 <-yr[feet[yl[1]] : feet[yl[1]+1]] # COBA
  ysTest2 <- resample(ysTest2, rf, length(ysTest2))  # COBA
  
  plot(ysTest2, type = "l", xlab = "Index", ylab = "Value", main = "Feet and Average Half Selection")
  

  for (i in 1:floor((length(feet)-1)/2)) {
    ys <- yr[(feet[yl[i]]):(feet[yl[i]+1])]
    ys <- resample(ys, rf, length(ys))
    ysum <- ysum + ys
    points(ys, col = "green", pch = ".")
  }

  selection <- ysum / floor((length(feet)-1)/2)
  lines(selection, col = "red")

  
  return(selection)
}
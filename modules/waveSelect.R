waveselect <- function(comp) {
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
    n <- length(u)
    t <- (0:(n-1))/s
    f <- smooth.spline(t, u, spar = r)
    y <- predict(f, t)
    if (b == 1) selection <- y[301:10300]
    else selection <- rbind(selection, y[301:10300])
  }
  
  y <- selection
  # Loc = peakFinder(y);
  # Loc2 = peakFinder(y,c(),c(),-1,c())
  
  return(list(
    y = y
  ))
}
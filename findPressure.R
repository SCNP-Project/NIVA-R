

findpressure <- function(raw, cSys, cDia, title){
  
  # Perubahan data sensor ke mm Hg
  #  raw ==> integer long 32bit
  #  P ==> float / double
  V <- raw/(2^23)*4.9;
  P <- (V/0.09 - 2.2222)*7.5;
  
  # write.csv(pressLA,  paste(title, "_P.scv"))
  
  plot(P, type='l', main =  paste(title, "P"),  xlab="Time (mili second)", ylab="mmhg")
  
  pf <- smoothTest2(P,2000)
  osc <- P - pf;
  
  plot(osc, type='l', main =  paste(title, "OSC"),  xlab="Time (mili second)", ylab="mmhg")
  
  press <- medianFilter1(P, 100)
  #loc <- match(max(press), press)
  loc <- which(press == max(press))
  loc <- loc[1]
  
  oscmaxmin <- c()
  
  for(i in 1:(length(osc)-1001)) {
    oscSubset <- osc[i:(i+1000)]
    oscmaxmin[i] <- max(oscSubset) - min(oscSubset)
  }
  
  # press <- press[loc:length(press)]
  end1 <- 67000
  press <- press[loc:end1]
  oscmaxmin <- oscmaxmin[loc:length(oscmaxmin)]
  
  #Plot before smooth
  plot(oscmaxmin, type='l', main =  paste(title, "Raw"),  xlab="Time (mili second)", ylab="mmhg")
  
  oscmaxmin <- smoothTest2(oscmaxmin, 10000)
  
  # Plot after smooth
  plot(oscmaxmin, type='l', main = paste(title, "Smooth"), xlab="Time (mili second)", ylab="mmhg")
  
  oscmaxmin <- oscmaxmin[2000:length(oscmaxmin)]
  
  # Plot after smooth & cut 2000 ms 
  plot(oscmaxmin, type='l', main = paste(title, "Smooth with Cutting 2000ms"), xlab="Time (mili second)", ylab="mmhg")
  
  pos <- which.max(oscmaxmin)
  mag <- oscmaxmin[pos]
  

  
  # MAP
  mapidx <- pos
  map <- press[mapidx]
  mapheight <- oscmaxmin[mapidx]
  
  # Constant
  # cSys <- 0.7
  # cDia <- 0.7
  
  # Systole
  i <- mapidx
  while ((oscmaxmin[i] > (0.7*mapheight)) && (i != 1)) {
    i <- i - 1
  }
  sys <- press[i]
  sysidx <- i
  
  # Diastole
  i <- mapidx
  while ((oscmaxmin[i] > (cDia*mapheight)) && (i != length(osc))) {
    i <- i + 1
  }
  dia <- press[i]
  diaidx <- i
  
  listReturn <- list(
    oscmaxmin_i = oscmaxmin[i],
    mapidx = mapidx,
    map = map,
    mapheight = mapheight,
    sysidx=sysidx,
    systolic = sys,
    diastolic = dia
  )
  
  
  return(listReturn)
  
  
}
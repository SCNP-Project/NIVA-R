calculatePpg <- function(ppginput,subjectHeight) {
  fs <- 1000               # frekuensi sampling
  fc <- 100               # denyut jantung maksimum (ga penting)
  
  filtLConstant <- 0.02
  Sqrt2PI <- 2.506628274631
  VAConstant1 <- 45.5
  VAConstant2 <- 65.9
  
  # n <- length(ppginput)
  r <- 0.9999
  # t <- (1:n-1)/fs
  # f <- smooth.spline(t, ppginput, spar = r)
  # A <- predict(f, t)
  
  differential_data <- differential(ppginput, fs, r)
  A <- differential_data$y2
  
  require(pracma)
  centralConv <- function(u,v) {
    conVec <- conv(u,v)
    startIndex <- as.integer(length(conVec)/2) - as.integer((length(u)/2)-1) 
    endIndex <- startIndex + (length(u)-1)
    return (conVec[startIndex : endIndex])
  }
  
  sinyal <- A
  filtL <- filtLConstant*fs
  t <- c(1:length(sinyal))/fs
  ddump1 <- round(filtL*4)
  ddump2 <- c(-ddump1:ddump1)/filtL
  filt <- exp(-ddump2^2/2)/Sqrt2PI/filtL
  
  sinyal2 <- c(sinyal[1]*rep(1, ddump1), sinyal, sinyal[length(sinyal)]*rep(1,ddump1))
  sinyalf <- centralConv(sinyal2,filt)
  sinyalf <- sinyalf[ddump1 + c(1:length(sinyal))]
  singra1 <- diff(c(0,sinyalf))*fs
  singra2 <- diff(c(singra1,0))*fs
  singra3 <- diff(c(0,singra2))*fs
  singra1[c( 1:(filtL*4), (length(singra1)-filtL*4):length(singra1) )] <- 0
  singra2[c( 1:(filtL*4), (length(singra2)-filtL*4):length(singra2) )] <- 0
  singra3[c( 1:(filtL*4), (length(singra3)-filtL*4):length(singra3) )] <- 0
  
  # Find systolic peaks
  Tc <- round(60 / fc  * fs  / 2)
  ika <- 0 
  iki <- 0 
  n_max_x <- rep(0, ceil(length(sinyalf) / fs  * fc  / 60) + 1)
  n_min_x <- n_max_x
  
  for (ii in (Tc+1) : (length(sinyalf)-Tc)) {
    dumpa <- TRUE
    dumpi <- TRUE
    ij <- 1    
    
    if (iki > ika) {
      while (dumpa && ij <= Tc) {
        dumpa <- dumpa & ( (sinyalf[ii] > sinyalf[ii-ij]) & (sinyalf[ii] > sinyalf[ii+ij]) )
        ij <- ij + 1
      }
      
      if (dumpa) {
        ika <- ika + 1
        n_max_x[ika] <- ii                        
      }
    } else {
      while (dumpi && ij <= Tc) {
        dumpi <- dumpi & ( (sinyalf[ii] < sinyalf[ii-ij]) & (sinyalf[ii] < sinyalf[ii+ij]) )
        ij <- ij + 1
      }
      
      if (dumpi) {
        iki <- iki + 1
        n_min_x[iki] <- ii                        
      }
      
    }
  }
  
  n_min_x <- n_min_x[1:iki]
  n_max_x <- n_max_x[1:(iki-1)]
  
  # Find dicrotic notch and diastolic peaks
  sinyald <- sinyalf
  sinyald[1 : (n_min_x[1]-1)] <- 0
  sinyald[(length(n_min_x)+1) : length(sinyald)] <- 0
  n_bel_x <- rep(NaN, length(n_max_x))
  n_dia_x <- n_bel_x
  
  for (ii in 1:length(n_max_x)) {
    sinyald[n_min_x[ii] : (n_max_x[ii]-1)] <- 0
    dumpg <- -( sinyalf[n_min_x[ii+1]] - sinyalf[n_max_x[ii]] ) / ( n_min_x[ii+1] - n_max_x[ii])
    sinyald[ n_max_x[ii] : n_min_x[ii+1] ] <- sinyalf[ n_max_x[ii] : n_min_x[ii+1] ] + dumpg*c( 0 : (n_min_x[ii+1] - n_max_x[ii]) ) - sinyalf[n_max_x[ii]]
    
    dumpp <- round((n_min_x[ii+1] - n_max_x[ii]) / 5)
    dumpy <- min(sinyald[n_max_x[ii] + c(dumpp : (4*dumpp))])
    dumpx <- which.min(sinyald[n_max_x[ii] + c(dumpp : (4*dumpp))])
    n_bel_x[ii] <- n_max_x[ii]+dumpx-1+dumpp
    
    dumpx <- singra1[ round( n_bel_x[ii] : (0.5*(n_bel_x[ii] + n_min_x[ii+1])) ) ]
    dumpy <- c(0, dumpx[1:(length(dumpx)-1)])
    dumpx[dumpy<dumpx] <- 1000
    dumpy <- min(abs(dumpx))
    dumpx <- which.min(abs(dumpx));
    n_dia_x[ii] <- n_bel_x[ii] + dumpx + 1    
  }
  
  # Calculate parameter
  n_ha0_x <- rep(NaN, length(n_max_x))
  n_ha1_x <- n_ha0_x;
  par <- list(x=c(), y=c(), AI=c(), PI=c(), dt=c(), SI=c(), IPA=c(), PW=c(), VA=c())
  
  par$x <- sinyalf[n_max_x] - sinyalf[n_min_x[ 1 : (length(n_min_x)-1) ]]
  par$y <- sinyalf[n_dia_x] - sinyalf[n_min_x[ 1 : (length(n_min_x)-1) ]]
  par$SA <- par$x
  par$AI <- par$y / par$x                              
  par$PI <- ( n_min_x[2:length(n_min_x)] - n_min_x[1:(length(n_min_x)-1)] ) / fs      
  par$dt <- ( n_dia_x - n_max_x ) / fs                      
  par$SI <- subjectHeight / par$dt                     
  par$IPA <- rep(0, length(par$SI))
  
  for (ii in 1:length(n_max_x)) {
    dump0 <- ( sinyalf[n_min_x[ii]] + sinyalf[n_min_x[ii+1]] ) / 2
    dumpx <- sum( sinyalf[ n_min_x[ii] : n_dia_x[ii] ] - dump0 )
    dumpy <- sum( sinyalf[ n_dia_x[ii] : n_min_x[ii+1] ] - dump0 )
    par$IPA[ii] <- dumpy / dumpx
    dump1 <- ( sinyalf[n_max_x[ii]] + sinyal[n_min_x[ii]] ) / 2
    dump0 <- sinyalf[ n_min_x[ii] : n_max_x[ii] ]
    dumpy <- min(abs(dump0-dump1))
    dumpx <- which.min(abs(dump0-dump1))
    n_ha0_x[ii] <- n_min_x[ii] + dumpx - 1
    dump0 <- sinyalf[ n_max_x[ii] : n_min_x[ii+1] ]
    dumpy <- min(abs(dump0-dump1))
    dumpx <- which.min(abs(dump0-dump1))
    n_ha1_x[ii] <- n_max_x[ii] + dumpx - 1
  }
  par$PW <- (n_ha1_x - n_ha0_x) / fs
  
  # Calculate Heart Rate
  peak_loc <- n_max_x
  beat_intv <- 0
  beat_count <- 1
  for (i in 1:(length(peak_loc)-1)){
    if (peak_loc[i] < peak_loc[i+1]-10) {
      beat_intv <- beat_intv + (fs /(peak_loc[i+1] - peak_loc[i]))*60
      beat_count <- beat_count + 1
    }
  }
  
  if (beat_count > 1) {
    par$PR <- beat_intv / (beat_count-1)
  }
  
  
  # a-b-c-d-e
  dumpna <- dumpnb <- dumpnc <- dumpnd <- dumpne <- rep(NaN, length(n_max_x))
  
  for ( ii in 1:length(n_max_x) ) {
    dump0 <- singra2[ n_min_x[ii] : n_max_x[ii] ]
    
    dumpy <- max(dump0)
    dumpx <- which.max(dump0) 
    dumpna[ii] <- n_min_x[ii] + dumpx - 1
    
    dumpy <- min(dump0)
    dumpx <- which.min(dump0) 
    dumpnb[ii] <- n_min_x[ii] + dumpx - 1
    
    dump0 <- singra2[ n_max_x[ii] : n_dia_x[ii] ]
    
    dumpy <- max(dump0)
    dumpx <- which.max(dump0)
    dumpne[ii] <- n_max_x[ii] + dumpx - 1
    
    dumpp <- round((dumpne[ii] - dumpnb[ii]) / 4)
    dump0 <- singra3[dumpnb[ii] + dumpp + c(1:(2*dumpp))]
    
    dumpy <- max(dump0)
    dumpx <- which.max(dump0)
    dumpnc[ii] <- dumpnb[ii] + dumpp + dumpx
    
    dumpy <- min(dump0)
    dumpx <- which.min(dump0)
    dumpnd[ii] <- dumpnb[ii] + dumpp + dumpx
  }
  
  pabcd <- list(a=c(), b=c(), c=c(), d=c(), e=c())
  dump1 <- floor((dumpnd-dumpnc) / 2)
  dumpnc <- dumpnc + dump1
  dumpnd <- dumpnd + dump1
  pabcd$a <- singra2[dumpna]
  pabcd$b <- singra2[dumpnb]
  pabcd$c <- singra2[dumpnc]    
  pabcd$d <- singra2[dumpnd]
  pabcd$e <- singra2[dumpne]
  
  compresultnum <- pabcd$b - pabcd$c - pabcd$d - pabcd$e
  VA <- (compresultnum / pabcd$a) * VAConstant1 + VAConstant2
  
  
  # Result
  pulsewidth <- median(par$PW)
  pulseinterval <- median(par$PI)
  augmentationindex <- median(par$AI)
  stiffnessindex <- median(par$SI)
  ipa <- median(par$IPA)
  timedelay <- median(par$dt)
  vascularage <- max(VA)
  systolicamplitude <- median(par$SA)
  heartrate <- par$PR
  
  # Return PPG Function
  listReturn <- list(
    pulsewidth = pulsewidth,
    pulseinterval = pulseinterval,
    augmentationindex = augmentationindex,
    stiffnessindex = stiffnessindex,
    ipa = ipa,
    timedelay = timedelay,
    vascularage = vascularage,
    systolicamplitude = systolicamplitude,
    heartrate = heartrate
  )
  
  return(listReturn)
  
}
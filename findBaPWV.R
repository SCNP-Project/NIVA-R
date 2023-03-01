findbapwv <- function(pressarm,pressleg,height) {
  
  # Perubahan data sensor Arm ke mm Hg
  # Varm <- pressarm/(2^23)*5
  Varm <- pressarm/(2^23)*4.9
  Parm <- (Varm/0.09 - 2.2222)*7.5
  
  # Perhitungan osc Arm yang berasal dari sinyal Parm
  pfarm <- smoothTest2(Parm, 2000)
  oscarm = Parm - pfarm
  
  # Perubahan data sensor Leg ke mm Hg
  # Vleg <- pressleg/(2^23)*5
  Vleg <- pressleg/(2^23)*4.9
  Pleg <- (Vleg/0.09 - 2.2222)*7.5
  
  # Perhitungan osc Leg yang berasal dari sinyal Pleg
  pfleg <- smoothTest2(Pleg, 2000)
  oscleg <- Pleg - pfleg
  
  # Perhitungan bapwv dari P dan osc (Arm & Leg)
  press = medianFilter1(pressleg, 100)
  loc <- which(press == max(press))
  loc <- loc[1]
 
  # buat motong diambil dari leg supaya dapet MAP dari arm juga
  oscmaxmin <- c()
  
  for(i in 1:(length(oscleg)-1001)) {
    oscSubset <- oscleg[i:(i+1000)]
    oscmaxmin[i] <- max(oscSubset) - min(oscSubset)
  }
  
  oscmaxmin <- oscmaxmin[loc:length(oscmaxmin)]
  oscarm <- oscarm[loc:length(oscarm)]
  oscleg <- oscleg[loc:length(oscleg)]
  
  # oscmaxmin <- oscmaxmin[2000:length(oscmaxmin)]
  # 
  # pos <- which.max(oscmaxmin)
  # mag <- oscmaxmin[pos]
  

  testLength <- length(oscmaxmin)
  testLength <- testLength - 30000
  newOscmaxim <- oscmaxmin[2000: testLength]
  
  # pos <- which.max(oscmaxmin)
  pos <- peakFinder(newOscmaxim)
  pos <- pos + 1999
  
  mapidx <- pos[length(pos)]

  
  # menghindari systol dan diastol
  # biasanya variabel map index ini yang dirubah-rubah jika hasil perhitungannya error
  oscarm <- oscarm[(mapidx-5000):(mapidx)]
  oscleg <- oscleg[(mapidx-5000):(mapidx)]
   
  differential_oscarm <- differential(oscarm, 1000, 0.9999)
  differential_oscleg <- differential(oscleg, 1000, 0.9999)
  # 
  d1arm <- differential_oscarm$d1
  d2arm <- differential_oscarm$d2
  d1leg <- differential_oscleg$d1
  d2leg <- differential_oscleg$d2
  
  # 
  posarm1 <- peakFinder(d2arm)
  posleg1 <- peakFinder(d2leg)

  posarm <- posarm1[d1arm[posarm1] > 0]
  posleg <- posleg1[d1leg[posleg1] > 0]

  Lb <- 0.8129*height + 12.328
  La <- 0.2195*height - 2.074
  deltaL <- Lb - La

  # kurangin baPWV di titik 1 dan akhir
  # untuk menghilangkan error
  bapwvd2 <- c()

  for(i in 2:length(posleg)-1) {
    armpeakidx <- posarm[posarm<posleg[i] & posarm>posleg[i-1]]
    
    if (length(armpeakidx) != 0){
      armpeakidx <- armpeakidx[length(armpeakidx)]
      delta <- (posleg[i] - armpeakidx)/1000
      bapwvd2[i] <- deltaL/delta
    }else{
      bapwvd2[i] <- 0
    }
  }

  # diambil baPWV terburuk
  bapwv <- max(bapwvd2)
  
  

  # return(list(
  #   d2leg = d2leg,
  #   posleg = posleg,
  #   posleg1 = posleg1
  #   
  # ))
  
  return(bapwv)
  
}
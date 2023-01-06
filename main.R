require('pspline')
require('pracma')

# Import data NIVA
source("./modules/readFile.R")
source("./modules/smoothTest2.R")
source("./modules/medianFilter1.R")
source("./modules/differential.R")
source("./modules/peakFinder.R")
source("./modules/waveSelect.R")
source("./findPressure.R")
source("./findBaPWV.R")
source("./compCalc.R")
source("./calculatePpg.R")

# koefisien / constant
arm_cSys <- 0.7	
arm_cDia <- 0.7
leg_cSys <- 0.76
leg_cDia <- 0.66
height <- 167

source("./findPressure.R")
pressarmleft <- findpressure(pressLA, arm_cSys, arm_cDia, 'Left Arm')
pressarmright <- findpressure(pressRA, arm_cSys, arm_cDia, 'Right Arm')
presslegleft <- findpressure(pressLL, leg_cSys, leg_cDia, 'Left Leg')
presslegright <- findpressure(pressRL, leg_cSys, leg_cDia, 'Right Leg')

lasys <- pressarmleft$systolic
ladia <- pressarmleft$diastolic
rasys <- pressarmright$systolic
radia <- pressarmright$diastolic
llsys <- presslegleft$systolic
lldia <- presslegleft$diastolic
rlsys <- presslegright$systolic
rldia <- presslegright$diastolic

# Find Abi
leftabi <- llsys/max(lasys, rasys)
rightabi <- rlsys/max(lasys, rasys)

# perhitungan baPWV
source("./findBaPWV.R")
leftbapwv <- findbapwv(pressLA, pressLL, height)
rightbapwv <- findbapwv(pressRA, pressRL, height)

# Perhitungan Compliance
# source("./modules/waveSelect.R")
# comp_select <- waveselect(comp)
# comp_select
# test <- dim(comp)
compresult <- compCalc(comp, lasys, ladia, height)
C1 <- compresult$C1
C2 <- compresult$C2

# Perhitungan PPG
ppginput <- PPGLA
ppginput <- ppginput[40760:(40760+12500)]
source("./calculatePpg.R")
ppgparams <- calculatePpg(ppginput, height)

# ini change





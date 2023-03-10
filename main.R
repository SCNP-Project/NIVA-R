require('pspline')
require('pracma')

# Import data NIVA
source("./modules/readFile.R")
source("./modules/smoothTest2.R")
source("./modules/medianFilter1.R")
source("./modules/differential.R")
source("./modules/peakFinder.R")
source("./modules/waveSelect.R")
source("./modules/fakeSpline.R")
source("./modules/resample.R")
source("./modules/signal/fir1.R")
source("./modules/signal/fir2.R")
source("./modules/signal/hamming.R")
source("./findPressure.R")
source("./findBaPWV.R")
source("./compCalc.R")
source("./calculatePpg.R")

# koefisien / constant
arm_cSys <- 0.62
arm_cDia <- 0.65
leg_cSys <- 0.65
leg_cDia <- 0.65
height <- 170

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


df <- data.frame(
  name = c('LA systolic','LA diastolic','RA systolic','RA diastolic','LL systolic','LL diastolic','RL systolic','RL diastolic','Left ABI','Right ABI','Left baPWV','Right baPWV'),
  value = c(lasys,ladia,rasys,radia,llsys,lldia,rlsys,rldia,leftabi,rightabi,leftbapwv,rightbapwv)
)
df
# Perhitungan Compliance
# source("./modules/fakeSpline.R")
# source("./modules/polyfun/unmkpp.R")
# source("./modules/polyfun/ppval.R")
source("./modules/resampleQ.R")
source("./modules/waveSelect.R")
comp_select <- waveselect(comp)
# comp_select <- t(comp_select)

plot(comp_select, type='l', main =  'comp_select',  xlab="Time (mili second)")

source("./modules/differential.R")
source("./compCalc.R")
compresult <- compCalc(comp_select, lasys, ladia, height)
C1 <- compresult$C1
C2 <- compresult$C2

# Perhitungan PPG
ppg_select <- waveselect(PPGLA)
ppginput <- PPGLA
ppginput <- ppginput[40760:(40760+12500)]
source("./calculatePpg.R")
ppgparams <- calculatePpg(ppginput, height)

# ini change


#### TESTING ####

testCompSelect <- read.csv("./data/comp_select")
mydata <- read.table("./data/comp_select", header = FALSE)
my_vector <- scan(text = mydata$V1, sep = ",")
plot(my_vector, type='l', main =  'comp_select',  xlab="Time (mili second)")


lasys2 <- 135.2991
ladia2 <- 79.3774
height2<- 170
source("./compCalc.R")
compresult <- compCalc(my_vector, lasys2, ladia2, height2)
source("./compCalcOld.R")
compresult <- compCalcOld(my_vector, lasys, ladia, height)
C1 <- compresult$C1
C2 <- compresult$C2




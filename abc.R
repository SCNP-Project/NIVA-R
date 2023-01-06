fooABC <- function(x) {
  k <- barXYZ(x)+1
  return(k)
}

barABC <- function(x){
  k <- x+30
  return(k)
}

library(signal)

# Membuat sinyal x yang terdiri dari 100 titik dan terdapat noise pada sinyal tersebut
x <- 1:100
x <- x + rnorm(100, mean=0, sd=10)
write.csv2(x, file = "my_data.csv")
x

mmed <- function(x,n=5){runmed(x,n)}
mmed(x)

y <- c(1,4,9,16,25,36,49,64,81,100,21,120,301,401,20,30,10,22,13,14,16,20)
runmed(y, 5)

test12 <- c(0.263825557118717, 10.6120437175075, 5.53699639512152, 9.05646500210806, 7.86006043646057, 3.8725899283165, -5.68927004892853, 3.93815479769659, 18.8346132089137, 8.62225422639869, 12.20908082496, 21.4108084144279, 15.7493259700793, 12.3017232066574, 18.682660076382, 5.70050334887404, 10.2261825960595, 23.4794886286413, 19.7316562286767, 12.0393203634654, 26.7142665606612, 16.9997900104343, 39.1487966015279, 32.6483525934064, 29.0559368957424, 8.56292465146014, 27.8297046907842, 4.83498628124321, 22.9136870921276, 12.7633424682375)
mmed(test12)
runmed(test12, 5, endrule='constant')


cobaDariVS <- function(input, order) {
  press <- c()
  order <- order
  
  for(i in 1:length(input)) {
    indices <- as.integer(order/2)
    medianArr <- c()
    start <- i-indices
    end <- if(order %% 2 == 1) i+indices else i+(indices-1)
    
    if(start<1) {
      start <- 0
      medianArr <- c(medianArr, rep(0, abs(i-indices)+1))
      medianArr <- c(medianArr, input[start : end])
    }
    else if(end>length(input)) {
      medianArr <- c(medianArr, input[start : length(input)])
      medianArr <- c(medianArr, rep(0, end-length(input)))
    }
    else {
      medianArr <- c(medianArr, input[start : end])
    }
    
    press[i] <- median(medianArr)
  }
  return(press)
}

test13 <- cobaDariVS(test12, 5)

# Buat vektor press dengan nilai acak
press <- c(11, 8, 10, 9, 7, 10, 8, 11, 13, 13)
loc <- match(max(press), press)
loc
# Ambil indeks pertama dari hasil yang dikembalikan oleh fungsi find()
loc <- loc[1]
loc

oscmaxmin <- c(1,2,3,4,5,7,8,9,10,11,12,13,14,15,15,15,14, 20) 
# Mencari indeks posisi maksimum dari sinyal oscmaxmin
pos <- which.max(oscmaxmin)
pos
# Mencari nilai maksimum dari sinyal oscmaxmin
mag <- oscmaxmin[pos]
mag
# Mencari indeks posisi maksimum terakhir dari sinyal oscmaxmin
mapidx <- pos[length(pos)]


testFungsi <- function(input) {
  
  listReturn <- list(
    studentName = "Harry Potter",
    studentAge = 19,
    studentContact ="London"
  )
  
  input + 2
  input + 5
  
  return(listReturn)
}

makan <- testFungsi(pos)
makan
makan$studentAge


rm(list = ls())

# initialize keys and respected values
students <- c("Pulkit", "Ritika", "Parth",
              "Vishesh", "Dharvik", "krishav",
              "Reshav")

marks <- c(75, 92, 97, 80, 85, 87, 52)

# make the list
results <- what(as.list(marks), students)

# Access value using the key
print(results$Pulkit)


# Plot 
plot(oscmaxmin, type='l')
plot(c(2,5),  type='l', main="Main title",
        xlab="X axis title",
        ylab="Y axis title",
        sub="Sub-title")


# Membuat object press yang berisi sekumpulan data
press <- c(1, 4, 9, 16, 25, 36, 49, 64, 81, 100, 21, 120, 301, 1, 301)

# Mencari indeks posisi nilai maksimum dari press
loc <- which(press == max(press))
loc


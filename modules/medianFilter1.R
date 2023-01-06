medianFilter1 <- function(input, order) {
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
smoothTest2 <- function(y, span){
  yy <- c()
  span <- span
  devider <- 3
  if (span %% 2 == 0) span <- span-1
  
  for(i in 1:length(y)) {
    mean <- 0
    
    if(i==1 || i==length(y)) mean <- y[i]
    else {
      spanBegin <- (i-(as.integer(devider/2)))
      spanEnd <- (i+(as.integer(devider/2)))
      mean <- sum(y[spanBegin:spanEnd]) / devider 
      
      if(devider < span && i < span) devider <- devider + 2
      else if( (i+1) > length(y)-(as.integer(span/2))) devider <- devider - 2 
    }
    
    yy[i] <- mean
  }
  
  return(yy)
}
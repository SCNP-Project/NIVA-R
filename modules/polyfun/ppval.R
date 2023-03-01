v <- function(pp, xx) {
  
  if(is.data.frame(xx) || is.list(xx)){
    temp <- xx
    xx <- pp
    pp <- temp
  }
  
  
  if(!is.numeric(xx)){
    stop("error: input must be numeric")
  }
  if(is.complex(xx)){
    stop("error: input must be real")
  }
  
  # obtain the row vector xs equivalent to XX
  sizexx <- length(xx)
  lx <- length(xx)
  xs <- c(xx)
  
  #if XX is row vector, suppress its first dimension
  if(length(sizexx)==2 && sizexx[1]==1) sizexx[1] <- NULL
  
  # take apart PP
  unmkPp <- unmkpp(pp)
  b <- unmkPp$breaks
  c <- unmkPp$coefs
  l <- unmkPp$l
  k <- unmkPp$k
  dd <- unmkPp$d
  
  # for each evaluation site, compute its breakpoint interval
  # (mindful of the possibility that xx might be empty)
  # if(lx){
  #   index <- cut(xs, c(-Inf, b[2:l], Inf), labels = TRUE)
  # } else {
  #   index <- rep(1,lx)
  # }
  
  index <- rep(1,lx)
  
  # 
  # # adjust for troubles, like evaluation sites that are NaN or +-inf
  # infxs <- which(xs==Inf)
  # if(length(infxs)) index[infxs] <- l
  # nogoodxs <- which(index==0)
  # if(length(nogoodxs)){
  #   xs[nogoodxs] <- NaN
  #   index[nogoodxs] <- 1
  # }
  # 
  # # now go to local coordinates ...
  # xs <- xs - b[index]
  # 
  # if(d>1){
  #   xs <- rep(xs, d)
  #   index <- d*index
  #   temp <- (-d:-1)
  #   index <- rep(index,d) + temp
  # } else {
  #   if (length(sizexx)>1) dd <- NULL else dd <- 1
  # }
  # 
  # # ... and apply nested multiplication:
  # v <- c(index,1)
  # for(i in 2:k){
  #   v <- xs*v + c(index,i)
  # }
  # 
  # # If evaluating a piecewise constant with more than one piece at NaN, return
  # # NaN.  With one piece return the constant.
  # 
  # if(!is.null(nogoodxs) && k==1 && l>1){
  #   v <- matrix(v,d,lx)
  #   v[,nogoodxs] <- NA
  # }
  # v <- array(v,c(dd,sizexx))
  # 
  # if("orient" %in% names(pp) && pp$orient == "first"){
  #   if(!(is.null(dd) || (length(dd) == 1 && dd == 1))){
  #     if(is.vector(xx) && !is.scalar(xx)){
  #       permVec <- c(ndim(v),1:(ndim(v)-1))
  #     }else{
  #       ndimsxx <- ndim(xx)
  #       permVec <- c((ndim(v)-ndimsxx+1) : ndim(v), 1:(ndim(v)-ndimsxx))
  #     }
  #     v <- aperm(v, permVec)
  #   }
  # }
  
  
  
  
  
  
}
peakFinder <- function(x0, sel, thresh, extrema, include_endpoints) {
  
  # Find array/vector/object size
  s <- dim(x0)
  
  # return(s)
  
  s <- if (is.null(s)) c(1, length(x0)) else s

  flipData <- s[1] < s[2]
  len0 <- length(x0)


  # # Check if data is not a vector or empty
  if (len0 != s[1] && len0 != s[2]) stop('PEAKFINDER:Input - ','The input data must be a vector')
  else if (length(x0) == 0) return (c())

  # # Check if data is real number
  if(!is.numeric(x0)) {
    warning('PEAKFINDER:NotReal - ','Absolute value of data will be used')
    x0 <- abs(x0)
  }


  # # Parameter check: If there is only 1 input parameter exist
  if (nargs() < 2 || missing(sel) || !is.numeric(sel)) sel <- (max(x0)-min(x0))/4
  else if (length(sel) > 1) sel <- sel[1]

  # # Parameter check: If there are only 2 input parameter exist
  if (nargs() < 3 || missing(thresh) || !is.numeric(thresh)) thresh <- c()
  else if (length(thresh) > 1) thresh <- thresh[1]

  # # Parameter check: If there are only 3 input parameter exist
  if (nargs() < 4 || missing(extrema)) extrema <- 1
  else {
    # Should only be 1 or -1 but make sure
    extrema <- sign(extrema[1])
    if (extrema == 0) stop('PEAKFINDER:ZeroMaxima - ','Either 1 (for maxima) or -1 (for minima) must be input for extrema')
  }

  # # Parameter check: If there are only 4 input parameter exist
  if (nargs() < 5 || missing(include_endpoints)) include_endpoints <- TRUE


  # # Make it so we are finding maxima regardless
  x0 <- extrema*x0
  # # Adjust threshold according to extrema.
  thresh <- thresh*extrema
  # # Find derivative
  dx0 <- diff(x0)
  # # This is so we find the first of repeated values. Note: User pracma package for eps
  require('pracma')
  dx0 <-replace(dx0, dx0==0, -eps(1))
  # Find where the derivative changes sign
  ind <- which(dx0[1:length(dx0)-1] * dx0[2:length(dx0)] < 0) + 1


  # # Include endpoints in potential peaks and valleys as desired
  if (include_endpoints) {
    x <- c(x0[1], x0[ind], x0[length(x0)])
    ind <- c(1, ind, len0)
    minMag <- min(x)
    leftMin <- minMag
  }
  else {
    x <- x0[ind]
    minMag <- min(x)
    leftMin <- x0[1]
  }


  # # x only has the peaks, valleys, and possibly endpoints
  len <- length(x)

  # # Function with peaks and valleys
  # if (len > 2) {
    # Set initial parameters for loop
    tempMag <- minMag
    foundPeak <- FALSE

    if (include_endpoints) {
      # Deal with first point a little differently since tacked it on
      # Calculate the sign of the derivative since we tacked the first
      # point on it does not neccessarily alternate like the rest.
      signDx <- sign(diff(x[1:3]))
      if (signDx[1] <= 0) {
        # The first point is larger or equal to the second
        if (signDx[1] == signDx[2]) {
          # Want alternating signs
          x <- x[-2]
          ind <- ind[-2]
          len <- len-1
        }
      }
      else { # First point is smaller than the second
        if (signDx[1] == signDx[2]) { # Want alternating signs
          x <- x[-1]
          ind <- ind[-1]
          len <- len-1
        }
      }
    }

    # Skip the first point if it is smaller so we always start on a maxima
    ii <- if (x[1] >= x[2]) 0 else 1

    # Preallocate max number of maxima
    maxPeaks <- ceil(len/2)
    peakLoc <- integer(maxPeaks)
    peakMag <- integer(maxPeaks)
    cInd <- 1

    # Loop through extrema which should be peaks and then valleys
    while (ii < len) {
      # This is a peak
      ii <- ii+1

      # Reset peak finding if we had a peak and the next peak is bigger than the last or the left min was small enough to reset.
      if (foundPeak) {
        tempMag <- minMag
        foundPeak <- FALSE
      }

      # Make sure we don't iterate past the length of our vector
      # We assign the last point differently out of the loop
      if (ii == len) break

      # Found new peak that was lager than temp mag and selectivity larger than the minimum to its left.
      if (x[ii] > tempMag && x[ii] > (leftMin + sel)) {
        tempLoc <- ii
        tempMag <- x[ii]
      }

      # Move onto the valley
      ii <- ii+1

      # Come down at least sel from peak
      if (!foundPeak && tempMag > (sel + x[ii])) {
        # We have found a peak
        foundPeak <- TRUE
        leftMin <- x[ii]

        # Add peak to index
        peakLoc[cInd] <- tempLoc
        peakMag[cInd] <- tempMag
        cInd <- cInd+1
      }
      else if (x[ii] < leftMin) {
        # New left minima
        leftMin <- x[ii]
      }
    }

    # Check end point
    if (include_endpoints) {
      if (x[length(x)] > tempMag && x[length(x)] > leftMin + sel) {
        peakLoc[cInd] <- len
        peakMag[cInd] <- x[length(x)]
        cInd <- cInd + 1
      }
      else if (!foundPeak && tempMag > minMag) {
        # Check if we still need to add the last point
        peakLoc[cInd] <- tempLoc
        peakMag[cInd] <- tempMag
        cInd <- cInd + 1
      }
    }
    else if (!foundPeak) {
      if (tempMag > x0[length(x0)] + sel) {
        peakLoc[cInd] <- tempLoc
        peakMag[cInd] <- tempMag
        cInd <- cInd + 1
      }
    }

    # Create output
    peakInds <- ind[peakLoc[1:cInd-1]]
    peakMags <- peakMag[1:cInd-1]
  } else {
  #   
  #   # This is a monotone function where an endpoint is the only peak
    peakMags <- max(x)
    xInd <- which.max(x)

    if (include_endpoints && (peakMags > (minMag + sel))) peakInds <- ind[xInd]
    else {
      peakMags <- c()
      peakInds <- c()
    }
  }

  # # Apply threshold value.  Since always finding maxima it will always be larger than the thresh.
  if (length(thresh) != 0) {
    m <- peakMags > thresh
    peakInds <- peakInds[m]
    peakMags <- peakMags[m]
  }

  # Rotate data if needed. In this R conversion, the vector doesn't flipped or even rotated
  if (flipData) {
    peakMags <- peakMags
    peakInds <- peakInds
  }

  # # Change sign of data if was finding minima
  if (extrema < 0) {
    peakMags <- (peakMags * -1)
    x0 <- -x0
  }

  # # Plot if no output desired
  if (missing(peakInds)) {
    warning('No significant peaks found')
  }
  else {
  #return(c(peakInds,peakMags))
    return(peakInds)
  }
}
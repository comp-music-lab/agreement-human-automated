transposer <- function(pitch, numshift) {
  if (pitch == "XX") {
    return(pitch)
  }
  
  PITCH_CLASS <- c("C", "d", "D", "e", "E", "F", "g", "G", "a", "A", "b", "B")
  
  PITCH <- substr(pitch, 1, 1)
  
  OCTAVE_MAP <- rep(as.numeric(substr(pitch, 2, 2)), length((PITCH_CLASS)))
  
  if (numshift > 0) {
    TRANSPOSED <- c(PITCH_CLASS[(1 + numshift):12], PITCH_CLASS[1:(1 + numshift - 1)])
    
    OCTAVE_MAP[(12- numshift + 1):12] <- OCTAVE_MAP[(12- numshift + 1):12] + 1
  } else if (numshift == 0) {
    TRANSPOSED <- PITCH_CLASS
  } else {
    numshift <- abs(numshift)
    
    TRANSPOSED <- c(PITCH_CLASS[(12 - numshift + 1):12], PITCH_CLASS[1:(12 - numshift)])
    
    OCTAVE_MAP[1:(1 + numshift - 1)] <- OCTAVE_MAP[1:(1 + numshift - 1)] - 1
  }
  
  idx <- PITCH_CLASS == PITCH
  pitch_new <- paste(TRANSPOSED[idx], as.character(OCTAVE_MAP[idx]), sep = "")
  
  return(pitch_new)
}
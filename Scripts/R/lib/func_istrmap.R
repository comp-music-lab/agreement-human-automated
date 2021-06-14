func_istrmap <- function(alignment, elemmap) {
  #
  for (j in 1:length(alignment)) {
    als1 <- as.character(alignment[j])
    als1 <- unlist(as.list(strsplit(als1, "")[[1]]))
    note1 <- als1
    uniqelem1 <- unique(als1)
    
    for (k in 1:length(uniqelem1)) {
      if (uniqelem1[k] != "-") {
        idx <- which(als1 == uniqelem1[k])
        mapidx <- which(elemmap[, 2] == uniqelem1[k])
        
        note1[idx] <- elemmap[mapidx, 1]
        
        note1 <- unlist(note1)
      } else {
        idx <- which(als1 == uniqelem1[k])
        
        note1[idx] <- "--"
        
        note1 <- unlist(note1)
      }
    }
    
    alignment[j] <- paste(note1, collapse = "") 
  }
  
  #
  return(alignment)
}
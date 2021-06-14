func_strmap <- function(noteseq, elemmap) {
  noteseq_list <- strsplit(noteseq, "(?<=[0-9a-zA-Z]{2})", perl = TRUE)
  
  uniqelem <- unique(unlist(noteseq_list))
  strlist <- noteseq_list
  i <- 1
    
  for(noteseq_i in noteseq_list) {
    for (e in uniqelem) {
      idx <- which(noteseq_i == e)
      mapidx <- which(elemmap[, 1] == e)
        
      noteseq_i[idx] <- elemmap[mapidx, 2]
    }
      
    strlist[[i]] <- paste(noteseq_i, collapse = "")
    i <- i + 1
  }
  
  strlist <- unlist(strlist)
    
  return(strlist)
}
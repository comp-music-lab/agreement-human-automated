library(stringdist)

func_levd <- function(noteseq) {
  #
  K <- length(noteseq)
  pair <- combn(K, 2)
  averageLEVD <- 0
  averageOPE <- 0
  
  #
  for (i in (1:dim(pair)[2])) {
    k <- pair[1, i]
    l <- pair[2, i]
    
    levd <- stringdist(noteseq[k], noteseq[l], method = "lv")
    numope <- levd2 <- drop(attr(adist(noteseq[k], noteseq[l], count = TRUE), "count"))
    
    averageLEVD <- averageLEVD + levd
    averageOPE <- averageOPE + numope
  }
  
  #
  averageLEVD <- averageLEVD / dim(pair)[2]
  averageOPE <- averageOPE / dim(pair)[2]
  
  return(c(averageLEVD, averageOPE))
}
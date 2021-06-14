library(Biostrings)

func_pid <- function(noteseq) {
  #
  K <- length(noteseq)
  pair <- combn(K, 2)
  averagePID <- 0
  averagePMM <- 0
  
  #
  for (i in (1:dim(pair)[2])) {
    k <- pair[1, i]
    l <- pair[2, i]
    
    alignment_kl <- 
      pairwiseAlignment(noteseq[k], noteseq[l], type = "global", substitutionMatrix = NULL, gapOpening = -.8, gapExtension = -.2)
    
    averagePID <- averagePID + pid(alignment_kl, type = "PID4")
    averagePMM <- averagePMM + nmismatch(alignment_kl)/(0.5*(nchar(noteseq[k]) + nchar(noteseq[k]))) * 100
  }
  
  #
  averagePID <- averagePID / dim(pair)[2]
  averagePMM <- averagePMM / dim(pair)[2]
  averagePGAP <- 100 - averagePID - averagePMM
  
  return(c(averagePID, averagePMM, averagePGAP))
}
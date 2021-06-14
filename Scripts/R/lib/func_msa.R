library(Biostrings)
library(gtools)
library(stringi)

func_msa <- function(noteseq) {
  idx <- 1:length(noteseq)
  
  #submat <- NULL
  #gapop <- -0.8
  #gapex <- -0.2
  #getbestidx <- which.max
  
  #submat <- NULL
  #gapop <- -1.0
  #gapex <- -1.0
  #getbestidx <- which.max
  
  submat <- matrix(-1.0, nrow = 26*2 + 11, ncol = 26*2 + 11, dimnames = list(c(letters, LETTERS, as.character(0:9), ' '), c(letters, LETTERS, as.character(0:9), ' ')))
  diag(submat) <- 0
  gapop <- 0.0
  gapex <- -1.0
  getbestidx <- which.max
  
  #submat <- matrix(-1.0, nrow = 26*2 + 11, ncol = 26*2 + 11, dimnames = list(c(letters, LETTERS, as.character(0:9), ' '), c(letters, LETTERS, as.character(0:9), ' ')))
  #diag(submat) <- 0
  #gapop <- -1.0
  #gapex <- -1.0
  #getbestidx <- which.max
  
  # search center sequence based on sum-of-pairs score
  sp_score <- vector(length = length(noteseq))
  for (k in 1:length(noteseq)) {
    idx_l <- setdiff(idx, k)
    
    for (l in idx_l) {
      alignment_kl <- 
        pairwiseAlignment(noteseq[k], noteseq[l], type = "global", substitutionMatrix = submat, gapOpening = gapop, gapExtension = gapex)
      
      sp_score[k] = sp_score[k] + score(alignment_kl)
    }
  }
  
  idx_c <- getbestidx(sp_score)
  
  # create merged sequence which encompasses all sequence patterns
  idx_l <- setdiff(idx, idx_c)
  
  alignment_kl <- 
    pairwiseAlignment(noteseq[idx_c], noteseq[idx_l[1]], type = "global", substitutionMatrix = submat, gapOpening = gapop, gapExtension = gapex)
  
  seq1 <- as.character(alignedSubject(alignment_kl))
  seq2 <- as.character(alignedPattern(alignment_kl))
  mseq <- paste(rep("*", nchar(seq1)), collapse = "")
  
  charpos <- which(stri_detect(strsplit(seq1, "")[[1]], regex = '\\w'))
  for (m in charpos) {
    substr(mseq, m, m) <- substr(seq1, m, m)
  }
  
  charpos <- which(stri_detect(strsplit(seq2, "")[[1]], regex = '\\w'))
  for (m in charpos) {
    substr(mseq, m, m) <- substr(seq2, m, m)
  }
  
  if (length(idx_l) > 1) {
    for (l in (2:length(idx_l))) {
      alignment_kl <- 
        pairwiseAlignment(mseq, noteseq[idx_l[l]], type = "global", substitutionMatrix = submat, gapOpening = gapop, gapExtension = gapex)
      
      seq1 <- as.character(alignedSubject(alignment_kl))
      seq2 <- as.character(alignedPattern(alignment_kl))
      mseq <- paste(rep("*", nchar(seq1)), collapse = "")
      
      charpos <- which(stri_detect(strsplit(seq1, "")[[1]], regex = '\\w'))
      for (m in charpos) {
        substr(mseq, m, m) <- substr(seq1, m, m)
      }
      
      charpos <- which(stri_detect(strsplit(seq2, "")[[1]], regex = '\\w'))
      for (m in charpos) {
        substr(mseq, m, m) <- substr(seq2, m, m)
      }
    }
  }
  
  # create alignment
  alignment <- vector(mode = "character", length = length(noteseq))
  
  for (k in (1:length(noteseq))) {
    alignment_kl <- 
      pairwiseAlignment(mseq, noteseq[k], type = "global", substitutionMatrix = submat, gapOpening = gapop, gapExtension = gapex)
    
    alignment[k] <- as.character(alignedSubject(alignment_kl))
  }
  
  #
  return(list(alignment, max(sp_score)))
}
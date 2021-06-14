#
source("./lib/func_pid.R")
source("./lib/func_levd.R")
source("./lib/func_msa.R")
source("./lib/func_irr.R")
source("./lib/func_elemmap.R")
source("./lib/func_strmap.R")
source("./lib/func_istrmap.R")
source("./lib/func_transposer.R")

#
datadir <- "./data/test/transcriber-pairwise_agreement/"
outputdir <- "./output/transcriber-pairwise_agreement/"
octavinf <- TRUE
group <- "human-human"
project <- "paper"

#TRANSPOSITION <- 0
TRANSPOSITION <- -2:2

#
songstyle <- read.csv("./song-style.csv", header = TRUE, sep = ",")

#
songdir <- list.files(datadir)
irrstat <- data.frame()

for (i in 1:length(songdir)) {
  # retrieve note sequences
  noteseqfile <- list.files(paste(datadir, songdir[i], "/human/", sep = ""), pattern = "*[.]csv")
  noteseq <- vector(mode = "list", length = length(noteseqfile))
  
  for (j in 1:length(noteseqfile)) {
    filepath <- paste(datadir, songdir[i], "/human/", noteseqfile[j], sep = "")
    noteseq[j] <- read.csv(filepath, header = FALSE, sep = ",")
    
    if (!octavinf) {
      noteseq[[j]] <- substr(noteseq[[j]], 1, 1)
    }
    
    noteseq[[j]] <- paste(noteseq[[j]], collapse = "")
  }
    
  noteseq <- unlist(noteseq)
  
  # keep original note sequences
  noteseq_orig <- lapply(noteseq, function(x) strsplit(x, "(?<=[0-9a-zA-Z-]{2})", perl = TRUE)[[1]])
  PID_t <- matrix(0, nrow = length(TRANSPOSITION), ncol = 1)
  
  # create pairwise combinations
  pairlist <- t(combn(x = 1:length(noteseq), m = 2))
  numpair <- dim(pairlist)[1]
  
  for (j in 1:numpair) {
    # choose pair
    noteseq_j = c(noteseq[pairlist[j, 1]], noteseq[pairlist[j, 2]])
    noteseqfile_j <- c(noteseqfile[pairlist[j, 1]], noteseqfile[pairlist[j, 2]])
    
    # search best transposition
    for (t in 1:length(TRANSPOSITION)) {
      noteseq_t <- paste(unlist(lapply(noteseq_orig[pairlist[j, 1]][[1]], function(x) transposer(x, TRANSPOSITION[t]))), collapse = "")
      
      # map 2 character strings into 1 character element
      if (octavinf) {
        elemmap <- func_elemmap(c(noteseq_t, noteseq_j[2]))
        charseq <- func_strmap(c(noteseq_t, noteseq_j[2]), elemmap)
      }
      
      averagePID <- func_pid(charseq)
      
      PID_t[t] <- averagePID[1]
    }
    
    numshift <- which.max(PID_t)
    
    # map 2 character strings into 1 character element
    noteseq_t <- paste(unlist(lapply(noteseq_orig[pairlist[j, 1]][[1]], function(x) transposer(x, TRANSPOSITION[numshift]))), collapse = "")
    
    if (octavinf) {
      elemmap <- func_elemmap(c(noteseq_t, noteseq_j[2]))
      charseq <- func_strmap(c(noteseq_t, noteseq_j[2]), elemmap)
    }
    
    # calculate average PID
    averagePID <- func_pid(charseq)
    
    # calculate average Levenstein distance
    averageLEVD <- func_levd(charseq)
    
    # get multiple sequence alignment and calculate inter-rater reliability coefficients
    alignment_score <- func_msa(charseq)
    alignment <- alignment_score[[1]]
    similarity <- alignment_score[[2]]
    irrstat_i <- func_irr(alignment)
    
    # map back to the original 2 character strings
    if (octavinf) {
      alignment <- func_istrmap(alignment, elemmap)
    }
    
    # output MSA
    transcriber <- unlist(lapply(noteseqfile_j, function (x) strsplit(x, "_")[[1]][1]))
    alignment.df <- data.frame(transcriber, alignment)
    
    cat(paste(songdir[i], ": ", paste(transcriber, collapse = "_"), "\n", sep = ""))
    cat(paste(paste(alignment, collapse = "\n")), "\n", sep = "")
    cat("\n")
    
    output_filename <- paste(outputdir, project, "/msa/", songdir[i], "_", paste(transcriber, collapse = "-"), "_msa.csv", sep = "")
    write.csv(alignment.df, output_filename, row.names = FALSE)
    
    # output analysis
    irrstat_i$averagePID <- averagePID[1]
    irrstat_i$averagePMM <- averagePID[2]
    irrstat_i$averagePGAP <- averagePID[3]
    irrstat_i$averageLEVD <- averageLEVD[1]
    irrstat_i$averageINS <- averageLEVD[2]
    irrstat_i$averageDEL <- averageLEVD[3]
    irrstat_i$averageSUB <- averageLEVD[4]
    irrstat_i$editsim <- similarity
    irrstat_i$group <- group
    irrstat_i$song <- songdir[i]
    irrstat_i$songstyle <- songstyle[which(songstyle[, 1] == songdir[i]), 2]
    irrstat_i$transcriber <- paste(transcriber, collapse = "_")
    irrstat_i$transposition <- TRANSPOSITION[numshift]
    
    print(irrstat_i)
    cat("\n")
    
    irrstat <- rbind(irrstat, irrstat_i)
  }
}
  
output_filename <- paste(outputdir, project, "/transcriber-pairwise_agreement.csv", sep = "")
write.csv(irrstat, output_filename, row.names = FALSE)
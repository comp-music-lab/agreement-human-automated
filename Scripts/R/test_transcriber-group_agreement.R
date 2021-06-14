#
source("./lib/func_pid.R")
source("./lib/func_levd.R")
source("./lib/func_msa.R")
source("./lib/func_irr.R")
source("./lib/func_elemmap.R")
source("./lib/func_strmap.R")
source("./lib/func_istrmap.R")

#
datadir <- "./data/test/transcriber-group_agreement/"
outputdir <- "./output/transcriber-group_agreement/"
octavinf <- TRUE
group <- "human-human"

#
songdir <- list.files(datadir)
irrstat <- data.frame()

for (i in 1:length(songdir)) {
  # retrieve note sequences
  noteseqfile <- list.files(paste(datadir, songdir[i], "/human/", sep = ""))
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
  
  # map 2 character strings into 1 character element
  if (octavinf) {
    elemmap <- func_elemmap(noteseq)
    noteseq <- func_strmap(noteseq, elemmap)
  }
  
  # calculate average PID
  averagePID <- func_pid(noteseq)
  
  # calculate average Levenstein distance
  averageLEVD <- func_levd(noteseq)
  
  # get multiple sequence alignment and calculate inter-rater reliability coefficients
  alignment_score <- func_msa(noteseq)
  alignment <- alignment_score[[1]]
  similarity <- alignment_score[[2]]
  irrstat_i <- func_irr(alignment)
  
  # map back to the original 2 character strings
  if (octavinf) {
    alignment <- func_istrmap(alignment, elemmap)
  }
  
  # output MSA
  transcriber <- sort(unlist(lapply(noteseqfile, function (x) strsplit(x, "_")[[1]][1])))
  alignment.df <- data.frame(transcriber, alignment)
    
  cat(paste(songdir[i], ": ", paste(transcriber, collapse = "_"), "\n", sep = ""))
  cat(paste(paste(alignment, collapse = "\n")), "\n", sep = "")
  cat("\n")
    
  output_filename <- paste(outputdir, "/msa/", songdir[i], "_", paste(transcriber, collapse = "-"), "_msa.csv", sep = "")
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
  irrstat_i$transcriber <- paste(transcriber, collapse = "_")
    
  print(irrstat_i)
  cat("\n")
    
  irrstat <- rbind(irrstat, irrstat_i)
}
  
output_filename <- paste(outputdir, "transcriber-group_agreement.csv", sep = "")
write.csv(irrstat, output_filename, row.names = FALSE)
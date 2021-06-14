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
project <- "paper"
datadir <- c("./data/test/human-machine-pairwise_agreement/with_unison/",
             "./data/test/human-machine-pairwise_agreement/without_unison/")
outputdir <- c(paste("./output/human-machine-pairwise_agreement/", project, "/with_unison/", sep = ""),
               paste("./output/human-machine-pairwise_agreement/", project, "/without_unison/", sep = ""))
outputfilename <- c("human-machine-pairwise_agreement (with unison).csv",
                    "human-machine-pairwise_agreement (without unison).csv")
octavinf <- TRUE
group <- "human-machine"

TRANSPOSITION <- -2:2

ddndd <- read.csv("./machine-type.csv", header = TRUE, sep = ",")
songstyle <- read.csv("./song-style.csv", header = TRUE, sep = ",")

#
for (k in 1:length(datadir)) {
  songdir <- list.files(datadir[k])
  irrstat <- data.frame()
  
  for (i in 1:length(songdir)) {
    # retrieve note sequences
    noteseqfile_h <- list.files(paste(datadir[k], songdir[i], "/human/", sep = ""))
    noteseqfile_m <- list.files(paste(datadir[k], songdir[i], "/machine/", sep = ""))
    noteseq_h <- vector(mode = "list", length = length(noteseqfile_h))
    noteseq_m <- vector(mode = "list", length = length(noteseqfile_m))
    
    # human
    for (j in 1:length(noteseqfile_h)) {
      filepath <- paste(datadir[k], songdir[i], "/human/", noteseqfile_h[j], sep = "")
      noteseq_h[j] <- read.csv(filepath, header = FALSE, sep = ",")
      
      if (!octavinf) {
        noteseq_h[[j]] <- substr(noteseq_h[[j]], 1, 1)
      }
      
      noteseq_h[[j]] <- paste(noteseq_h[[j]], collapse = "")
    }
    
    noteseq_h <- unlist(noteseq_h)
    
    # machine
    for (j in 1:length(noteseqfile_m)) {
      filepath <- paste(datadir[k], songdir[i], "/machine/", noteseqfile_m[j], sep = "")
      
      if (file.info(filepath)$size == 0) {
        noteseq_m[[j]] <- "XX"
      }else {
        noteseq_m[j] <- read.csv(filepath, header = FALSE, sep = ",")
        
        if (!octavinf) {
          noteseq_m[[j]] <- substr(noteseq_m[[j]], 1, 1)
        }
        
        noteseq_m[[j]] <- paste(noteseq_m[[j]], collapse = "")
      }
    }
    
    noteseq_m <- unlist(noteseq_m)
    
    # keep original seq
    noteseq_h_orig <- lapply(noteseq_h, function(x) strsplit(x, "(?<=[0-9a-zA-Z-]{2})", perl = TRUE)[[1]])
    noteseq_m_orig <- lapply(noteseq_m, function(x) strsplit(x, "(?<=[0-9a-zA-Z-]{2})", perl = TRUE)[[1]])
    
    # search best transposition
    for (j in 1:length(noteseq_h)) {
      PID_t <- matrix(0, nrow = length(TRANSPOSITION), ncol = 1)
      
      for (t in 1:length(TRANSPOSITION)) {
        noteseq_t <- paste(unlist(lapply(noteseq_h_orig[j][[1]], function(x) transposer(x, TRANSPOSITION[t]))), collapse = "")
        
        # map 2 character strings into 1 character element
        if (octavinf) {
          elemmap <- func_elemmap(c(noteseq_t, noteseq_m))
          charseq_t <- func_strmap(noteseq_t, elemmap)
          charseq_m <- func_strmap(noteseq_m, elemmap)
        }
        
        for (l in 1:length(charseq_m)) {
          charseq_j <- c(charseq_t, charseq_m[l])
          averagePID <- func_pid(charseq_j)
          
          PID_t[t] <- PID_t[t] + averagePID[1]
        }
      }
      
      PID_t <- PID_t/length(charseq_m)
      numshift <- which.max(PID_t)
      
      # main loop
      noteseq_t <- paste(unlist(lapply(noteseq_h_orig[j][[1]], function(x) transposer(x, TRANSPOSITION[numshift]))), collapse = "")
      
      # map 2 character strings into 1 character element
      if (octavinf) {
        elemmap <- func_elemmap(c(noteseq_t, noteseq_m))
        charseq_t <- func_strmap(noteseq_t, elemmap)
        charseq_m <- func_strmap(noteseq_m, elemmap)
      }
      
      for (l in 1:length(charseq_m)) {
        charseq_j <- c(charseq_t, charseq_m[l])
        
        # calculate average PID
        averagePID <- func_pid(charseq_j)
        
        # calculate average Levenstein distance
        averageLEVD <- func_levd(c(charseq_j[2], charseq_j[1]))
        
        # get multiple sequence alignment and calculate inter-rater reliability coefficients
        alignment_score <- func_msa(charseq_j)
        alignment <- alignment_score[[1]]
        similarity <- alignment_score[[2]]
        irrstat_i <- func_irr(alignment)
        
        # map back to the original 2 character strings
        if (octavinf) {
          alignment <- func_istrmap(alignment, elemmap)
        }
        
        # output MSA
        noteseqfile_j <- c(noteseqfile_h[j], noteseqfile_m[l])
        transcriber <- unlist(lapply(noteseqfile_j, function (x) strsplit(x, "_")[[1]][1]))
        alignment.df <- data.frame(transcriber, alignment)
        
        cat(paste(songdir[i], ": ", paste(transcriber, collapse = "_"), "\n", sep = ""))
        cat(paste(paste(alignment, collapse = "\n")), "\n", sep = "")
        cat("\n")
        
        output_filename <- paste(outputdir[k], "msa/", songdir[i], "_", paste(transcriber, collapse = "-"), "_msa.csv", sep = "")
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
        
        irrstat_i$machine <- transcriber[2]
        irrstat_i$human <- transcriber[1]
        irrstat_i$machinetype <- ddndd[which(ddndd[, 1] == transcriber[2]), 3]
        irrstat_i$transposition <- TRANSPOSITION[numshift]
        
        print(irrstat_i)
        cat("\n")
        
        irrstat <- rbind(irrstat, irrstat_i)
      }
    }
  }
  
  output_filepath <- paste(outputdir[k], outputfilename[k], sep = "")
  write.csv(irrstat, output_filepath, row.names = FALSE)
}
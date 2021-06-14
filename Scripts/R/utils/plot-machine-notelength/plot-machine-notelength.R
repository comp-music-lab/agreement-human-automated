# ---------------------------
library(ggplot2)

# ---------------------------
DATA_DIR <- c("../../data/test/human-machine-pairwise_agreement/with_unison/",
             "../../data/test/human-machine-pairwise_agreement/without_unison/")
WITH_UNISON = c(TRUE, FALSE)

OUTPUT_DIR <- "./"
OUTPUT_FILEID <- "notelength"

G_POINTSIZE <- 0.5
TITLE_TEXTSIZE <- 18
YAXIS_TITLE_TEXTSIZE <- 14
STRIP_TEXTSIZE <- 14
AXIS_TEXTSIZE <- 12
LEGEND_TEXTSIZE <- 13

FIG_WID <- 8.09 * 2
FIG_HEI <- 5.00

# ---------------------------
SONG_STYLE <- read.csv("../../song-style.csv", header = TRUE, sep = ",")

# ---------------------------
notelen_m.frame <- data.frame(song = character(), style = character(), transcriber = character(), length = numeric(), unison = logical())
n <- 0

notelen_h.frame <- data.frame(song = character(), style = character(), transcriber = character(), length = numeric(), unison = logical())
m <- 0

# ---------------------------
for (i in 1:length(DATA_DIR)) {
  SONG_DIR <- list.files(DATA_DIR[i], full.names = TRUE)
  SONG_NAME <- unlist(lapply(SONG_DIR, function(x) strsplit(x, "/")[[1]][7]))
  
  # ---------------------------
  for (j in 1:length(SONG_DIR)) {
    noteseq_m_file <- list.files(paste(SONG_DIR[j], "/machine/", sep = ""), full.names = TRUE)
    MACHINE <- unlist(lapply(unlist(lapply(noteseq_m_file, function(x) strsplit(x, "/")[[1]][9])), function(x) strsplit(x, "_")[[1]][1]))
    
    noteseq_h_file <- list.files(paste(SONG_DIR[j], "/human/", sep = ""), full.names = TRUE)
    HUMAN <- unlist(lapply(unlist(lapply(noteseq_h_file, function(x) strsplit(x, "/")[[1]][9])), function(x) strsplit(x, "_")[[1]][1]))
    
    STYLE <- SONG_STYLE[SONG_STYLE$songname == SONG_NAME[j], ]$style
    
    # ---------------------------
    for (k in 1:length(MACHINE)) {
      notelen_m.frame[n + 1, ]$song <- SONG_NAME[j]
      notelen_m.frame[n + 1, ]$style <- STYLE
      notelen_m.frame[n + 1, ]$transcriber <- MACHINE[k]
      notelen_m.frame[n + 1, ]$unison <- WITH_UNISON[i]
      
      if (file.info(noteseq_m_file[k])$size == 0) {
        notelen_m.frame[n + 1, ]$length <- 0
      } else {
        noteseq <- read.csv(noteseq_m_file[k], header = FALSE, sep = ",")
        notelen_m.frame[n + 1, ]$length <- dim(noteseq)[1]
      }
      
      n <- n + 1
    }
    
    for (k in 1:length(HUMAN)) {
      notelen_h.frame[m + 1, ]$song <- SONG_NAME[j]
      notelen_h.frame[m + 1, ]$style <- STYLE
      notelen_h.frame[m + 1, ]$transcriber <- HUMAN[k]
      notelen_h.frame[m + 1, ]$unison <- WITH_UNISON[i]
      
      if (file.info(noteseq_h_file[k])$size == 0) {
        notelen_h.frame[m + 1, ]$length <- 0
      } else {
        noteseq <- read.csv(noteseq_h_file[k], header = FALSE, sep = ",")
        notelen_h.frame[m + 1, ]$length <- dim(noteseq)[1]
      }
      
      m <- m + 1
    }
  }
}

# ---------------------------
notelen_m.frame$transcriber <- toupper(notelen_m.frame$transcriber)

notelen_m.frame[grepl(pattern = "TONY [(]FRAME[)]", x = notelen_m.frame$transcriber), ]$transcriber <- "pYIN"
notelen_m.frame[grepl(pattern = "TONY [(]NOTE[)]", x = notelen_m.frame$transcriber), ]$transcriber <- "TONY"
notelen_m.frame[grepl(pattern = "MELODIA", x = notelen_m.frame$transcriber), ]$transcriber <- "Melodia"
notelen_m.frame[grepl(pattern = "SS-PNN", x = notelen_m.frame$transcriber), ]$transcriber <- "SS-nPNN"
notelen_m.frame[grepl(pattern = "MADMOM", x = notelen_m.frame$transcriber), ]$transcriber <- "madmom"

notelen_h.frame[grepl(pattern = "Cons", x = notelen_h.frame$transcriber), ]$transcriber <- "Consensus"

# ---------------------------
for (i in 1:length(WITH_UNISON)) {
  notelen_m.frame_i <- notelen_m.frame[notelen_m.frame$unison == WITH_UNISON[i], ]
  notelen_h.frame_i <- notelen_h.frame[notelen_h.frame$unison == WITH_UNISON[i], ]
  
  HUMAN <- unique(notelen_h.frame_i$transcriber)
  STYLE <- unique(notelen_m.frame_i$style)
  
  if (WITH_UNISON[i]) {
    figuretype <- "\"unison\""
    figuretype_file <- " (with unison)"
  } else {
    figuretype <- "\"non-unison\""
    figuretype_file <- " (without unison)"
  }
  
  # ---------------------------
  SONG <- unique(notelen_m.frame_i$song)
  SONG_ID <- format(1:length(SONG), digits = 2, nsmall = 0)
  
  for (j in 1:length(SONG)) {
    notelen_m.frame_i[notelen_m.frame_i$song == SONG[j], ]$song <- SONG_ID[j]
    notelen_h.frame_i[notelen_h.frame_i$song == SONG[j], ]$song <- SONG_ID[j]
  }
  
  # ---------------------------
  for (j in 1:length(HUMAN)) {
    notelen_h.frame_i <- notelen_h.frame_i[notelen_h.frame_i$transcriber == HUMAN[j], ]
    
    # ---------------------------
    g <- ggplot()
    
    g <- g + geom_line(data = notelen_m.frame_i, aes(x = song, y = length, group = transcriber, colour = transcriber))
    g <- g + geom_point(data = notelen_m.frame_i, aes(x = song, y = length, group = transcriber, colour = transcriber), size = G_POINTSIZE)
    
    g <- g + geom_line(data = notelen_h.frame_i, aes(x = song, y = length, group = transcriber), colour = "black", linetype = "longdash")
    g <- g + geom_point(data = notelen_h.frame_i, aes(x = song, y = length, group = transcriber), colour = "black", size = G_POINTSIZE)
    
    # ---------------------------
    g <- g + facet_grid(. ~ style, scales = "free_x")
    
    # ---------------------------
    g <- g + xlab("Song ID") + ylab("Sequence length")
    g <- g + theme(axis.title.x = element_text(size = YAXIS_TITLE_TEXTSIZE), axis.title.y = element_text(size = YAXIS_TITLE_TEXTSIZE))
    g <- g + theme(axis.text.x = element_text(size = AXIS_TEXTSIZE), axis.text.y = element_text(size = AXIS_TEXTSIZE))
    g <- g + theme(strip.text.x = element_text(size = STRIP_TEXTSIZE))
    g <- g + theme(legend.text = element_text(size = LEGEND_TEXTSIZE), legend.title = element_text(size = LEGEND_TEXTSIZE)) + 
      labs(colour = "Automated method")
    
    titletext <- paste("Comparison of note sequence length (vs. ", HUMAN[j], ", ", figuretype, ")", sep = "")
    g <- g + ggtitle(titletext)
    g <- g + theme(plot.title = element_text(size = TITLE_TEXTSIZE, hjust = 0.5))
    
    # ---------------------------
    ggsave(paste(OUTPUT_DIR, OUTPUT_FILEID, "_", HUMAN[j], figuretype_file, ".png", sep = ""),
           plot = g, width = FIG_WID, height = FIG_HEI)
  }
}
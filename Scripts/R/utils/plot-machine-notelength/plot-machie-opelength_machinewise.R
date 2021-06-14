# ---------------------------
library(ggplot2)
library(grid)
library(gridExtra)

# ---------------------------
DATA_DIR <- c("../../output/human-machine-pairwise_agreement/paper/with_unison/human-machine-pairwise_agreement (with unison).csv",
              "../../output/human-machine-pairwise_agreement/paper/without_unison/human-machine-pairwise_agreement (without unison).csv")

OUTPUT_DIR <- "./"
OUTPUT_FILEID <- "opelength"
FIGURE_TYPE <- c("\"unison\"", "\"non-unison\"")
FIGURE_TYPE_FILE <- c(" (with unison)", " (without unison)")

G_POINTSIZE <- 0.5
TITLE_TEXTSIZE <- 18
YAXIS_TITLE_TEXTSIZE <- 14
STRIP_TEXTSIZE <- 14
AXIS_TEXTSIZE <- 12
LEGEND_TEXTSIZE <- 13

YLIM_MAX <- 20

FIG_WID <- 8.09 * 2
FIG_HEI <- 5.00

# Define local functions ---------------------------
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  
  legend <- tmp$grobs[[leg]]
  
  return(legend)
}

# ---------------------------
for (i in 1:length(DATA_DIR)) {
  alignmentdb <- read.csv(DATA_DIR[i], header = TRUE, sep = ",")
  
  # Edit name ---------------------------
  alignmentdb[grepl(pattern = "Cons", x = alignmentdb$human), ]$human <- "Consensus"
  
  HUMAN <- unique(alignmentdb$human)
  
  # ---------------------------
  alignmentdb$machine <- toupper(alignmentdb$machine)
  
  alignmentdb[grepl(pattern = "TONY [(]FRAME[)]", x = alignmentdb$machine), ]$machine <- "pYIN"
  alignmentdb[grepl(pattern = "TONY [(]NOTE[)]", x = alignmentdb$machine), ]$machine <- "TONY"
  alignmentdb[grepl(pattern = "MELODIA", x = alignmentdb$machine), ]$machine <- "Melodia"
  alignmentdb[grepl(pattern = "SS-PNN", x = alignmentdb$machine), ]$machine <- "SS-nPNN"
  alignmentdb[grepl(pattern = "MADMOM", x = alignmentdb$machine), ]$machine <- "madmom"
  
  MACHINE <- unique(alignmentdb$machine)
  
  # ---------------------------
  for (j in 1:length(HUMAN)) {
    for (k in 1:length(MACHINE)) {
      alignmentdb_j <- alignmentdb[alignmentdb$human == HUMAN[j] & alignmentdb$machine == MACHINE[k], ]
    
      levope.frame <- rbind(data.frame(song = alignmentdb_j$song, style = alignmentdb_j$songstyle,
                               metric = alignmentdb_j$averageINS, type = "Insertion"),
                            data.frame(song = alignmentdb_j$song, style = alignmentdb_j$songstyle,
                                       metric = alignmentdb_j$averageDEL, type = "Deletion"),
                            data.frame(song = alignmentdb_j$song, style = alignmentdb_j$songstyle,
                                       metric = alignmentdb_j$averageSUB, type = "Substitution"))
      # ---------------------------
      SONG <- unique(levope.frame$song)
      SONG_ID <- format(1:length(SONG), digits = 2, nsmall = 0)
      
      for (l in 1:length(SONG)) {
        levope.frame[levope.frame$song == SONG[l], ]$song <- SONG_ID[l]
      }
      
      # ---------------------------
      g <- ggplot(data = levope.frame)
      g <- g + geom_bar(aes(x = song, y = metric, fill = type), stat = "identity")
      
      # ---------------------------
      g <- g + facet_grid(. ~ style, scales = "free_x")
      
      # ---------------------------
      g <- g + xlab("Song ID") + ylab("Levenshtein distance") + ylim(0, YLIM_MAX)
      g <- g + theme(axis.title.x = element_text(size = YAXIS_TITLE_TEXTSIZE), axis.title.y = element_text(size = YAXIS_TITLE_TEXTSIZE))
      g <- g + theme(axis.text.x = element_text(size = AXIS_TEXTSIZE), axis.text.y = element_text(size = AXIS_TEXTSIZE))
      g <- g + theme(strip.text.x = element_text(size = STRIP_TEXTSIZE))
      g <- g + theme(legend.position = "bottom", legend.text = element_text(size = LEGEND_TEXTSIZE), legend.title = element_text(size = LEGEND_TEXTSIZE)) + 
        labs(fill = "Operation")
      
      # ---------------------------
      num_in <- sum(levope.frame[levope.frame$type == "Insertion", ]$metric)
      num_del <- sum(levope.frame[levope.frame$type == "Deletion", ]$metric)
      num_sub <- sum(levope.frame[levope.frame$type == "Substitution", ]$metric)
      L <- length(SONG)
      prop_in <- num_in / L
      prop_del <- num_del / L
      prop_sub <- num_sub / L
      levopeave.frame_all <- data.frame(song = "", metric = c(prop_in, prop_del, prop_sub), type = c("Insertion", "Deletion", "Substitution"), style = "All")
      
      idx <- levope.frame$style == "Solo singing with instruments"
      num_in <- sum(levope.frame[levope.frame$type == "Insertion" & idx, ]$metric)
      num_del <- sum(levope.frame[levope.frame$type == "Deletion" & idx, ]$metric)
      num_sub <- sum(levope.frame[levope.frame$type == "Substitution" & idx, ]$metric)
      L <- length(unique(levope.frame[levope.frame$style == "Solo singing with instruments", ]$song))
      prop_in <- num_in / L
      prop_del <- num_del / L
      prop_sub <- num_sub / L
      levopeave.frame_w <- data.frame(song = "", metric = c(prop_in, prop_del, prop_sub), type = c("Insertion", "Deletion", "Substitution"), style = "w/ inst.")
      
      idx <- levope.frame$style == "Solo singing without instruments"
      num_in <- sum(levope.frame[levope.frame$type == "Insertion" & idx, ]$metric)
      num_del <- sum(levope.frame[levope.frame$type == "Deletion" & idx, ]$metric)
      num_sub <- sum(levope.frame[levope.frame$type == "Substitution" & idx, ]$metric)
      L <- length(unique(levope.frame[levope.frame$style == "Solo singing without instruments", ]$song))
      prop_in <- num_in / L
      prop_del <- num_del / L
      prop_sub <- num_sub / L
      levopeave.frame_wo <- data.frame(song = "", metric = c(prop_in, prop_del, prop_sub), type = c("Insertion", "Deletion", "Substitution"), style = "w/o inst.")
      
      levopeave.frame <- rbind(levopeave.frame_all, levopeave.frame_w, levopeave.frame_wo)
      
      # ---------------------------
      g_p <- ggplot(data = levopeave.frame)
      g_p <- g_p + geom_bar(aes(x = song, y = metric, fill = type), stat = "identity")
      
      # ---------------------------
      g_p <- g_p + facet_grid(. ~ style, scales = "free_x")
      
      # ---------------------------
      g_p <- g_p + ylab("Average of operations") + ylim(0, YLIM_MAX)
      g_p <- g_p + theme(axis.title.x = element_blank(), axis.title.y = element_text(size = YAXIS_TITLE_TEXTSIZE))
      g_p <- g_p + theme(axis.text.y = element_text(size = AXIS_TEXTSIZE))
      g_p <- g_p + theme(strip.text.x = element_text(size = STRIP_TEXTSIZE))
      g_p <- g_p + theme(legend.position = "bottom", legend.text = element_text(size = LEGEND_TEXTSIZE), legend.title = element_text(size = LEGEND_TEXTSIZE)) + 
        labs(fill = "Operation")
      
      # Extract & clear legend ---------------------------
      legend <- get_legend(g)
      
      g <- g + theme(legend.position = "none")
      g_p <- g_p + theme(legend.position = "none")

      # ---------------------------
      title_text <- paste("Decomposition of Levenshtein distance", " - ", MACHINE[k], " (vs. ", HUMAN[j], ", ", FIGURE_TYPE[i], ")", sep = "")
      g = grid.arrange(arrangeGrob(g_p, g, nrow = 1, widths = c(1, 4)), legend, nrow = 2, heights = c(10, 1), 
                       top = textGrob(title_text, gp = gpar(fontsize = TITLE_TEXTSIZE)))
      
      ggsave(paste(OUTPUT_DIR, OUTPUT_FILEID, "_", HUMAN[j], "_", MACHINE[k], FIGURE_TYPE_FILE[i], ".png", sep = ""),
             plot = g, width = FIG_WID, height = FIG_HEI)
    }
  }
}
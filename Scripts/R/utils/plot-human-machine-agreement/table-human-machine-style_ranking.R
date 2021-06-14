# Load libraries ---------------------------
library(ggplot2)
library(grid)
library(gridExtra)

# Define constant variables ---------------------------
OUTPUT_DIR <- "../../output/figures/"
OUTPUT_FILEID <- "human-machine (paper)_ranking"

CSV_FILEPATH_WITH_UNI <- "../../output/human-machine-pairwise_agreement/paper/with_unison/human-machine-pairwise_agreement (with unison).csv"
CSV_FILEPATH_WITHOUT_UNI <- "../../output/human-machine-pairwise_agreement/paper/without_unison/human-machine-pairwise_agreement (without unison).csv"

METRIC_COLNAME <- c("coef", "averagePID")
METRIC_NAME <- c("Fleiss' Kappa", "Percent identity")

YAXIS_TITLE_TEXTSIZE <- 14
XAXIS_TEXTROTATE <- -75
TITLE_TEXTSIZE <- 18
STRIP_TEXTSIZE <- 14
AXIS_TEXTSIZE <- 12
LEGEND_TITLE_TEXTSIZE <- 13
LEGEND_TEXTSIZE <- 12

FIG_WID <- 8.09 * 2
FIG_HEI <- 5.00

# Define local functions ---------------------------
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  
  legend <- tmp$grobs[[leg]]
  
  return(legend)
}

# Load data ---------------------------
irrstat_i <- read.csv(CSV_FILEPATH_WITH_UNI, header = TRUE, sep = ",")
irrstat_i$withunison <- TRUE
irrstat_j <- read.csv(CSV_FILEPATH_WITHOUT_UNI, header = TRUE, sep = ",")
irrstat_j$withunison <- FALSE
irrstat <- rbind(irrstat_i, irrstat_j)

# ---------------------------
HUMAN <- unique(irrstat$human)
MACHINE <- unique(irrstat$machine)
UNISON <- unique(irrstat$withunison)

# ---------------------------
result <- data.frame(unison = logical(), human = character(), machine = character(), metric = character(),
                     mean_with_inst = numeric(), mean_without_inst = numeric())
counter <- 1

for (i in 1:length(UNISON)) {
  for (j in 1:length(HUMAN)) {
    for (k in 1:length(MACHINE)) {
      for (l in 1:length(METRIC_COLNAME)) {
        idx <- irrstat$withunison == UNISON[i] & irrstat$human == HUMAN[j] & irrstat$machine == MACHINE[k]
        metric <- cbind(irrstat[idx, ][METRIC_COLNAME[l]], irrstat[idx, ]["songstyle"])
        metric$ranking <- rank(metric[, 1])
        
        result_i <- aggregate(x = metric[, 3], by = list(metric$songstyle), mean)
        
        result[counter, ]$unison <- UNISON[i]
        result[counter, ]$human <- HUMAN[j]
        result[counter, ]$machine <- MACHINE[k]
        result[counter, ]$metric <- METRIC_NAME[l]
        result[counter, ]$mean_with_inst <- result_i[result_i$Group.1 == "Solo singing with instruments", ]$x
        result[counter, ]$mean_without_inst <- result_i[result_i$Group.1 == "Solo singing without instruments", ]$x
        
        counter <- counter + 1
      }
    }
  }
}

# ---------------------------
result$machine <- toupper(result$machine)
result[grepl("MELODIA", result$machine), ]$machine <- "Melodia"
result[grepl("MADMOM", result$machine), ]$machine <- "madmom"
result[grepl("SS-PNN", result$machine), ]$machine <- "SS-nPNN"
result[grepl("TONY [(]FRAME[)]", result$machine), ]$machine <- "pYIN"
result[grepl("TONY [(]NOTE[)]", result$machine), ]$machine <- "TONY"

G_X_ORDER <- c("CREPE", "pYIN", "SPICE", "TONY", "AD-NNMF", "madmom", "Melodia", "OAF", "SS-nPNN", "STF")

result$unison_chr <- ""
result[result$unison == TRUE, ]$unison_chr <- "\"unison\""
result[result$unison == FALSE, ]$unison_chr <- "\"non-unison\""

# ---------------------------
df_with <- cbind(result["human"], result["machine"], result["metric"], result["mean_with_inst"], result["unison_chr"])
names(df_with)[4] <- "mean_rank"
df_with$style <- "Solo singing with instruments"

df_without <- cbind(result["human"], result["machine"], result["metric"], result["mean_without_inst"], result["unison_chr"])
names(df_without)[4] <- "mean_rank"
df_without$style <- "Solo singing without instruments"

df <- rbind(df_with, df_without)

# ---------------------------
g_list <- vector(mode = "list", length = length(METRIC_NAME))

for (i in 1:length(METRIC_NAME)) {
  df_i <- df[df$human == "Cons" & df$metric == METRIC_NAME[i], ]
  
  # ---------------------------
  g <- ggplot(data = df_i)
  g <- g + geom_bar(aes(x = machine, y = mean_rank, group = style, fill = style), stat = "identity", position = "dodge")
  
  # ---------------------------
  g <- g + facet_grid(. ~ unison_chr)
  g <- g + scale_x_discrete(limits = G_X_ORDER)
  
  # ---------------------------
  ylab_text <- paste("Mean of score ranking by style", sep = "")
  
  g <- g + ggtitle(METRIC_NAME[i]) + theme(plot.title = element_text(size = TITLE_TEXTSIZE, hjust = 0.5))
  g <- g + ylab(ylab_text) + theme(axis.title.x = element_blank(), axis.title.y = element_text(size = YAXIS_TITLE_TEXTSIZE))
  g <- g + theme(axis.text.x = element_text(angle = XAXIS_TEXTROTATE, vjust = 0.5, size = AXIS_TEXTSIZE),
                 axis.text.y = element_text(size = AXIS_TEXTSIZE))
  g <- g + theme(strip.text.x = element_text(size = STRIP_TEXTSIZE))
  g <- g + theme(legend.position = "bottom", legend.text = element_text(size = LEGEND_TEXTSIZE), legend.title = element_text(size = LEGEND_TITLE_TEXTSIZE))
  g <- g + labs(fill = "Style")
  
  # ---------------------------
  g_list[[i]] <- g
}

# ---------------------------
legend <- get_legend(g_list[[1]])

for (j in 1:length(METRIC_NAME)) {
  g_list[[j]] <- g_list[[j]] + theme(legend.position = "none")
}

# Output plot ---------------------------
g = grid.arrange(arrangeGrob(g_list[[1]], g_list[[2]], nrow = 1), legend, nrow = 2, heights = c(10, 1),
                 top = textGrob("Consensus vs. Automated methods", gp = gpar(fontsize = TITLE_TEXTSIZE + 2)))

ggsave(paste(OUTPUT_DIR, OUTPUT_FILEID, ".png", sep = ""),
       plot = g, width = FIG_WID, height = FIG_HEI)
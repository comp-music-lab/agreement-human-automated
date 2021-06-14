# Load libraries ---------------------------
library(ggplot2)
library(grid)
library(gridExtra)

# Define constant variables ---------------------------
PAIR_CSV_FILEPATH <- "../../output/transcriber-pairwise_agreement/paper/transcriber-pairwise_agreement.csv"
OUTPUT_DIR <- "../../output/figures/"
OUTPUT_FILEID <- "human-human (calibrated)"

FDR_RESULT <- "../../test result/fdr_test.csv"

METRIC_COL <- c("coef", "averagePID")
METRIC_NAME <- c("Fleiss' Kappa", "Percent identity")

LINESIZE <- 0.25
LINESTAT <- "median"

CI_WID <- 0.1

G_JITTER_WID <- 0.15
G_V_ADJUST <- 0.5
MARKER_SIZE <- 2
YAXIS_TITLE_TEXTSIZE <- 14
XAXIS_TEXTROTATE <- -30
TITLE_TEXTSIZE <- 18
STRIP_TEXTSIZE <- 10
AXIS_TEXTSIZE <- 12

GRID_ORDER <- c('Pairwise','vs. our consensus','vs. NHS consensus', 'Our consensus vs. NHS')

PAIRWISE <- c("A-B", "B-C", "A-C")
OUR_CONS <- c("A-Cons", "B-Cons", "C-Cons")
THEIR_CONS <- c("A-NHS", "B-NHS", "C-NHS")

FIG_WID <- 8.09 * 2
FIG_HEI <- 5.00

# Define local functions ---------------------------
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  
  legend <- tmp$grobs[[leg]]
  
  return(legend)
}

# Median's 95% CI parameters ---------------------------
al <- 0.05
C <- qnorm(1 - al/2, mean = 0, sd = 1)

# Load data ---------------------------
irrstat <- read.csv(PAIR_CSV_FILEPATH, header = TRUE, sep = ",")
testresult.frame <- read.csv(FDR_RESULT, header = TRUE, sep = ",")
rejection <- testresult.frame[1, ]$reject

# Transform (negative Levenshtein distance)---------------------------
irrstat$averageLEVD <- -irrstat$averageLEVD

# Transform (replace)---------------------------
irrstat$transcriber <- gsub("_", "-", irrstat$transcriber)

irrstat$transcriber <- gsub("Pub", "NHS", irrstat$transcriber)

irrstat$transcriber <- gsub("Cons-A", "A-Cons", irrstat$transcriber)
irrstat$transcriber <- gsub("Cons-B", "B-Cons", irrstat$transcriber)
irrstat$transcriber <- gsub("Cons-C", "C-Cons", irrstat$transcriber)
irrstat$transcriber <- gsub("NHS-C", "C-NHS", irrstat$transcriber)

# Transform (addition)---------------------------
irrstat$type <- ""
irrstat[irrstat$transcriber %in% PAIRWISE, ]$type <- GRID_ORDER[1]
irrstat[irrstat$transcriber %in% OUR_CONS, ]$type <- GRID_ORDER[2]
irrstat[irrstat$transcriber %in% THEIR_CONS, ]$type <- GRID_ORDER[3]
irrstat[irrstat$transcriber == "Cons-NHS", ]$type <- GRID_ORDER[4]

# Plot---------------------------
g_list <- vector(mode = "list", length = length(METRIC_COL))

for (j in 1:length(METRIC_COL)) {
  # Extract data ---------------------------
  df_ij <- cbind(irrstat["transcriber"], irrstat["song"], irrstat["type"], irrstat["songstyle"])
  df_ij$metric <- irrstat[[METRIC_COL[j]]]
  
  # CI ---------------------------
  PAIR <- unique(df_ij$transcriber)
  CI.frame <- data.frame(transcriber = PAIR, type = "", se_u = 0, se_l = 0, median = 0)
  
  for(i in 1:length(PAIR)) {
    metric <- df_ij[df_ij$transcriber == PAIR[i], ]$metric
    metric <- sort(metric)
    
    n <- length(metric)
    r = round(n/2 - C*sqrt(n)/2)
    s = round(1 + n/2 + C*sqrt(n)/2)
    
    CI.frame[i, ]$se_l <- metric[r]
    CI.frame[i, ]$se_u <- metric[s]
    
    CI.frame[i, ]$type <- df_ij[df_ij$transcriber == PAIR[i], ]$type[1]
    
    CI.frame[i, ]$median <- median(metric)
  }
  
  # Plot (data) ---------------------------
  g <- ggplot(data = df_ij)
  
  g <- g + geom_violin(aes(x = transcriber, y = metric),
                       draw_quantiles = NULL, trim = TRUE, scale = "count", adjust = G_V_ADJUST)
  
  if (METRIC_NAME[j] == "Fleiss' Kappa") {
    g <- g + 
      geom_jitter(aes(x = transcriber, y = metric, color = songstyle), size = MARKER_SIZE, width = G_JITTER_WID, alpha = 0.75) + 
      scale_shape_manual(values = c(8, 16))
  } else {
    g <- g + 
      geom_jitter(aes(x = transcriber, y = metric, color = songstyle), shape = 16, size = MARKER_SIZE, width = G_JITTER_WID, alpha = 0.75)
  }
  
  g <- g + stat_summary(aes(x = transcriber, y = metric, group = 1), fun = LINESTAT, colour = "black", geom = "point", group = 1)
  
  g <- g + geom_errorbar(data = CI.frame, aes(x = transcriber, ymin = se_l, ymax = se_u), width = CI_WID)
  
  # Sign test---------------------------
  if (rejection && METRIC_NAME[j] == "Fleiss' Kappa") {
    X <- df_ij[df_ij$transcriber == "Cons-NHS", ]$metric
    
    df_text <- data.frame(x = "Cons-NHS", y = median(X), type = "Our consensus vs. NHS")
      
    g <- g + geom_point(data = df_text, aes(x = x, y = y, shape = "3"), size = MARKER_SIZE)
  }
  
  # Plot (layout) ---------------------------
  g <- g + facet_grid(. ~ factor(type, levels = GRID_ORDER), scales = "free_x")
  
  if (METRIC_NAME[j] == "Percent identity") {
    g <- g + ylim(-1, 101)
  }
  
  g <- g + theme(plot.title = element_text(hjust = 0.5, size = TITLE_TEXTSIZE))
  g <- g + ylab(METRIC_NAME[j]) + theme(axis.title.x = element_blank(), axis.title.y = element_text(size = YAXIS_TITLE_TEXTSIZE))
  g <- g + theme(axis.text.x = element_text(angle = XAXIS_TEXTROTATE, vjust = 0.5, size = AXIS_TEXTSIZE),
                 axis.text.y = element_text(size = AXIS_TEXTSIZE))
  g <- g + theme(strip.text.x = element_text(size = STRIP_TEXTSIZE))
  
  g <- g + guides(shape = FALSE) + labs(colour = "Style") + theme(legend.position = "bottom")
  
  # Output data ---------------------------
  g_list[[j]] <- g
  
  print(CI.frame)
}

# Extract & clear legend ---------------------------
legend <- get_legend(g_list[[1]])

for (j in 1:length(METRIC_COL)) {
  g_list[[j]] <- g_list[[j]] + theme(legend.position = "none")
}

# Output plot ---------------------------
g = grid.arrange(arrangeGrob(g_list[[1]], g_list[[2]], nrow = 1), legend, nrow = 2, heights = c(10, 1),
                 top = textGrob("Human vs. Human (\"unison\")", gp = gpar(fontsize = TITLE_TEXTSIZE)))

ggsave(paste(OUTPUT_DIR, OUTPUT_FILEID, " (with unison).png", sep = ""),
       plot = g, width = FIG_WID, height = FIG_HEI)
# Load libraries ---------------------------
library(ggplot2)
library(grid)
library(gridExtra)

# Define constant variables ---------------------------
P_ALPHA <- 0.05
POSTERIOR_THRESH <- 0.8

OUTPUT_DIR <- "../../output/figures/"
OUTPUT_FILEID <- "human-machine (paper)"

CSV_FILEPATH_WITH_UNI <- "../../output/human-machine-pairwise_agreement/paper/with_unison/human-machine-pairwise_agreement (with unison).csv"
CSV_FILEPATH_WITHOUT_UNI <- "../../output/human-machine-pairwise_agreement/paper/without_unison/human-machine-pairwise_agreement (without unison).csv"

TESTRESULT_WITH_UNI <- "../../test result/human-machine_two-sample_test_with_unison.csv"
TESTRESULT_WITHOUT_UNI <- "../../test result/human-machine_two-sample_test_without_unison.csv"
FDR_RESULT <- "../../test result/fdr_test.csv"

#METRIC <- c("Levenshtein")
#METRIC_FULL <- c("Negative Levenshtein distance")

METRIC <- c("Kappa", "PID")
METRIC_FULL <- c("Fleiss' Kappa", "Percent identity")

CI_WID <- 0.6

G_TEXT_TEXTSIZE <- 3.4

G_V_ADJUST <- c(0.5, 1.0)
YAXIS_TITLE_TEXTSIZE <- 14
XAXIS_TEXTROTATE <- -75
TITLE_TEXTSIZE <- 18
STRIP_TEXTSIZE <- 14
AXIS_TEXTSIZE <- 12
LEGEND_TEXTSIZE <- 13

LINESTAT <- "median"

FIG_WID <- 8.09 * 2
FIG_HEI <- 5.00

# Median's 95% CI parameters ---------------------------
al <- 0.05
C <- qnorm(1 - al/2, mean = 0, sd = 1)

# Define local functions ---------------------------
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  
  legend <- tmp$grobs[[leg]]
  
  return(legend)
}

# Load data ---------------------------
testresult_i <- read.csv(TESTRESULT_WITH_UNI, header = TRUE, sep = ",")
testresult_i$withunison <- TRUE
testresult_j <- read.csv(TESTRESULT_WITHOUT_UNI, header = TRUE, sep = ",")
testresult_j$withunison <- FALSE
testresult <- rbind(testresult_i, testresult_j)

fdrresult.frame <- read.csv(FDR_RESULT, header = TRUE, sep = ",")
fdrresult.frame <- fdrresult.frame[2:dim(fdrresult.frame)[1], ]

# Add label text ---------------------------
testresult$labeltext <- unlist(lapply(round(testresult$A, 2), toString))

#idx <- testresult$pval < P_ALPHA & testresult$withunison == TRUE & testresult$var == "coef"
#testresult[idx, ]$labeltext <- paste(testresult[idx, ]$labeltext, "*", sep = "")

idx <- testresult$var == "coef" & testresult$human == "Cons" & testresult$withunison == TRUE
stopifnot(all(testresult[idx, ]$machine == unlist(lapply(fdrresult.frame$pair, function(x) strsplit(x, "_")[[1]][2]))))
testresult[idx & fdrresult.frame$reject, ]$labeltext <- paste(testresult[idx & fdrresult.frame$reject, ]$labeltext, "*", sep = "")

idx <- testresult$posterior_H1 > POSTERIOR_THRESH & testresult$withunison == TRUE & testresult$var == "coef"
testresult[idx, ]$labeltext <- paste(testresult[idx, ]$labeltext, "^", sep = "")

# Edit name ---------------------------
testresult[grepl(pattern = "Cons", x = testresult$human), ]$human <- "Consensus"

testresult$machine <- toupper(testresult$machine)
testresult[grepl(pattern = "TONY [(]FRAME[)]", x = testresult$machine), ]$machine <- "pYIN"
testresult[grepl(pattern = "MADMOM", x = testresult$machine), ]$machine <- "madmom"
testresult[grepl(pattern = "MELODIA", x = testresult$machine), ]$machine <- "Melodia"
testresult[grepl(pattern = "SS-PNN", x = testresult$machine), ]$machine <- "SS-nPNN"

testresult[grepl(pattern = "coef", x = testresult$var) & testresult$withunison == TRUE, ]$var <- "Kappa (w/ uni.)"
testresult[grepl(pattern = "averagePID", x = testresult$var) & testresult$withunison == TRUE, ]$var <- "PID (w/ uni.)"
testresult[grepl(pattern = "averageLEVD", x = testresult$var) & testresult$withunison == TRUE, ]$var <- "Levenshtein (w/ uni.)"
testresult[grepl(pattern = "coef", x = testresult$var) & testresult$withunison == FALSE, ]$var <- "Kappa (w/o uni.)"
testresult[grepl(pattern = "averagePID", x = testresult$var) & testresult$withunison == FALSE, ]$var <- "PID (w/o uni.)"
testresult[grepl(pattern = "averageLEVD", x = testresult$var) & testresult$withunison == FALSE, ]$var <- "averageLEVD (w/o uni.)"

names(testresult)[names(testresult) == "var"] <- "metric"

# Load data ---------------------------
irrstat_i <- read.csv(CSV_FILEPATH_WITH_UNI, header = TRUE, sep = ",")
irrstat_i$withunison <- TRUE
irrstat_j <- read.csv(CSV_FILEPATH_WITHOUT_UNI, header = TRUE, sep = ",")
irrstat_j$withunison <- FALSE
irrstat <- rbind(irrstat_i, irrstat_j)

# Edit name ---------------------------
irrstat[grepl(pattern = "Cons", x = irrstat$human), ]$human <- "Consensus"

irrstat$machine <- toupper(irrstat$machine)
irrstat[grepl(pattern = "TONY [(]NOTE[)]", x = irrstat$machine), ]$machine <- "TONY"
irrstat[grepl(pattern = "TONY [(]FRAME[)]", x = irrstat$machine), ]$machine <- "pYIN"
irrstat[grepl(pattern = "MELODIA", x = irrstat$machine), ]$machine <- "Melodia"
irrstat[grepl(pattern = "SS-PNN", x = irrstat$machine), ]$machine <- "SS-nPNN"
irrstat[grepl(pattern = "MADMOM", x = irrstat$machine), ]$machine <- "madmom"

irrstat[grepl(pattern = "Pub", x = irrstat$human), ]$human <- "NHS"
testresult[grepl(pattern = "Pub", x = testresult$human), ]$human <- "NHS"

# Use negative Levenshtein distance
irrstat$averageLEVD <- -irrstat$averageLEVD

# Plot order (x-axis) ---------------------------
#score <- data.frame(machine = irrstat$machine, SUM = irrstat$coef)
#aggsum <- aggregate(x = score$SUM, by = list(score$machine), FUN = sum)
#aggsum <- aggsum[order(aggsum$x, decreasing = TRUE), ]
#G_X_ORDER <- c(aggsum$Group.1)

score <- data.frame(machine = irrstat$machine, score = irrstat$coef, unison = irrstat$withunison, transcriber = irrstat$human)
MACHINE <- unique(score$machine)
machine_median <- data.frame(machine = MACHINE, median = unlist(lapply(MACHINE, function(x) median(score[score$machine == x & score$unison == TRUE & score$transcriber == "Consensus", ]$score))))
machine_median <- machine_median[order(machine_median$median, decreasing = TRUE), ]
G_X_ORDER <- c(machine_median$machine)

G_X_ORDER <- c("TONY", G_X_ORDER[G_X_ORDER != "TONY"])

# Extract human transcribers and create figures transcriber-wise---------------------------
TRANSCRIBER <- unique(irrstat$human)
MACHINE <- unique(irrstat$machine)
MACHINE_TYPE <- as.data.frame(unique(cbind(machine = irrstat$machine, machinetype = irrstat$machinetype)))

for (i in 1:length(TRANSCRIBER)) {
  irrstat_i <- irrstat[irrstat$human == TRANSCRIBER[i], ]
  
  # Transform to algorithm-score-metric format---------------------------
  irrstat_withuni <- irrstat_i[irrstat_i$withunison == TRUE, ]
  irrstat_withoutuni <- irrstat_i[irrstat_i$withunison == FALSE, ]
  
  scoremat <- data.frame(machine = character(), song = character(), songstyle = character(), score = double(), metric = character(), machinetype = character())
  
  for (j in 1:length(MACHINE)) {
    irrstat_j <- irrstat_withuni[irrstat_withuni$machine == MACHINE[j], ]
    kappa <- data.frame(machine = MACHINE[j], song = irrstat_j$song, songstyle = irrstat_j$songstyle, score = irrstat_j$coef, metric = "Kappa (w/ uni.)")
    PID <- data.frame(machine = MACHINE[j], song = irrstat_j$song, songstyle = irrstat_j$songstyle, score = irrstat_j$averagePID, metric = "PID (w/ uni.)")
    LEVD <- data.frame(machine = MACHINE[j], song = irrstat_j$song, songstyle = irrstat_j$songstyle, score = irrstat_j$averageLEVD, metric = "Levenshtein (w/ uni.)")
    
    tmp <- rbind(kappa, PID, LEVD)
    tmp$machinetype <- MACHINE_TYPE[MACHINE_TYPE$machine == MACHINE[j], ]$machinetype
    scoremat <- rbind(scoremat, tmp)
    
    irrstat_j <- irrstat_withoutuni[irrstat_withoutuni$machine == MACHINE[j], ]
    kappa <- data.frame(machine = MACHINE[j], song = irrstat_j$song, songstyle = irrstat_j$songstyle, score = irrstat_j$coef, metric = "Kappa (w/o uni.)")
    PID <- data.frame(machine = MACHINE[j], song = irrstat_j$song, songstyle = irrstat_j$songstyle, score = irrstat_j$averagePID, metric = "PID (w/o uni.)")
    LEVD <- data.frame(machine = MACHINE[j], song = irrstat_j$song, songstyle = irrstat_j$songstyle, score = irrstat_j$averageLEVD, metric = "Levenshtein (w/o uni.)")
    
    tmp <- rbind(kappa, PID, LEVD)
    tmp$machinetype <- MACHINE_TYPE[MACHINE_TYPE$machine == MACHINE[j], ]$machinetype
    scoremat <- rbind(scoremat, tmp)
  }
  
  scoremat$transcriber <- TRANSCRIBER[i]
  
  # Create plot by metric type ---------------------------
  glist <- vector(mode = "list", length = length(METRIC))
  
  for (j in 1:length(METRIC)) {
    scoremat_j <- scoremat[grepl(pattern = METRIC[j], x = scoremat$metric), ]
    
    # CI ---------------------------
    MACHINE <- unique(scoremat_j$machine)
    METRIC_j <- unique(scoremat_j$metric)
    CI.frame <- data.frame(machine = character(), metric = character(), se_u = numeric(), se_l = numeric(), median = numeric())
    m <- 1
    
    for (l in 1:length(METRIC_j)) {
      for (k in 1:length(MACHINE)) {
        idx <- scoremat_j$machine == MACHINE[k] & scoremat_j$metric == METRIC_j[l]
        
        score <- scoremat_j[idx, ]$score
        score <- sort(score)
        
        n <- length(score)
        r = round(n/2 - C*sqrt(n)/2)
        s = round(1 + n/2 + C*sqrt(n)/2)
        
        CI.frame[m, ]$se_l <- score[r]
        CI.frame[m, ]$se_u <- score[s]
        
        CI.frame[m, ]$machine <- MACHINE[k]
        CI.frame[m, ]$metric <- METRIC_j[l]
        
        CI.frame[m, ]$median <- median(score)
          
        m <- m + 1
      }
    }
    
    # Plot (data) ---------------------------
    g <- ggplot(data = scoremat_j)
    
    g <- g + 
      geom_violin(aes(x = machine, y = score, fill = machinetype),
                  draw_quantiles = NULL, trim = TRUE, scale = "count", adjust = G_V_ADJUST[j], alpha = 0.15) +
      scale_fill_manual(values = c("#000080", "#ff4500"))
    
    #g <- g + 
    #  geom_point(aes(x = machine, y = score, shape = songstyle), color = "tomato", alpha = 0.75)
    g <- g + 
        geom_jitter(aes(x = machine, y = score, shape = songstyle, color = songstyle), alpha = 0.75, width = 0.2)
    
    g <- g + stat_summary(aes(x = machine, y = score, group = 1), fun = LINESTAT, colour = "black", geom = "point", group = 1)
    
    g <- g + geom_errorbar(data = CI.frame, aes(x = machine, ymin = se_l, ymax = se_u), width = CI_WID, alpha = 0.8)
    
    # Plot (dashed line) ---------------------------
    if (METRIC[j] == "Kappa") {
      g <- g + geom_hline(yintercept = 0, linetype = "dashed")
    }
    
    # Plot (test result) ---------------------------
    if (METRIC[j] == "Kappa") {
      Y_DELTA <- 0.1
    } else if(METRIC[j] == "PID") {
      Y_DELTA <- 4
    } else if(METRIC[j] == "Levenshtein") {
      Y_DELTA <- -1
    }
    
    if (METRIC[j] == "Kappa" && TRANSCRIBER[i] == "UNUSED") {
      testresult_j <- testresult[grepl(pattern = METRIC[j], x = testresult$metric) & testresult$human == TRANSCRIBER[i], ]
      testresult_j$score <- max(scoremat_j$score) + Y_DELTA
      
      testresult_j <- testresult_j[grepl(pattern = "Kappa [(]w/ uni[.)]", x = testresult_j$metric), ]
      g <- g + geom_text(data = testresult_j, aes(x = machine, y = score, label = labeltext), size = G_TEXT_TEXTSIZE, fontface = "italic")
      
    }
      
    # Plot (layout) ---------------------------
    metric_unison <- unique(scoremat_j$metric)
    metric_with <- metric_unison[grep(pattern = "w/ uni.", x = metric_unison, ignore.case = FALSE)]
    metric_without <- metric_unison[grep(pattern = "w/o uni.", x = metric_unison, ignore.case = FALSE)]
    labeli <- setNames(c("\"unison\"", "\"non-unison\""), c(metric_with, metric_without))
    
    g <- g + facet_grid(. ~ metric, labeller = as_labeller(labeli))
    g <- g + scale_x_discrete(limits = G_X_ORDER)
      
    g <- g + ylab(METRIC_FULL[j]) + theme(axis.title.x = element_blank(), axis.title.y = element_text(size = YAXIS_TITLE_TEXTSIZE))
    g <- g + theme(axis.text.x = element_text(angle = XAXIS_TEXTROTATE, vjust = 0.5, size = AXIS_TEXTSIZE),
                   axis.text.y = element_text(size = AXIS_TEXTSIZE))
    
    g <- g + theme(strip.text.x = element_text(size = STRIP_TEXTSIZE))
    
    g <- g + 
      theme(legend.position = "bottom", legend.text = element_text(size = LEGEND_TEXTSIZE), legend.title = element_text(size = LEGEND_TEXTSIZE)) + 
      guides(colour = guide_legend(nrow = 1, byrow = TRUE), shape = guide_legend(nrow = 1, byrow = TRUE), fill = guide_legend(nrow = 1, byrow = TRUE)) + 
      labs(colour = "Accompaniment", shape = "Accompaniment", fill = "Type")
    
    # Output data ---------------------------
    glist[[j]] <- g
    
    print(TRANSCRIBER[i])
    print(CI.frame)
  }
  
  # Extract & clear legend ---------------------------
  legend <- get_legend(glist[[1]])
  
  for (j in 1:length(METRIC)) {
    glist[[j]] <- glist[[j]] + theme(legend.position = "none")
  }
  
  # Output plot ---------------------------
  #g = grid.arrange(arrangeGrob(glist[[1]], nrow = 1), legend, nrow = 2, heights = c(10, 1), 
  #                 top = textGrob(paste(TRANSCRIBER[i], " vs. Automated methods", sep = ""), gp = gpar(fontsize = TITLE_TEXTSIZE)))
  g = grid.arrange(arrangeGrob(glist[[1]], glist[[2]], nrow = 1), legend, nrow = 2, heights = c(10, 1),
                   top = textGrob(paste(TRANSCRIBER[i], " vs. Automated methods", sep = ""), gp = gpar(fontsize = TITLE_TEXTSIZE)))
  
  ggsave(paste(OUTPUT_DIR, OUTPUT_FILEID, "_", TRANSCRIBER[i], ".png", sep = ""),
         plot = g, width = FIG_WID, height = FIG_HEI)
}
# Load libraries ---------------------------
library(ggplot2)
library(grid)
library(gridExtra)

# Define constant variables ---------------------------
OUTPUT_DIR <- "../../output/figures/"

AGREEMENT_HH <- "../../output/transcriber-pairwise_agreement/paper/transcriber-pairwise_agreement.csv"

AGREEMENT_HM_WITH_UNI <- "../../output/human-machine-pairwise_agreement/paper/with_unison/human-machine-pairwise_agreement (with unison).csv"
AGREEMENT_HM_WITHOUT_UNI <- "../../output/human-machine-pairwise_agreement/paper/without_unison/human-machine-pairwise_agreement (without unison).csv"

TESTRESULT_WITH_UNI <- "../../test result/human-machine_two-sample_test_with_unison.csv"
TESTRESULT_WITHOUT_UNI <- "../../test result/human-machine_two-sample_test_without_unison.csv"

FDRRESULT <- "../../test result/fdr_test.csv"

GRID_ORDER_HH <- c('Pairwise','vs. Our consensus','vs. NHS consensus', 'Our consensus vs. NHS')
PAIRWISE <- c("A-B", "B-C", "A-C")
OUR_CONS <- c("A-Cons", "B-Cons", "C-Cons")
THEIR_CONS <- c("A-NHS", "B-NHS", "C-NHS")

METRIC_COL <- c("coef", "averagePID")
METRIC_NAME <- c("Fleiss' Kappa", "Percent identity")

SUMMARY_FUN <- "median"
ERRORBAR_WID_HH <- 0.1
ERRORBAR_WID_HM <- 0.3
JITTER_WID <- 0.15
VIOLIN_ADJUST_HH <- 0.5
VIOLIN_ADJUST_HM <- c(0.5, 1.0)
MARKER_SIZE <- 2
TEXT_TEXTSIZE <- 4.0
TEXT_YPOS <- 1.1

TITLE_TEXTSIZE <- 18
STRIP_TEXTSIZE <- 12
AXIS_TEXTSIZE <- 12
XAXIS_TEXTROTATE <- -30
YAXIS_TITLE_TEXTSIZE <- 14
LEGEND_TEXTSIZE <- 12

KAPPA_YLIM <- c(-1.05, 1.15)
PID_YLIM <- c(-5, 105)

FIG_WID <- 8.09
FIG_HEI <- 5.00 * 2

# Median's 95% CI parameters ---------------------------
al <- 0.05
C <- qnorm(1 - al/2, mean = 0, sd = 1)

# Define local functions ---------------------------
get_legend <- function(myggplot) {
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  
  legend <- tmp$grobs[[leg]]
  
  return(legend)
}

# Load files ---------------------------
agreementdb_hh <- read.csv(AGREEMENT_HH, header = TRUE, sep = ",")

agreementdb_hm_i <- read.csv(AGREEMENT_HM_WITH_UNI, header = TRUE, sep = ",")
agreementdb_hm_i$withunison <- TRUE
agreementdb_hm_j <- read.csv(AGREEMENT_HM_WITHOUT_UNI, header = TRUE, sep = ",")
agreementdb_hm_j$withunison <- FALSE
agreementdb_hm <- rbind(agreementdb_hm_i, agreementdb_hm_j)

testresultdb_i <- read.csv(TESTRESULT_WITH_UNI, header = TRUE, sep = ",")
testresultdb_i$withunison <- TRUE
testresultdb_j <- read.csv(TESTRESULT_WITHOUT_UNI, header = TRUE, sep = ",")
testresultdb_j$withunison <- FALSE
testresultdb <- rbind(testresultdb_i, testresultdb_j)

fdrresultdb <- read.csv(FDRRESULT, header = TRUE, sep = ",")
fdrresultdb_hh <- fdrresultdb[1, ]
fdrresultdb_hm <- fdrresultdb[2:dim(fdrresultdb)[1], ]

# Edit name (1) ---------------------------
agreementdb_hh$transcriber <- gsub("_", "-", agreementdb_hh$transcriber)

agreementdb_hh$transcriber <- gsub("Pub", "NHS", agreementdb_hh$transcriber)

agreementdb_hh$transcriber <- gsub("Cons-A", "A-Cons", agreementdb_hh$transcriber)
agreementdb_hh$transcriber <- gsub("Cons-B", "B-Cons", agreementdb_hh$transcriber)
agreementdb_hh$transcriber <- gsub("Cons-C", "C-Cons", agreementdb_hh$transcriber)
agreementdb_hh$transcriber <- gsub("NHS-C", "C-NHS", agreementdb_hh$transcriber)

agreementdb_hh$pairtype <- ""
agreementdb_hh[agreementdb_hh$transcriber %in% PAIRWISE, ]$pairtype <- GRID_ORDER_HH[1]
agreementdb_hh[agreementdb_hh$transcriber %in% OUR_CONS, ]$pairtype <- GRID_ORDER_HH[2]
agreementdb_hh[agreementdb_hh$transcriber %in% THEIR_CONS, ]$pairtype <- GRID_ORDER_HH[3]
agreementdb_hh[agreementdb_hh$transcriber == "Cons-NHS", ]$pairtype <- GRID_ORDER_HH[4]

# Edit name (2) ---------------------------
agreementdb_hm$machine <- toupper(agreementdb_hm$machine)
agreementdb_hm[grepl(pattern = "TONY [(]FRAME[)]", x = agreementdb_hm$machine), ]$machine <- "pYIN"
agreementdb_hm[grepl(pattern = "TONY [(]NOTE[)]", x = agreementdb_hm$machine), ]$machine <- "TONY"
agreementdb_hm[grepl(pattern = "MELODIA", x = agreementdb_hm$machine), ]$machine <- "Melodia"
agreementdb_hm[grepl(pattern = "MADMOM", x = agreementdb_hm$machine), ]$machine <- "madmom"
agreementdb_hm[grepl(pattern = "SS-PNN", x = agreementdb_hm$machine), ]$machine <- "SS-nPNN"

agreementdb_hm[grepl(pattern = "Pub", x = agreementdb_hm$human), ]$human <- "NHS"
agreementdb_hm[grepl(pattern = "Cons", x = agreementdb_hm$human), ]$human <- "Consensus"

# Edit name (3) ---------------------------
testresultdb$machine <- toupper(testresultdb$machine)
testresultdb[grepl(pattern = "TONY [(]FRAME[)]", x = testresultdb$machine), ]$machine <- "pYIN"
testresultdb[grepl(pattern = "MELODIA", x = testresultdb$machine), ]$machine <- "Melodia"
testresultdb[grepl(pattern = "MADMOM", x = testresultdb$machine), ]$machine <- "madmom"
testresultdb[grepl(pattern = "SS-PNN", x = testresultdb$machine), ]$machine <- "SS-nPNN"

testresultdb[grepl(pattern = "Pub", x = testresultdb$human), ]$human <- "NHS"
testresultdb[grepl(pattern = "Cons", x = testresultdb$human), ]$human <- "Consensus"

# Edit name (4) ---------------------------
fdrresultdb_hh$pair <- gsub("_", "-", fdrresultdb_hh$pair)
fdrresultdb_hh$pair <- gsub("Pub", "NHS", fdrresultdb_hh$pair)

# Edit name (5) ---------------------------
fdrresultdb_hm$machine <- unlist(lapply(strsplit(fdrresultdb_hm$pair, "_"), function(x) x[2]))

fdrresultdb_hm$machine <- toupper(fdrresultdb_hm$machine)
fdrresultdb_hm[grepl(pattern = "TONY [(]FRAME[)]", x = fdrresultdb_hm$machine), ]$machine <- "pYIN"
fdrresultdb_hm[grepl(pattern = "MELODIA", x = fdrresultdb_hm$machine), ]$machine <- "Melodia"
fdrresultdb_hm[grepl(pattern = "MADMOM", x = fdrresultdb_hm$machine), ]$machine <- "madmom"
fdrresultdb_hm[grepl(pattern = "SS-PNN", x = fdrresultdb_hm$machine), ]$machine <- "SS-nPNN"

# Plot order x ---------------------------
idx <- agreementdb_hm$withunison == TRUE & agreementdb_hm$human == "Consensus"

score <- data.frame(machine = agreementdb_hm[idx, "machine"], score = agreementdb_hm[idx, "coef"])
MACHINE <- unique(score$machine)
machine_median <- data.frame(machine = MACHINE, median = unlist(lapply(MACHINE, function(x) median(score[score$machine == x, ]$score))))
machine_median <- machine_median[order(machine_median$median, decreasing = TRUE), ]

X_ORDER_HM <- c(machine_median$machine)
X_ORDER_HM <- c("TONY", X_ORDER_HM[X_ORDER_HM != "TONY"])

# Create plot (human-human) [TODO: Add p-value and significance sign text]---------------------------
glist_hh <- vector(mode = "list", length = length(METRIC_COL))

for (i in 1:length(METRIC_COL)) {
  # Extract ---------------------------
  df_i <- cbind(agreementdb_hh["transcriber"], agreementdb_hh["pairtype"], agreementdb_hh["songstyle"])
  df_i$metric <- agreementdb_hh[[METRIC_COL[i]]]
  
  # CI ---------------------------
  PAIR <- unique(df_i$transcriber)
  CI.df <- data.frame(transcriber = PAIR, pairtype = "", se_u = 0, se_l = 0, median = 0)
  
  for(j in 1:length(PAIR)) {
    metric <- df_i[df_i$transcriber == PAIR[j], ]$metric
    metric <- sort(metric)
    
    n <- length(metric)
    r = round(n/2 - C*sqrt(n)/2)
    s = round(1 + n/2 + C*sqrt(n)/2)
    
    CI.df[j, ]$se_l <- metric[r]
    CI.df[j, ]$se_u <- metric[s]
    
    CI.df[j, ]$pairtype <- df_i[df_i$transcriber == PAIR[j], ]$pairtype[1]
    
    CI.df[j, ]$median <- median(metric)
  }
  
  # Set plot data ---------------------------
  g <- ggplot(data = df_i)
  
  g <- g + geom_violin(aes(x = transcriber, y = metric),
                       draw_quantiles = NULL, trim = TRUE, scale = "count", adjust = VIOLIN_ADJUST_HH)

  g <- g + geom_jitter(aes(x = transcriber, y = metric, color = songstyle),
                       size = MARKER_SIZE, width = JITTER_WID, alpha = 0.75)

  g <- g + stat_summary(aes(x = transcriber, y = metric, group = 1),
                        fun = SUMMARY_FUN, colour = "black", geom = "point", group = 1)
  
  g <- g + geom_errorbar(data = CI.df,
                         aes(x = transcriber, ymin = se_l, ymax = se_u),
                         width = ERRORBAR_WID_HH, alpha = 0.8)
  
  # Horizontal line for Kappa ---------------------------
  if (METRIC_NAME[i] == "Fleiss' Kappa") {
    g <- g + geom_hline(yintercept = 0, linetype = "dashed")
  }
  
  # Statistical test result ---------------------------
  if (METRIC_NAME[i] == "Fleiss' Kappa") {
    testsummary <- data.frame(transcriber = fdrresultdb_hh$pair, ypos = TEXT_YPOS, text = "",
                              H0reject_sign = "", pairtype = GRID_ORDER_HH[4])
    
    kappa <- df_i[df_i$pairtype == GRID_ORDER_HH[4], ]$metric
    testsummary$effect_size <- sum(kappa > 0.00)/length(kappa) - 0.50
    
    if (fdrresultdb_hh$reject == TRUE) {
      testsummary$H0reject_sign <- "*"
    }
    
    testsummary$text <- paste(format(testsummary$effect_size, digits = 2), testsummary$H0reject_sign, sep = "")
    
    g <- g + geom_text(data = testsummary, aes(x = transcriber, y = ypos, label = text),
                       size = TEXT_TEXTSIZE, fontface = "italic")
  }
  
  # Arrange plot layout ---------------------------
  g <- g + facet_grid(. ~ factor(pairtype, levels = GRID_ORDER_HH), scales = "free")
  
  if (METRIC_NAME[i] == "Fleiss' Kappa") {
    g <- g + ylim(KAPPA_YLIM[1], KAPPA_YLIM[2])
  } else if (METRIC_NAME[i] == "Percent identity") {
    g <- g + ylim(PID_YLIM[1], PID_YLIM[2])
  }
  
  g <- g + theme(plot.title = element_text(hjust = 0.5, size = TITLE_TEXTSIZE))
  g <- g + ylab(METRIC_NAME[i]) + theme(axis.title.x = element_blank(), axis.title.y = element_text(size = YAXIS_TITLE_TEXTSIZE))
  g <- g + theme(axis.text.x = element_text(angle = XAXIS_TEXTROTATE, vjust = 0.5, size = AXIS_TEXTSIZE),
                 axis.text.y = element_text(size = AXIS_TEXTSIZE))
  g <- g + theme(strip.text.x = element_text(size = STRIP_TEXTSIZE))
  
  g <- g + labs(colour = "Accompaniment") + theme(legend.position = "bottom")
  
  # Output plot result ---------------------------
  glist_hh[[i]] <- g
}

# Extract & clear legend ---------------------------
legend <- get_legend(glist_hh[[1]])

for (j in 1:length(METRIC_COL)) {
  glist_hh[[j]] <- glist_hh[[j]] + theme(legend.position = "none")
}

# Grid arrange ---------------------------
g_hh <- grid.arrange(arrangeGrob(glist_hh[[1]], glist_hh[[2]], ncol = 1), legend, nrow = 2, heights = c(10, 1),
                 top = textGrob("Human vs. Human (\"unison\")", gp = gpar(fontsize = TITLE_TEXTSIZE)))

# Save figure ---------------------------
ggsave(paste(OUTPUT_DIR, "Human-Human", " (with unison).png", sep = ""),
       plot = g_hh, width = FIG_WID, height = FIG_HEI)

# Create plot (human-human) ---------------------------
glist_hm <- vector(mode = "list", length = length(METRIC_COL))

HUMAN <- unique(agreementdb_hm$human)
UNISON <- unique(agreementdb_hm$withunison)

for (k in 1:length(UNISON)) {
  if (UNISON[k]) {
    UNISON_TEXT <- "\"unison\""
    UNISON_FILEID <- "(with unison)"
  } else {
    UNISON_TEXT <- "\"non-unison\""
    UNISON_FILEID <- "(without unison)"
  }
  
  for (j in 1:length(HUMAN)) {
    STRIP_TEXT <- HUMAN[j]
    
    if (HUMAN[j] == "Consensus") {
      STRIP_TEXT <- "Our consensus"
    } else if (HUMAN[j] == "NHS") {
      STRIP_TEXT <- "NHS consensus"
    }
    
    for (i in 1:length(METRIC_COL)) {
      # Extract ---------------------------
      idx <- agreementdb_hm$withunison == UNISON[k] & agreementdb_hm$human == HUMAN[j]
      df_i <- data.frame(machine = agreementdb_hm[idx, "machine"], songstyle = agreementdb_hm[idx, "songstyle"],
                         machinetype = agreementdb_hm[idx, "machinetype"], metric = agreementdb_hm[idx, METRIC_COL[i]],
                         dummy = paste("vs. ", STRIP_TEXT, sep = ""))

      # CI ---------------------------
      PAIR <- unique(df_i$machine)
      CI.df <- data.frame(machine = PAIR, se_u = 0, se_l = 0, median = 0)
      
      for(l in 1:length(PAIR)) {
        metric <- df_i[df_i$machine == PAIR[l], ]$metric
        metric <- sort(metric)
        
        n <- length(metric)
        r = round(n/2 - C*sqrt(n)/2)
        s = round(1 + n/2 + C*sqrt(n)/2)
        
        CI.df[l, ]$se_l <- metric[r]
        CI.df[l, ]$se_u <- metric[s]
        
        CI.df[l, ]$median <- median(metric)
      }
      
      # Set plot data ---------------------------
      g <- ggplot(data = df_i)
      
      #g <- g + geom_violin(aes(x = machine, y = metric, fill = machinetype), 
      #                     draw_quantiles = NULL, trim = TRUE, scale = "count", adjust = VIOLIN_ADJUST_HM[i], alpha = 0.15) + 
      #  scale_fill_manual(values = c("#000080", "#ff4500"))
      g <- g + geom_violin(aes(x = machine, y = metric),
                           draw_quantiles = NULL, trim = TRUE, scale = "count", adjust = VIOLIN_ADJUST_HM[i], alpha = 0.15) 
                           
      g <- g + geom_jitter(aes(x = machine, y = metric, color = songstyle),
                           size = MARKER_SIZE, width = JITTER_WID, alpha = 0.75)
      
      g <- g + stat_summary(aes(x = machine, y = metric, group = 1),
                            fun = SUMMARY_FUN, colour = "black", geom = "point", group = 1)
      
      g <- g + geom_errorbar(data = CI.df,
                             aes(x = machine, ymin = se_l, ymax = se_u),
                             width = ERRORBAR_WID_HM, alpha = 0.8)
      
      # Horizontal line for Kappa ---------------------------
      if (METRIC_NAME[i] == "Fleiss' Kappa") {
        g <- g + geom_hline(yintercept = 0, linetype = "dashed")
      }
      
      # Statistical test result ---------------------------
      if (UNISON[k] == TRUE && HUMAN[j] == "Consensus" && METRIC_NAME[i] == "Fleiss' Kappa") {
        idx <- testresultdb$withunison == UNISON[k] & testresultdb$human == HUMAN[j] & testresultdb$var == METRIC_COL[i]
        testsummary <- data.frame(effect_size = testresultdb[idx, ]$A, machine = testresultdb[idx, ]$machine, ypos = TEXT_YPOS)
        
        idx <- unlist(lapply(testsummary$machine, function(x) which(x == fdrresultdb_hm$machine)))
        testsummary$H0reject <- fdrresultdb_hm$reject[idx]
        testsummary$H1accept <- fdrresultdb_hm$posterior_H1[idx]
        
        testsummary$H0reject_sign <- ""
        testsummary$H0reject_sign[testsummary$H0reject == TRUE] <- "*"
        
        testsummary$H1accept_sign <- ""
        testsummary$H1accept_sign[testsummary$H1accept > 0.8] <- "^"
        
        testsummary$text <- paste(format(testsummary$effect_size, digits = 2), testsummary$H0reject_sign, testsummary$H1accept_sign, sep = "")
        
        g <- g + geom_text(data = testsummary, aes(x = machine, y = ypos, label = text),
                           size = TEXT_TEXTSIZE, fontface = "italic")
      }
      
      # Arrange plot layout ---------------------------
      g <- g + facet_grid(. ~ dummy)
      g <- g + scale_x_discrete(limits = X_ORDER_HM)
      
      if (METRIC_NAME[i] == "Fleiss' Kappa") {
        g <- g + ylim(KAPPA_YLIM[1], KAPPA_YLIM[2])
      } else if (METRIC_NAME[i] == "Percent identity") {
        g <- g + ylim(PID_YLIM[1], PID_YLIM[2])
      }
      
      g <- g + ylab(METRIC_NAME[i]) + theme(axis.title.x = element_blank(), axis.title.y = element_text(size = YAXIS_TITLE_TEXTSIZE))
      g <- g + theme(axis.text.x = element_text(angle = XAXIS_TEXTROTATE, vjust = 0.5, size = AXIS_TEXTSIZE),
                     axis.text.y = element_text(size = AXIS_TEXTSIZE))
      
      g <- g + theme(strip.text.x = element_text(size = STRIP_TEXTSIZE))
      
      g <- g + 
        theme(legend.position = "bottom", legend.text = element_text(size = LEGEND_TEXTSIZE), legend.title = element_text(size = LEGEND_TEXTSIZE)) + 
        guides(colour = guide_legend(nrow = 1, byrow = TRUE), shape = guide_legend(nrow = 1, byrow = TRUE), fill = guide_legend(nrow = 1, byrow = TRUE)) + 
        labs(colour = "Accompaniment")
        #labs(colour = "Style", fill = "Type")
      
      # Output plot result ---------------------------
      glist_hm[[i]] <- g
    }
    
    # Extract & clear legend ---------------------------
    legend <- get_legend(glist_hm[[1]])
    
    for (l in 1:length(METRIC_COL)) {
      glist_hm[[l]] <- glist_hm[[l]] + theme(legend.position = "none")
    }
    
    # Grid arrange ---------------------------
    g_hm <- grid.arrange(arrangeGrob(glist_hm[[1]], glist_hm[[2]], ncol = 1), legend, nrow = 2, heights = c(10, 1),
                        top = textGrob(paste("Human vs. Automated methods (", UNISON_TEXT, ")", sep = ""), gp = gpar(fontsize = TITLE_TEXTSIZE)))
                         
    # Save figure ---------------------------
    ggsave(paste(OUTPUT_DIR, "Human-Machine_", HUMAN[j], "_", UNISON_FILEID, ".png", sep = ""),
           plot = g_hm, width = FIG_WID, height = FIG_HEI)
          
    # Grid arrange (combined) ---------------------------
    for (l in 1:length(METRIC_COL)) {
      glist_hm[[l]] <- glist_hm[[l]] + theme(axis.title.y = element_blank(), axis.text.y = element_blank())
    }
    
    #g_hh <- grid.arrange(arrangeGrob(glist_hh[[1]], glist_hh[[2]], ncol = 1),
    #                     top = textGrob("Human vs. Human (\"unison\")", gp = gpar(fontsize = TITLE_TEXTSIZE)))
    g_hh <- grid.arrange(arrangeGrob(glist_hh[[1]], glist_hh[[2]], ncol = 1),
                         top = textGrob("Human vs. Human", gp = gpar(fontsize = TITLE_TEXTSIZE)))
                         
    #g_hm <- grid.arrange(arrangeGrob(glist_hm[[1]], glist_hm[[2]], ncol = 1),
    #                     top = textGrob(paste("Human vs. Automated methods (", UNISON_TEXT, ")", sep = ""), gp = gpar(fontsize = TITLE_TEXTSIZE)))
    g_hm <- grid.arrange(arrangeGrob(glist_hm[[1]], glist_hm[[2]], ncol = 1),
                         top = textGrob(paste("Human vs. Automated methods", sep = ""), gp = gpar(fontsize = TITLE_TEXTSIZE)))
                         
    g_hhhm <- grid.arrange(arrangeGrob(g_hh, g_hm, nrow = 1, widths = c(6, 5)), legend, nrow = 2, heights = c(10, 1))
    
    # Save figure ---------------------------
    ggsave(paste(OUTPUT_DIR, "Human-Human_Human-Machine_", HUMAN[j], "_", UNISON_FILEID, ".png", sep = ""),
           plot = g_hhhm, width = FIG_WID * 2, height = FIG_HEI)
  }
}
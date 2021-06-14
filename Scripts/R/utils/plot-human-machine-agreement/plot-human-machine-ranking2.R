# Define constant variables ---------------------------
AGREEMENT_HM_WITH_UNI <- "../../output/human-machine-pairwise_agreement/paper/with_unison/human-machine-pairwise_agreement (with unison).csv"
AGREEMENT_HM_WITHOUT_UNI <- "../../output/human-machine-pairwise_agreement/paper/without_unison/human-machine-pairwise_agreement (without unison).csv"

OUTPUT_DIR <- "../../output/human-machine-pairwise_agreement/paper/"
OUTPUT_FILEID <- "top_k_agreement.csv"

K <- 10
M <- 10
METRIC_COLNAME <- "coef"

TYPE <- "best"

# Load files ---------------------------
agreementdb_hm_i <- read.csv(AGREEMENT_HM_WITH_UNI, header = TRUE, sep = ",")
agreementdb_hm_i$withunison <- TRUE
agreementdb_hm_j <- read.csv(AGREEMENT_HM_WITHOUT_UNI, header = TRUE, sep = ",")
agreementdb_hm_j$withunison <- FALSE
agreementdb_hm <- rbind(agreementdb_hm_i, agreementdb_hm_j)

# Edit name ---------------------------
agreementdb_hm$machine <- toupper(agreementdb_hm$machine)
agreementdb_hm[grepl(pattern = "TONY [(]FRAME[)]", x = agreementdb_hm$machine), ]$machine <- "pYIN"
agreementdb_hm[grepl(pattern = "TONY [(]NOTE[)]", x = agreementdb_hm$machine), ]$machine <- "TONY"
agreementdb_hm[grepl(pattern = "MELODIA", x = agreementdb_hm$machine), ]$machine <- "Melodia"
agreementdb_hm[grepl(pattern = "MADMOM", x = agreementdb_hm$machine), ]$machine <- "madmom"
agreementdb_hm[grepl(pattern = "SS-PNN", x = agreementdb_hm$machine), ]$machine <- "SS-nPNN"

agreementdb_hm[grepl(pattern = "Pub", x = agreementdb_hm$human), ]$human <- "NHS"
agreementdb_hm[grepl(pattern = "Cons", x = agreementdb_hm$human), ]$human <- "Consensus"

# Create summary ---------------------------
UNISON <- c(TRUE, FALSE)
HUMAN <- unique(agreementdb_hm$human)
MACHINE <- unique(agreementdb_hm$machine)
MACHINE <- MACHINE[MACHINE != "AD-NNMF"]
SONG <- unique(data.frame(song = agreementdb_hm$song, songstyle = agreementdb_hm$songstyle))

if (TYPE == "best") {
  order_decreasing = TRUE
} else {
  order_decreasing = FALSE
}

for (i in 1:length(HUMAN)) {
  top_k_agreement <- rbind(data.frame(transcriber = HUMAN[i], song = SONG$song, songstyle = SONG$songstyle, count = 0, max = -1, machine = "", unison = TRUE),
                           data.frame(transcriber = HUMAN[i], song = SONG$song, songstyle = SONG$songstyle, count = 0, max = -1, machine = "", unison = FALSE))
  
  for (j in 1:length(UNISON)) {
    idx <- agreementdb_hm$human == HUMAN[i] & agreementdb_hm$withunison == UNISON[j]
    
    for (k in 1:length(MACHINE)) {
      idx_k <- idx & agreementdb_hm$machine == MACHINE[k]
      
      df_k <- agreementdb_hm[idx_k, ]
      orderidx <- order(df_k[[METRIC_COLNAME]], decreasing = order_decreasing)
      orderidx <- orderidx[1:K]
      
      top_k_song <- df_k[orderidx, ]$song
      top_k_score <- df_k[orderidx, METRIC_COLNAME]
      
      for (l in 1:K) {
        idx_l <- top_k_agreement$song == top_k_song[l] & top_k_agreement$unison == UNISON[j]
        top_k_agreement[idx_l, ]$count <- top_k_agreement[idx_l, ]$count + 1
        
        if (top_k_agreement[idx_l, ]$max < top_k_score[l]) {
          top_k_agreement[idx_l, ]$max <- top_k_score[l]
          top_k_agreement[idx_l, ]$machine <- MACHINE[k]
        }
      }
    }
  }
  
  # Merge ---------------------------
  idx_u <- order(top_k_agreement$count * as.numeric(top_k_agreement$unison == TRUE), decreasing = TRUE)
  idx_u <- idx_u[1:M]
  idx_nu <- order(top_k_agreement$count * as.numeric(top_k_agreement$unison == FALSE), decreasing = TRUE)
  idx_nu <- idx_nu[1:M]
  
  top_m_agreement <- rbind(top_k_agreement[idx_u, ], top_k_agreement[idx_nu, ])
  
  # Output ---------------------------
  output_filepath <- paste(OUTPUT_DIR, HUMAN[i], "_", TYPE, "_", OUTPUT_FILEID, sep = "")
  write.csv(top_m_agreement, output_filepath, row.names = FALSE)
}
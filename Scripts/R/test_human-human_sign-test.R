# Constant variables---------------------------
PAIR_CSV_FILEPATH <- "./output/transcriber-pairwise_agreement/paper/transcriber-pairwise_agreement.csv"
OUTPUT_DIR <- "./test result/"
OUTPUT_FILEID <- "human-human_one-sample_test.csv"

# Extraction---------------------------
irrstat <- read.csv(PAIR_CSV_FILEPATH, header = TRUE, sep = ",")

PAIR <- unique(irrstat$transcriber)

# Sign test---------------------------
testresult.frame <- data.frame(pair = PAIR, pval = 0, N = 0, S = 0)

for (i in 1:length(PAIR)) {
  X <- irrstat[irrstat$transcriber == PAIR[i], ]$coef
  
  N <- length(X)
  S <- sum(X > 0)
  
  pval <- 1 - pbinom(S - 1, size = N, prob = 0.5, lower.tail = TRUE, log.p = FALSE)
  
  testresult.frame[i, ]$pval <- pval
  testresult.frame[i, ]$N <- N
  testresult.frame[i, ]$S <- S
  
  #testresult <- binom.test(S, n = N, p = 0.5, alternative = "greater", conf.level = 0.95)
  #pval <- testresult$p.value
}

output_filepath <- paste(OUTPUT_DIR, OUTPUT_FILEID, sep = "")
write.csv(testresult.frame, output_filepath, row.names = FALSE)
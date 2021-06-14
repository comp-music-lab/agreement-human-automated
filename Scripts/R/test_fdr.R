# Constant variables---------------------------
ONE_SAMPLE_CSV_FILEPATH <- "./test result/human-human_one-sample_test.csv"
TWO_SAMPLE_CSV_FILEPATH <- "./test result/human-machine_two-sample_test_with_unison.csv"

OUTPUT_DIR <- "./test result/"
OUTPUT_FILEID <- "fdr_test.csv"

fdr_level = 0.05

# Extract---------------------------
one_sample_result <- read.csv(ONE_SAMPLE_CSV_FILEPATH, header = TRUE, sep = ",")
two_sample_result <- read.csv(TWO_SAMPLE_CSV_FILEPATH, header = TRUE, sep = ",")

testresult.frame <- data.frame(category = character(), pair = character(),
                               pval = numeric(), posterior_H1 = numeric(), lnbf = numeric(), es = numeric(),
                               level = numeric(), threshold = numeric(), reject = logical())

# Extract - one-sample---------------------------
idx <- one_sample_result$pair == "Cons_Pub"

testresult.frame[1, ] <- data.frame(category = "one-sample", pair = one_sample_result[idx, ]$pair, 
                                    pval = one_sample_result[idx, ]$pval, posterior_H1 = NaN, lnbf = NaN, es = NaN,
                                    level = fdr_level, threshold = 0, reject = FALSE)

# Extract - two-sample---------------------------
idx <- two_sample_result$var == "coef" & two_sample_result$human == "Cons"

testresult.frame_i <- data.frame(category = "two-sample",
                                 pair = paste(two_sample_result[idx, ]$human, "_", two_sample_result[idx, ]$machine, sep = ""),
                                 pval = two_sample_result[idx, ]$pval,
                                 posterior_H1 = two_sample_result[idx, ]$posterior_H1,
                                 lnbf = two_sample_result[idx, ]$lnbf,
                                 es = two_sample_result[idx, ]$A,
                                 level = fdr_level, threshold = 0, reject = FALSE)

testresult.frame = rbind(testresult.frame, testresult.frame_i)

# BH step-up procedure---------------------------
idx <- order(testresult.frame$pval)

m <- dim(testresult.frame)[1]

for (i in 1:length(idx)) {
  testresult.frame[idx[m - i + 1], ]$threshold <- fdr_level * (m - i + 1)/m
}

rejection <- testresult.frame$pval < testresult.frame$threshold
testresult.frame$reject <- rejection

# Output results---------------------------
output_filepath <- paste(OUTPUT_DIR, OUTPUT_FILEID, sep = "")
write.csv(testresult.frame, output_filepath, row.names = FALSE)

# simulation---------------------------
city <- c("富山", "石川", "福井", "山梨", "長野", "岐阜", "静岡", "愛知", "三重", "滋賀", "京都", "大阪", "兵庫", "奈良", "和歌山", "鳥取", "島根", "岡山", "広島", "山口")
pval <- c(6.18*1e-5, 0.0151, 0.44, 0.0018, 6.02*1e-7, 4.02*1e-4, 4.17*1e-5, 1.19 * 1e-12, 0.0013, 0.3284, 0.0966, 0.0129, 0.1171, 0.0092, 0.0608, 0.9030, 0.0047, 0.0356, 1.09*1e-6, 0.1202)
threshold <- vector(length = length(pval))
rejection <- vector(length = length(pval))

idx <- order(pval)

m <- length(pval)

for (i in 1:length(idx)) {
  threshold[idx[m - i + 1]] <- fdr_level * (m - i + 1)/m
}

rejection <- pval < threshold
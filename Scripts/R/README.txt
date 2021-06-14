1. test_transcriber-pairwise_agreement_v2.R
 -> This script creats alignment of human-human note sequences and summarizes Fleiss' Kappa, PID and Levenshtein of all pairs.
 -> The output file containing agreement scores is being used as input of one-sample test (4.).

2. test_human-machine-pairwise_agreement_v2.R
 -> This script creats alignment of human-machine note sequences and summarizes Fleiss' Kappa, PID and Levenshtein of all pairs.
 -> The output file containing agreement scores is being used as input of the two-sample scripts of Matlab.

3. test_transcriber-group_agreement.R
 -> This script creats multi-sequence alignment of human-human note sequences and summarizes Fleiss' Kappa, PID and Levenshtein of all pairs.

4. test_human-human_sign-test.R
 -> This script calculates p-value of the one-sample one-tailed sign test of human-human agreement.

5. test_fdr.R
 -> This script judges the rejection of null hypotheses using a false discovery rate technique. The output of 4. and the Matlab script need to be placed under "./test result" folder.
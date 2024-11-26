# processes the results to get the performance for each iteration/unique condition.
# saves the output in the ./data/ folder.

# load in data and functions
source('sim/functions.R')
library(kit) # needed for 'kit::topn()' which gets nth highest number of vector.
output <- readRDS(file.path('sim', 'results', 'results_added.RData')) # load in unprocessed results
rownames(output) <- 1:nrow(output)

# configurations
n_conditions <- 9

# Ht (= true hypothesis) per set
true_hyps <- c(set1=-1, set2=-1, set3=-1, set4 = -1, set5 = 1, set6 = -1, set7=1)

grace_hyps <- list(set1=c(-1), set2=c(-1), set3=c(-1), # these are ignored
                   set4 = c(-1), set5 = c(1, 3), set6 = c(-1), set7 = c(1))

# check for non-convergence
hypothesis_data <- output[,(n_conditions+1):ncol(output)]   # subset for the hypothesis weights
sum(is.na(c(hypothesis_data[,1], hypothesis_data[,2])))     # Because each set contains at least 2 hypotheses, we check if there are no NA values in all rows for the first two hypotheses.
sum(rowSums(hypothesis_data, na.rm=T) < 0.1)  # sum of tested hypothesis weights should in each iteration be 1 (or at list bigger than 0.999 if rounding error occurs). 
noncon <- which(rowSums(hypothesis_data, na.rm=T) < 0.1)  # sum of tested hypothesis weights should in each iteration be 1 (or at list bigger than 0.999 if rounding error occurs). 
# one iteration non-converged, but running analysis manually does provide output with these conditions. Thus this is row is omitted
output <- output[-c(noncon),]

# get performance per iteration, takes ~1 minute to run
metrics <- matrix(NA, nrow(output), 5) # there are five metrics defined below which we initialize a matrix of.
colnames(metrics) <- c('correct', # correct = true, most parsimonious is selected? 1 if yes, 0 if no
                       'support', # support = Relative support of selected hyp vs hyp with second highest weight,
                       'Weight_Ht', 'Weight_Hs', # weights of true and selected hypothesis (can differ if true hypothesis is not selected).
                       "grace")   # Also counts true if unconstrained is selected.
for(row in 1:nrow(output)){ # get performance indicators for every row
  metrics[row,] <- performance(output[row,])
}
res <- cbind(output, metrics) # performance per iteration
current_date <- "2024-09-22"
saveRDS(res, file.path('analysis', 'data', 'data_to_analyse_2024_09_22.RData')) # save to .RData file.

# obtain performance average per condition (so per unique combination of design factor, get THR, support and median weights)
# NB, this data is not used yet in the analyse_results.r script, but may be handy to check a specific condition.
# niter <- 100 # there were 100 iterations per condition
# Sequence <- seq(1, nrow(res), niter) # should be length of number of unique conditions
# perf_per_condition <- matrix(NA, length(Sequence), 8) # there are 8 values of interest 
# colnames(perf_per_condition) <- c(sprintf('H%d', 1:4), 'support', 'Weight_Ht', 'Weight_Hs', 'THR')
# loop_object <- cbind(Sequence, 1:length(Sequence)) # create object to loop over.
# for(i in 1:nrow(loop_object)){ # for every 100 iterations belonging to a unique design factor
#   Subset <- res[(loop_object[i,1]):(loop_object[i,1]+niter-1), ] # subset for the 100 iterations belonging to the unique design factor.
#   THR <- sum(Subset[,'correct']) / nrow(Subset)
#   measures <- Subset[,c(sprintf('H%d', 1:4), 'support', 'Weight_Ht', 'Weight_Hs')]
#   median_weights <- apply(measures, 2, median)
#   perf_per_condition[(loop_object[i,2]),] <- c(median_weights, THR)
# }
# res_per_condition <- cbind(unique(output[,1:n_conditions]), perf_per_condition)
# saveRDS(res_per_condition, file.path('analysis', 'data', 'avg_per_condition.RData'))


rm(list=ls())


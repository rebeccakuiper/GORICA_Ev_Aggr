# Simulation script, outputs .txt files with hypothesis data and row number of the `conditions` data.frame(). Row number is returned so we can match the conditions data.frame() with the hypothesis output.
# runtime ~ 4.67 hours on Hp elitebook 840 G8 (8 threads used) for 388800 iterations

# load necessary data and functions and dependencies, including analysis() function
load(file.path('sim', 'conditions.RData'))    # load data.frame() with conditions
source(file.path('sim', 'functions.R'))       # load in functions
dependencies <- readLines('requirements.txt') # load in names of external libraries
lapply(dependencies, library, character.only = T) # load in actual libraries

# initialize just in time compilation for analysis() function
compiler::enableJIT(level = 3)
analysis_c <- compiler::cmpfun(analysis) # Decreases runtime by ~ 1.06 times compared to analysis()

# initialize the parallel execution of the analysis 
set.seed(6164900)                          # seed to reproduce results
cl <- makeCluster(parallel::detectCores()) # this uses all available threads (NOTE: you cannot use your computer properly when all threads are used for processing. If you want to continue other work, reduce the number of used threads)
registerDoSNOW(cl) # Initialize multicluster
clusterExport(cl, c('analysis', 'analysis_c', 'print_model', 'estimate_betas', 'GoricEvSynEssentials')) # export necessary data to the different R sessions used by the different clusters


# add progress bar to keep track of how many iterations are left
pb <- txtProgressBar(min=1, max=nrow(conditions), style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

#run simulation
print('start simulation')
t0 <- Sys.time() # keep track of timing
foreach(rownum = 1:nrow(conditions), .packages = c("MASS", "Matrix", "matrixStats"), .options.snow=opts) %dopar% {
  # for each row in conditions:
  # run analysis with row-specific conditions
  res <- tryCatch(analysis_c(conditions$sets[rownum], conditions$models[rownum], 
                           conditions$true_R2[rownum], conditions$ratios[rownum], 
                           conditions$rho[rownum], conditions$N[rownum], 
                           specs, rownum),
           error = function(e){list(c(rownum, rep(0,4)), c(rownum,rep(0,4)))}) # return 4 x 0 if error occurs. This make sure the simulation does not stop. Non-converged iterations can be easily identified later on.
  
  # write outputs to .txt files for both added and equal evidence. Each session creates 2 .txt files, one for equal, other for added evidence.
  write.table(x = t(res[[1]]), file = sprintf("./sim/results/results_added/results_added_%d.txt" , Sys.getpid()), sep = "\t", append = TRUE, row.names = FALSE, col.names = FALSE)
  write.table(x = t(res[[2]]), file = sprintf("./sim/results/results_equal/results_equal_%d.txt" , Sys.getpid()), sep = "\t", append = TRUE, row.names = FALSE, col.names = FALSE)
  
}
stopCluster(cl) # close parallel sessions.
runtime <- Sys.time() - t0 # check runtime of sim.
#stop('End of Simulation')



# Combine the output + input (this way we know to which conditions the output belongs.)
for(typeEv in c('added', 'equal')){ # do that for both equal and added evidence
  max_hypos = 4 # there were maximally 4 hypotheses tested
  n_conditions <- ncol(conditions) - 1 # we subtract by one because the first columns in `conditions` is not a condition, but an iterations-tracker
  all_files <- list.files(file.path('sim','results', paste0('results_',typeEv)), pattern = '*.txt', full.names = T) # get all output file locations
  res <- do.call(rbind, lapply(all_files, read.table, sep = "\t", fill = T,   # row bind all output
                               col.names = c('rownum', sprintf('H%d', 1:max_hypos)))) # give columns appropriate names
  res <- res[order(res$rownum),] # order data based by row number
  final_res <- cbind(conditions[,2:ncol(conditions)], res[,2:ncol(res)]) # match output to appropriate condition and omit 'rownum' as a variable
  save_path <- file.path('sim', 'results', paste0('results_', typeEv, '.RData')) # location to write the data to
  saveRDS(final_res, save_path) # write final results to save file.
}

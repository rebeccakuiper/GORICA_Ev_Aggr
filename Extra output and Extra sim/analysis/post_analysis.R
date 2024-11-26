# This script shows the effect on the Hypothesis Selection Rate (HSR) for different thresholds (see manuscript for more info.)
dat <- readRDS(file.path('analysis', 'data', 'data_to_analyse.RData')) 

# relative support data for all iterations, true and false selection cases
support <- dat[, 'support']
support_correct <- dat[dat$correct == 1, 'support']
support_incorrect <- dat[dat$correct == 0, 'support']

# configurations
niter <- nrow(dat)
count_correct <- length(support_correct)
count_incorrect <- length(support_incorrect)
n_conditions <- 6 #there are 6 design factors
Q <- 1:200 # We are interested

# obtain HST per q value.
HSR_per_q <- t(vapply(Q, function(q){ # how often is Ht selected / total number of iterations
  support_count <- sum(support > q) # check how often the support is bigger than q
  support_count_correct <- sum(support_correct > q)
  support_count_incorrect <- sum(support_incorrect > q)
  c(support_count/niter, # and divide by number of iterations to get HSR
    support_count_correct/count_correct,
    support_count_incorrect / count_incorrect)
}, numeric(3)))


# append HSR values to Q values
res <- cbind(Q, round(HSR_per_q,3))
colnames(res) <- c('Q', 'HSR', 'HSR_correct', 'HSR_incorrect') #HSR means Hypothesis Selection Rate

# plot HSR against Q for all iterations and both the true and false selection cases
pdf('./images/HSR_selection_criteria.pdf', width = 7, height = 5) # save in ./images/ folder.
#Plot entire range
par(mfrow = c(1,2))
plot(res[,'Q'], res[,'HSR'], ylab = 'Hypothesis Selection Rate', xlab = 'q', 
     ylim = c(0,1), type = 'l', xaxt="n", col = "blue")
axis(1, at = c(1,50,100,150,200))
lines(res[,'Q'], res[,'HSR_correct'], col = 'olivedrab4', lty = 4)
lines(res[,'Q'], res[,'HSR_incorrect'], col = 'red', lty = 2)
#lines(res[,'Q'], res[,'HSR_correct']-res[,"HSR_incorrect"], col = 'darkgreen')
legend('topright', legend = c('All cases', 'True selection cases', 'False selection cases'), 
       col = c('blue', 'olivedrab4', 'red'), lty = c(1,4,2), cex = 0.7)

# Plot range 1-30
this_range <- 1:30
plot(res[,'Q'][this_range], res[,'HSR'][this_range], ylab = '', xlab = 'q', 
     ylim = c(0,1), type = 'l', xaxt="n", col = "blue")
axis(1, at = c(1,5,10,15, 20, 25, 30))
lines(res[,'Q'][this_range], res[,'HSR_correct'][this_range], col = 'olivedrab4', lty = 4)
lines(res[,'Q'][this_range], res[,'HSR_incorrect'][this_range], col = 'red', lty = 2)
#lines(res[,'Q'][this_range], res[,'HSR_correct'][this_range]-res[,"HSR_incorrect"][this_range], col = 'darkgreen')

dev.off()

rm(list=ls())

plot(res[,"Q"], (res[,"HSR_correct"]-res[,"HSR_incorrect"]), ylim = c(0,1), type = "l")


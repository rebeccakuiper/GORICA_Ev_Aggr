# Analysis of the results.
# NOTE: the runtime could be improved if `data.table` package would be used
#
dat <- readRDS(file.path('data', 'data_to_analyse_2024_09_22.RData')) # performance per iteration
dat_sim1 <- readRDS(file.path('data', 'data_to_analyse.RData'))
#
dat_comparison <- dat_sim1[dat_sim1$sets %in% c(5,8) & dat_sim1$models == "hetero" &
                             dat_sim1$ratios %in% c("ratio1", "ratio2") & dat_sim1$rho == 0.25 &
                             dat_sim1$N %in% c(25, 75, 200),]
#head(dat_comparison)
#
dat_aic <- dat_sim1[dat_sim1$sets %in% c(2,3),]
dat_aic_gorica <- dat_sim1[dat_sim1$sets %in% c(8,9),]
dat_aic <- dat_sim1[dat_sim1$ratios != "ratio3" & dat_sim1$sets %in% c(2,3),]
dat_aic_gorica <- dat_sim1[dat_sim1$ratios != "ratio3" & dat_sim1$sets %in% c(8,9),]
head(dat_aic)
#
rm(dat_sim1)

# NOTE hypothesis is NOT true when inverse = 1 - not comparable to 4 studies set-up
# None = Same R2 and ratio as other studies, 
# "inverse" = inverse ratio; R2 = 0.02, 
# "extreme" = inverse ratio; R2 = 0.25
dat_no_outlier <- dat[dat$inverse_ratio == 0 & dat$outlier_study == "None", ]     # RMK: dan inverse_ratio == 0
dat_inv_outlier <- dat[dat$inverse_ratio == 0 & dat$outlier_study == "inverse", ] # RMK: dan inverse_ratio == 1
dat_ext_outlier <- dat[dat$inverse_ratio == 0 & dat$outlier_study == "extreme", ]

# 
# dat_avg <- readRDS(file.path('analysis', 'data', 'avg_per_condition.RData')) # average performance per unique condition.
# 
source(file.path('functions_analysis.R')) # load functions to do the analysis with.
library(stringr) # for string manipulation.
# 
# # some config
numerical <- c('true_R2', 'rho', 'N', 'inverse_ratio') # numerical design factors
categorical <- c('sets', 'models', 'ratios', 'outlier_study') # categorical design factors
conditions <- c(numerical, categorical)


library(knitr)
# Overall performance
#cat("# Overall performance: \n")
cat("### Simulation results 4 studies: Random outliers \n")
kable(performance_condition(dat_comparison))

cat("### Simulation results 20 studies: Random outliers \n")
kable(performance_condition(dat_no_outlier))

cat("### Simulation results 20 studies: Inverse, low-ES outliers \n")
kable(performance_condition(dat_inv_outlier))

cat("### Simulation results 20 studies: Inverse, high-ES outliers \n")
kable(performance_condition(dat_ext_outlier))




# N per outlier type
cat("# Effect of sample size \n")

cat("### No outliers")
kable(marginal_effect(dat_no_outlier, "N"))

cat("### Inverse outlier")
kable(marginal_effect(dat_inv_outlier, "N"))

cat("### Extreme outlier")
kable(marginal_effect(dat_ext_outlier, "N"))

cat("### Dat with 4 studies")
kable(marginal_effect(dat_comparison, "N"))


pdf('images/N_no_outlier.pdf', height = 5, width = 7)
marginal_effect(dat_no_outlier, "N", plot_out = T)
dev.off()

pdf('images/N_inv_outlier.pdf', height = 5, width = 7)
marginal_effect(dat_inv_outlier, "N", plot_out = T)
dev.off()

pdf('images/N_ext_outlier.pdf', height = 5, width = 7)
marginal_effect(dat_ext_outlier, "N", plot_out = T)
dev.off()

pdf('images/N_4_studies.pdf', height = 5, width = 7)
marginal_effect(dat_comparison, "N", plot_out = T)
dev.off()



# Ratio per outlier type
cat("# Effect of ratios \n")

cat("### no outliers \n")
kable(marginal_effect(dat_no_outlier, "ratios"))
cat("### inverse outliers \n")
kable(marginal_effect(dat_inv_outlier, "ratios"))
cat("### extreme outliers \n")
kable(marginal_effect(dat_ext_outlier, "ratios"))
cat("### data with 4 studies \n")
kable(marginal_effect(dat_comparison, "ratios"))


# Sets per outlier type
cat("# Effect of Sets \n")

cat("### no outliers \n")
kable(marginal_effect(dat_no_outlier, "sets"))
cat("### inverse outliers \n")
kable(marginal_effect(dat_inv_outlier, "sets"))
cat("### extreme outliers \n")
kable(marginal_effect(dat_ext_outlier, "sets"))
cat("### data with 4 studies \n")
kable(marginal_effect(dat_comparison, "sets"))


# True_R2 per outlier type
cat("# Effect of true R2 \n")

cat("### no outliers \n")
kable(marginal_effect(dat_no_outlier, "true_R2"))
cat("### inverse outliers \n")
kable(marginal_effect(dat_inv_outlier, "true_R2"))
cat("### extreme outliers \n")
kable(marginal_effect(dat_ext_outlier, "true_R2"))
cat("### data with 4 studies \n")
kable(marginal_effect(dat_comparison, "true_R2"))

pdf('images/R2_no_outlier.pdf', height = 5, width = 7)
marginal_effect(dat_no_outlier, "true_R2", plot_out = T)
dev.off()

pdf('images/R2_inv_outlier.pdf', height = 5, width = 7)
marginal_effect(dat_inv_outlier, "true_R2", plot_out = T)
dev.off()

pdf('images/R2_ext_outlier.pdf', height = 5, width = 7)
marginal_effect(dat_ext_outlier, "true_R2", plot_out = T)
dev.off()

pdf('images/R2_4_studies.pdf', height = 5, width = 7)
marginal_effect(dat_comparison, "true_R2", plot_out = T)
dev.off()



### h0 vs hu
h0unc <- function(thisdat){
  cat("# quantiles weights H0")
  print(quantile(thisdat$H1))
  
  cat("# quantiles weights H_unc")
  print(quantile(thisdat$H4))
  
  cat("# how often is weight H_unc > weight H0")
  print(sum(thisdat$H4 > thisdat$H1)) 
  
  cat("# how often is weight H_unc > weight H_0 percentage")
  print(sum(thisdat$H4 > thisdat$H1) / nrow(thisdat))
  
  cat("#quantiles of suppport of H_unc over H_0")
  print(quantile(thisdat$H4/thisdat$H1))
}

cat("### AIC")

cat("### For both set 2 and 3")
h0unc(dat_aic)

cat("### Set 2: H0 vs Hunc, H1 (non-H0) true, and thus Hunc best")
h0unc(dat_aic[dat_aic$sets == 2,])

cat("### Set 3: H0 vs Hunc, compl H1 (non-H0) true, and thus Hunc best")
h0unc(dat_aic[dat_aic$sets == 3,])

#Comparable results for GORICA
cat("### GORICA comparable to AIC-scenario")
#dat_aic_gorica
# Both sets together
cat("### For both set 7 and 8")
kable(performance_condition(dat_aic_gorica))
# Set comparable to Set 2: H1 vs its compl and H1 true
cat("### Set 7: H1 vs its compl, H1 true and thus also best")
kable(performance_condition(dat_aic_gorica[dat_aic_gorica$sets == 8,]))
# Set comparable to Set 3: H1 vs its compl and compl H1 true
cat("### Set 8: H1 vs its compl, compl H1 true and thus also best")
kable(performance_condition(dat_aic_gorica[dat_aic_gorica$sets == 9,]))




#### effect of set
cat("#Effect of set on performance \n:")

cat("### complete dataset \n")
kable(marginal_effect(dat, "sets"))

#cat("### dataset where informative hyp is true \n")
#kable(marginal_effect(dat_hyp_true, "sets"))
#
#cat("### dataset where informative hyp is not true \n")
#kable(marginal_effect(dat_hyp_false, "sets"))


# THR_over_aggregated <- median(dat_avg$THR)
# 
# # marginal performance in cases that exclude conditions we would not recommend (= real life cases that abide by our recommendations for using GoricEvSyn, see manuscript for clearer explanation)
# good_practice <- dat[!(dat$true_R2 == 0.02 & dat$sets %in% c(3,6) & dat$ratios != 'ratio4'), ] # exclude when hu is true and effect size is very low
# good_practice <- good_practice[!(good_practice$sets %in% c(2,5) & good_practice$true_R2 == 0.02 & good_practice$ratios == 'ratio4'), ] # same, but for sets 2 and 5
# good_practice <- good_practice[!(good_practice$sets %in% c(1,2,3) & good_practice$true_R2 == 0.02), ] # exclude when H0 is part of set, and true R2 = 0.02
# recommended_performance <- performance_condition(good_practice)
# 
# # performance when false hypothesis is selected
# incorrect <- dat[dat$correct == 0,]
# incorrect_performance <- performance_condition(incorrect)

# # performance when true hypothesis was selected
# correct <- dat[dat$correct == 1,]
# correct_performance <- performance_condition(correct)
# 
# # plot marginal effects N, trueR2 and rho, the numerical design factors.
# pdf('images/samplesize_marginal.pdf', height = 5, width = 7)
# ME_N <- marginal_effect(dat = dat, condition = "N", plot_out = T)
# dev.off()
# pdf('images/trueR2_marginal.pdf', height = 5, width = 7)
# ME_true_R2 <- marginal_effect(dat, "true_R2", T)
# dev.off()
# pdf('images/rho_marginal.pdf', height = 5, width = 7)
# ME_rho <- marginal_effect(dat, "rho", T, MARSlegendpos = "topright")
# dev.off()
# 
# 
# # marginal effects for sets, models and ratios; the categorical design factors
# ME_ratios <- marginal_effect(dat, "ratios")    # ratios 2 and 3 do slightly worse.
# ME_ratios_true <- marginal_effect(correct, "ratios")
# ME_ratios_false <- marginal_effect(incorrect, "ratios")  
# 
# ME_sets <- marginal_effect(dat, "sets")        # set 3 and 6 do badly, due to unconstrained
# ME_sets_true <- marginal_effect(correct, "sets")
# ME_sets_false <- marginal_effect(incorrect, "sets") 
# 
# ME_models <- marginal_effect(dat, "models")    # no visible marginal relationship
# ME_models_true <- marginal_effect(correct, "models")
# ME_models_false <- marginal_effect(incorrect, "models")    
# 
# # check how often true is chosen, relative to correct, but not most parsimonious
# (sum(dat$grace) - sum(dat$correct)) / nrow(dat) * 100  # only in 0.25% of the iterations was a correct hyp chosen that was not most parsimonious
# 
# 
# # difference sets 1,2,3 and 4,5,6 due to True_R2 = 0.02
# marginal_effect(dat[dat$true_R2 != 0.02 & dat$ratios == 'ratio4', ], "sets") 
# marginal_effect(dat[dat$true_R2 == 0.02, ], "sets") 
# 
# # sets 3 and 6 do way better if unconstrained is not the best hypothesis, which is if ratio == 4
# marginal_effect(dat[dat$ratio == 'ratio4', ], "sets") 
# marginal_effect(dat[dat$ratio != 'ratio4', ], "sets")
# 
# # when true_R2 = 0.02 is filtered out sets 2-5 and 3-6 are almost identical, 4 still does slightly better than 1.
# marginal_effect(dat[dat$true_R2 != 0.02, ], "sets") 
# # Discrepancy between set 1 and 4 disappears if ratio == ratio 4.
# marginal_effect(dat[dat$true_R2 != 0.02 & dat$ratios == 'ratio4' , ], "sets") 
# 
# 
# 
# # interesting interactions effects
# IE_sets_ratios <- interaction_effect(dat, 'sets', 'ratios', print_it = T)
# IE_N_trueR2 <- interaction_effect(dat, 'N', 'true_R2', plot_out = T, print_it = F) # this interaction is mostly explained by the interaction between ratios and sets
# IE_sets_N <- interaction_effect(dat, 'sets', 'N', print_it = T)
# IE_sets_model_types <- interaction_effect(dat, 'sets', 'model_types') # likely also explained due to interaction of sets with ratios.
# 
# # Show weight of best hypothesis converges to 1 for sets 2,3,5,6 if Hu in set and Hu is not the only true hypothesis.
# best_perf <- dat[dat$true_R2 == 0.25 & dat$N == 1000 & dat$rho == 0,] # extract df of the expected best performing iterations
# excl_hu_true <- best_perf[ !(best_perf$sets %in% c(3,6) & best_perf$ratios != 'ratio4') # this excludes cases where Hu is true for sets 2,3,5,6 (this way we can show that Hm does not converge to one for these sets)
#                            &!(best_perf$sets %in% c(2,5) & best_perf$ratios == 'ratio4'), ]
# max(excl_hu_true[excl_hu_true$sets %in% c(2,3,5,6),'Weight_Hm']) # maximum value is not one, as expected.
# max(excl_hu_true[,'Weight_Hm']) # max value == 1 if no subset is made,If Hu is part of set and not the best hypothesis, the weight of Hm will not be one.
# 
# 
# # extra plot for the THR per set, grouped by colour.
# allsets <- c(1,4,2,5,8,3,6,9) # the tested sets (omit set 7)
# THRs <- vapply(allsets, function(set){ # get THR for all sets
#     correct <- dat[dat$sets == set, 'correct']
#     sum(correct) / length(correct)
#   }, numeric(1))
# names(THRs) <- allsets
# Colors <- c(rep("black", 2), rep("gray50",3), rep("gray",3))
# pdf(file.path('images', 'THR_sets.pdf'))
# plot(THRs, pch = rep(15,length(allsets)), xlab = 'sets', xaxt = 'n', cex = 2.2, ylim = c(0,1),
#      ylab = 'THR', col = Colors) # plot thrs per set, grouped by colour.
# labelnames <- c(1,4,2,5,7,3,6,8) # we omitted the original Set 7 from analysis. Original Set 8 is now Set 7.
# for(set in 1:length(labelnames)){
#   axis(1, at=set, labels=labelnames[set]) # font.lab = 4 (makes bold, but than colours arent distinguished)
# }
# dev.off()
# 
# 
# # ----- this part recalculates the weights of Ht when Ht is not weak and Hu is not true. Weight of Hu gets redistributed over other hypothesis weights as Hu in practice would be omitted if a true informative hypothesis is not weak.
# Hu_included <- dat[dat$sets %in% c(2,3,5,6),]
# False_Hu <- Hu_included[!(Hu_included$sets %in% c(3,6) & Hu_included$ratios != 'ratio4'),] # hu is true in these cases so weight would not be redistributed
# False_Hu <- False_Hu[!(False_Hu$sets %in% c(2,5) & False_Hu$ratios == 'ratio4'),] # hu is also true in these cases, thus omit
# 
# # initialize vectors for weights without and with redistribution
# old_weights_ht <- c()
# new_weights_ht <- c()
# correct2 <- c() # keeps track of false and true selection cases
# # takes ~ 1 minute to run
# for(row in 1:nrow(False_Hu)){
#   # recalculates weight of Ht or Hf if Hu is omitted.
#   hyp_data <- False_Hu[row, colnames(False_Hu) %in% sprintf("H%d", 1:4)] # get weights
#   hyp_data <- hyp_data[!is.na(hyp_data)] # omit NAs from weights vector
#   ht_hs <- False_Hu[row, colnames(False_Hu) %in% sprintf("Weight_H%s", c('t','s'))] # get weights of true and selected hypothesis (this may be the same)
#   if(ht_hs['Weight_Hs'] == hyp_data[length(hyp_data)]){ # if hu is selected, informative hyps are weak and thus no recalculation is needed
#     next
#   }
#   correct2 <- c(correct2, False_Hu[row, 'correct']) # check if this was a false or true seelection case
#   Ht_idx <- match(ht_hs['Weight_Ht'], hyp_data) # index of true
#   hyp_data <- hyp_data[-length(hyp_data)]       # remove unconstrained (which is always the last non-NA value in the weights)
#   old_weights_ht <- c(old_weights_ht, hyp_data[Ht_idx]) # save old weights to compare
#   new_weights <- hyp_data * (1/sum(hyp_data))   # Recalculate weights if weight from Hu is omitted. Suppose weights = c(0.50, 0.30, 0.20) and 0.2 is removed, new probabilities become 0.3 * (1/(0.3+0.5)) and 0.5 * (1/(0.3+0.5)) because 0.3k + 0.2k = 1 must be satisfied.
#   new_weights_ht <- c(new_weights_ht, new_weights[Ht_idx]) # append newly calculated weights
# }
# 
# redistributed <- cbind(new_weights_ht, old_weights_ht, correct2)
# nrow(False_Hu)/length(new_weights_ht) - 1 # hu is only selected in 1% of the cases
# quantile(old_weights_ht, c(0.05, 0.50, 0.95)) #quantiles of weight of Ht if Hu's weight is not redistributed
# quantile(new_weights_ht, c(0.05, 0.50, 0.95)) # quantiles when it is redistributed
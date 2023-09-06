# script contains the functions used in the analysis of the results.

performance_condition <- function(condition){ # 'condition' in this function = (subset of) dataframe of results.
  # calculates performance indicators for some (subset of) the results
  
  niters <- nrow(condition)
  THR <- sum(condition$correct)/niters
  percentiles_Wht <- quantile(condition$Weight_Ht, c(.05, .5, .95))
  names(percentiles_Wht) <- sprintf('Weight_ht_p%d', c(5,50,95))
  percentiles_Whs <- quantile(condition$Weight_Hs, c(.05, .5, .95))
  names(percentiles_Whs) <- sprintf('Weight_hs_p%d', c(5,50,95))

  percentiles_support <- quantile(condition$support, c(.05, .5, .95))
  names(percentiles_support) <- sprintf('Support_p%d', c(5,50,95))
  return(round(c(THR = THR, percentiles_Wht, percentiles_support, 
                 percentiles_Whs, niters = niters),3))
}

effect_per_level <- function(dat, condition){
  # helper function that returns the performance for each level of a specific design factor for (a subset of) the results.
  lvls <- names(table(dat[[condition]]))
  t(sapply(lvls, function(lvl){
    dat_level <- dat[dat[[condition]] == lvl,]
    performance_condition(dat_level)
  }))
}

marginal_effect <- function(dat, condition, plot_out = F, 
                            namey = expression("Median " *" w "["f"]^"S"),
                            MARSlegendpos = "topleft"){ # position of legend in median aggregated relative support plot.
  # This function plots/tabulates performance for a specific design factor.
  
  condition_name <- condition
  lvls <- names(table(dat[[condition_name]]))       # levels per condition
  dat_per_level <- t(sapply(lvls, function(lvl){    # subset the data for each condition level and obtain the performance indicators per level of the design factor.
    dat_level <- dat[dat[[condition_name]] == lvl,]
    performance_condition(dat_level)
  }))
  
  if(!(condition %in% numerical) || !plot_out){     # we usually don't want to plot for categorical data, so return table if design factor is categorical.
    return(dat_per_level)}
  
  if(condition == "true_R2"){                       # necessary to get math notation for R^2 and \rho on labels
    condition <- expression(R^2)
  } else if(condition == "rho") {
    condition <- expression(rho)
  } else {
    condition <- "n"
  }

  dat_per_level_true <- t(sapply(lvls, function(lvl){    # performance per level in true selection cases
    dat_level <- dat[dat[[condition_name]] == lvl & dat[["correct"]] == 1,]
    performance_condition(dat_level)
  }))
  
  dat_per_level_false <- t(sapply(lvls, function(lvl){    # performance per level in false selection cases
    dat_level <- dat[dat[[condition_name]] == lvl & dat[["correct"]] == 0,]
    perf <- performance_condition(dat_level)
    c(perf, p_iters = round(unname(perf["niters"]) / nrow(dat), 3)* 100) # also include proportion of false iterations relative to total
  }))
  
  # allow two plots next to each other in the same window.
  par(mfrow = c(1,2)) 
  
  ## -- Plot THR and aggregated weight of the true hypothesis
  par(mar= c(5.1, 4.4, 4.1, 2.1))
  plot(lvls, dat_per_level[,'THR'], 
       xlab = condition, 
       ylab = expression("THR & median " *" w"["t"]^"S"),
       xaxt = 'n', 
       type = 'b',
       ylim = c(0,1),
       pch = 0)
  axis(1, at = lvls, labels = lvls) # set the values of the design factor on the x axis
  lines(lvls, dat_per_level[,'Weight_ht_p50'], col = 'blue', type = 'b') # add median of weight Ht
  points(lvls, dat_per_level[,'Weight_ht_p5'], col = 'blue', pch = "-", cex = 1.1) # add fifth percentile
  points(lvls, dat_per_level[,'Weight_ht_p95'], col = 'blue', pch = "+", cex = 1.1) # add 95th percentile
  legend('right', legend = c('THR', expression("median w"["t"]^"S"), '95th percentile', '5th percentile'), 
         col = c('black', 'blue', 'blue', 'blue'), cex = 0.70, pch=c(0,1,3,charToRaw("-")))
 
  ## -- plot Median Relative Aggregated Support
  par(mar= c(5.1, 4.1, 4.1, 2.1))
  plot(lvls, dat_per_level[,'Support_p50'], # second plot = for relative support
       xlab = condition, 
       ylab = 'Median Relative Aggregated Support', # Median Relative Aggregated Support
       xaxt = 'n', type = 'b',
       ylim = c(1,(max(dat_per_level_true[,'Support_p50'])+5)),
       col = "blue")
  axis(1, at = lvls, labels = lvls)
  lines(lvls, dat_per_level_true[,'Support_p50'], col = 'olivedrab4', type = 'b', "t") # add median of weight Ht
  lines(lvls, dat_per_level_false[,'Support_p50'], col = 'red', type = 'b', "f") # add median of weight Hf
  legend(MARSlegendpos, legend = c('all cases', 'true cases', 'false cases'), 
         col = c('blue', 'olivedrab4', 'red'), cex = 0.70, pch=c(1, charToRaw("t"), charToRaw("f")))
  axis(3, at = rownames(dat_per_level_false), labels = dat_per_level_false[,"p_iters"], col.axis = "red", cex.axis = 0.8) # give top of figure labels with percentage of wrong iterations of total iterations.
  
  return(dat_per_level) # also return tabulated data
} 

interaction_effect <- function(dat, condition1, condition2, plot_out = T, print_it = F){
  # plots/tabulates THR as interaction of two conditions for (a subset of) the results dataframe
  
  lvls <- lapply(c(condition1, condition2), function(condition){ # get names of the design factors of interest
    names(table(dat[,condition]))
  })
  combinations <- expand.grid(lvls[[1]], lvls[[2]]) # get all unique combinations of the design factors.
  colnames(combinations) <- c(condition1, condition2)
  metrics <- t(sapply(1:nrow(combinations), function(i){ # get per unique combinations the performance indicators
    dat_level <- dat[dat[,condition1] == combinations[i,1] & dat[,condition2] == combinations[i,2],]
    performance_condition(dat_level)
  }))
  combinations <- cbind(combinations, metrics)
  if(!plot_out){return(combinations)} # just return tabular data if no plot is required
  
  par(mfrow = c(1,2)) # create plot for both THR+weights as well as for the relative support.
  colors <- c('red', 'orange', 'blue', 'green', 'purple', 'pink', 'cyan','black','gray') # used colours, in total 9 colours are needed maximum.
  for(i in 1:length(lvls[[1]])){ # for each unique combi of design factors
    tempdat <- combinations[combinations[[condition1]] == lvls[[1]][i], 'THR'] # get THR per unique condition
    if(i == 1){ # only create new plotting environment in first iteration
      plot(tempdat, xlab = condition2, ylab = 'median THR',
           main = paste0('Interaction of ', condition1, ' with ', condition2),
           xaxt = 'n', type = 'b', ylim = c(0,1), col = colors[i])
      axis(1, at = 1:length(tempdat), labels = lvls[[2]])
    } else{ # for the rest, just add the THR
      lines(tempdat, col = colors[i], type = 'b')
    }
  }
  legend('bottomright', col = colors, legend = lvls[[1]], fill = colors) # add legend
  
  for(i in 1:length(lvls[[1]])){ # for each unique combi of design factors
    tempdat <- combinations[combinations[[condition1]] == lvls[[1]][i], 'Support_p50']
    if(i == 1){ # only add new plotting environment for first iteration
      plot(tempdat, xlab = condition2, ylab = 'median Relative Support',
           main = paste0('Interaction of ', condition1, ' with ', condition2),
           xaxt = 'n', type = 'b', ylim = c(1,(max(combinations[,'Support_p50'])+5)), col = colors[i])
      axis(1, at = 1:length(tempdat), labels = lvls[[2]])
    } else{ # in all other cases, just add the THR
      lines(tempdat, col = colors[i], type = 'b')
    }
  }
  
  if(print_it){return(combinations)} # only return tabular data if desired.
}




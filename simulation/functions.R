# contains functions used in simulation and processing of output

print_model <- function(type, npred){
  # function prints the model depending on the type (linear, probit, logit) and number of predictors.
  
  predictors <- paste0(paste(sprintf("V%d", 1:(npred-1)),collapse = "+"), "+", sprintf("V%d", npred), collapse = "")
  model_blueprint <- paste0("lm(y~", predictors, ", data=sample") # blue print for every model, no matter the type.
  switch(type, # dependent on the model type, we add glm() instead of lm() as well as probit or logit link function.
    "linear" = {model <- paste0(model_blueprint, ")")},
    "probit" = {model <- paste0('g', model_blueprint, ", family = binomial(link=probit))")},
    "logit" = {model <- paste0('g', model_blueprint, ", family = binomial(link=logit))")}
  )
  return(model)
}

estimate_betas <- function(true_R2, ratios, mu, sigma, n, model, intercept, type) {
  # returns per simulated study the regression parameter estimates as well as the covariance matrix.
  
  ratio_study <- ratios[1:length(mu)] # depends on how much predictors are used in this study.
  switch(type, # variance of the errors depends on statistical model used in study.
    "linear" = {var.e <- 1-true_R2},  # (because all vars are standardized, var(resid)=1-R2)
    "probit" = {var.e <- pi^2/3},     # assumption by probit
    "logit" = {var.e <- 1}            # assumption by logit
         )
  
  # obtain beta values based on sample R2 and ratios
  fun <- function(x){ # Function that must be minimized (or maximized) to obtain sample R2 value.
    (t(ratio_study*x) %*% sigma %*% ratio_study*x) / (t(ratio_study*x) %*% sigma %*% ratio_study*x + var.e) - true_R2
  }
  x <- uniroot(fun, lower=0, upper=100)$root # sample R2 value
  betas <- ratio_study*x                     # beta estimates for the sample.
  
  # sample predictors and standardize
  sample <- as.data.frame(mvrnorm(n = n, mu, sigma, empirical=FALSE)) # Create sample predictor variables
  sample <- as.data.frame(scale(sample))
  
  # create outcome variable, dependent on model
  switch(type, 
     "linear" = {
       epsilon <- rnorm(n, sd=sqrt(var.e)) # sample error terms
       y <- intercept + as.matrix(sample, ncol = n.coef) %*% matrix(betas, ncol = 1) + epsilon   # sample outcome variables
     },
     "probit" = {
       sample$ystar <- intercept + as.matrix(sample, ncol = n.coef) %*% matrix(betas, ncol = 1)
       sample$prob <- pnorm(sample$ystar)
       sample$y <- rbinom(n, size=1,prob=sample$prob)
     },
     "logit" = {
       sample$ystar <- intercept + as.matrix(sample, ncol = n.coef) %*% matrix(betas, ncol = 1)
       sample$prob <- exp(sample$ystar) / (1 + exp(sample$ystar))
       sample$y <- rbinom(n,size=1,prob=sample$prob)
     }
       )

  # get estimates for model.
  fit <- eval(parse(text=model)) # run model (lm or glm with probit/logit link)
  betahat <- coef(fit)     # get beta estimates
  VCovMatrix <- vcov(fit)  # get vcovmat
  names(betahat) <- sprintf('beta%d', 0:(length(betahat)-1)) # give names which is necessary to run goric()
  
  return(list(ests = betahat, vcovmat = VCovMatrix))
} 

analysis <- function(set, model_types, true_R2, beta_ratios, rho, N, specs, rownum){
  # actual analysis function, wrapper function for needed other functions.
  
  # extract values that belong to this design factor combination condition.
  n.coef <- specs$npred[[model_types]]     # number of predictors
  hyp_set <- specs$hypotheses_sets[[set]]  # set of hypotheses evaluated
  n.hypos <- length(hyp_set)               # number of hypotheses evaluated
  ratio <- specs$ratios[[beta_ratios]]     # ratio of relative importance
  models <- sapply(1:4, function(study){   # models used in different studies
    p = n.coef[study]
    eval(parse(text = specs$models[[model_types]][[study]]))
  })
  Safeguard <- switch(set, 'none', 'unconstrained', 'unconstrained', 'none', 'unconstrained', 'unconstrained', 'complement', 'complement', 'complement') # safeguard depends on which set is evaluated
  model_forms <- switch(model_types, 'hetero' = c('probit','linear', 'linear', 'logit'), {rep('linear',4)})
  
  
  # estimate parameters + vcov matrix
  intercept <- 0 # remains zero over all iterations as everything is standardized
  beta_ests <- lapply(1:4, function(study){ # 1:4 as we always simulate 4 studies
    sigma <- matrix(rho, n.coef[study], n.coef[study]) # correlation matrix of predictors
    diag(sigma) <- 1
    mu <- rep(0, n.coef[study]) # number of predictors depend on which study is simulated. 
    estimate_betas(true_R2, ratio, mu, sigma, N, models[study], intercept, model_forms[study])
  })
  Param_studies <- lapply(beta_ests, function(study){study$ests})    # create list of beta estimates per study
  CovMx_studies <- lapply(beta_ests, function(study){study$vcovmat}) # vcov per study

  # obtain and return results
  results <- GoricEvSynEssentials(S = 4, Param_studies, CovMx_studies, SameHypo = 1, NrHypos = n.hypos, Hypo_studies = hyp_set, Safeguard)
  results_added <- c(rownum, results$added[(4+1),]) # 4+1 = because we need the final weights, which is (number of studies+1)th row. 
  results_equal <- c(rownum, results$equal[(4+1),])

  return(list(results_added, results_equal))
}

GoricEvSynEssentials <- function(S, Param_studies, CovMx_studies, SameHypo, NrHypos, Hypo_studies, Safeguard = "unconstrained", Name_studies = 1:S) {
  # GoricEvSyn() function containing only the essentials, so no data-checks are performed
  # needed, because there were many checks, making the function slow.
  
  NrHypos_incl <- NrHypos + 1
  if(Safeguard == "none"){
    NrHypos_incl <- NrHypos
  }
  
  GORICA_m  <- array(data=NA, dim=c(S,NrHypos_incl))
  weight_m <- array(data=NA, dim=c(S,NrHypos_incl))
  LL <- matrix(NA, nrow = S, ncol = NrHypos_incl)
  PT <- matrix(NA, nrow = S, ncol = NrHypos_incl)
  rownames(LL) <- rownames(PT) <- paste0("Study", 1:S)
  CumulativeGorica <- matrix(NA, nrow = (S+1), ncol = NrHypos_incl)
  CumulativeGoricaWeights <- matrix(NA, nrow = (S+1), ncol = NrHypos_incl)
  rownames(CumulativeGorica) <- rownames(CumulativeGoricaWeights) <- c(paste0("Study", 1:S), "Final")
  
  if(NrHypos == 1 & Safeguard == "complement"){
    namesH <- c("H1", "Hc1")
    rel.weight_mu <- array(data=NA, dim=c(S,1))
  }else if(Safeguard == "none"){
    namesH <- c(paste0("H", 1:NrHypos))
    rel.weight_mu <- array(data=NA, dim=c(S,NrHypos_incl))
  }else{
    namesH <- c(paste0("H", 1:NrHypos), "Hu")
    rel.weight_mu <- array(data=NA, dim=c(S,NrHypos_incl))
  }
  colnames(GORICA_m) <- colnames(weight_m) <- colnames(LL) <- colnames(PT) <- colnames(CumulativeGorica) <- colnames(CumulativeGoricaWeights) <- namesH
  rownames(GORICA_m) <- rownames(rel.weight_mu) <- rownames(weight_m) <- paste0("Study", 1:S)

  if(SameHypo==1){ # if same hypotheses for all studies
    for(HypoTeller in 1:NrHypos){
      eval(parse(text = paste0("H", HypoTeller, " <<- Hypo_studies[(HypoTeller)]")))
    }
  }
  teller <- 0
  teller_k <- 0
  for(s in 1:S){
    #
    if(SameHypo==0){ # if NOT same hypotheses for all studies
      for(HypoTeller in 1:NrHypos){
        eval(parse(text = paste0("H", HypoTeller, " <<- Hypo_studies[(teller+HypoTeller)]")))
      }
    }
    HypoSet <- noquote(paste0("H", 1:NrHypos, collapse = ", "))
    
    # GORICA

    est <- Param_studies[[s]]
    cov <- CovMx_studies[[s]]
    if(length(cov) == 1){cov <- matrix(cov)}

    # Run GORICA
    if(NrHypos == 1 & Safeguard == "complement"){ # vs complement
      eval(parse(text = paste0("res_goric <- restriktor:::goric(est, VCOV = cov, ",
                               HypoSet,
                               ", type = 'gorica', comparison = Safeguard)")))
      rel.weight_mu[s,] <- res_goric$ratio.gw[1, NrHypos_incl]
    } else{ # vs none or unconstrained (default)
      eval(parse(text = paste0("res_goric <- restriktor:::goric(est, VCOV = cov, ",
                               HypoSet,
                               ", type = 'gorica', comparison = Safeguard)")))
      if(Safeguard == "unconstrained"){
        rel.weight_mu[s,] <- res_goric$ratio.gw[, NrHypos_incl]
      }
    }
    LL[s,] <- res_goric$result[,2]
    PT[s,] <- res_goric$result[,3]
    GORICA_m[s,] <- res_goric$result[,4]
    weight_m[s,] <- res_goric$result[,5]
    teller <- teller + NrHypos
  }

  sumLL <- 0
  sumPT <- 0
  CumulativeGoricaEqual <- CumulativeGorica
  CumulativeGoricaWeightsEqual <- CumulativeGoricaWeights
  for(s in 1:S){
    sumLL <- sumLL + LL[s,]
    sumPT <- sumPT + PT[s,]
    CumulativeGorica[s,] <- -2 * sumLL + 2 * sumPT
    CumulativeGoricaEqual[s,] <- -2 * sumLL + 2 * sumPT/s
    minGoric <- min(CumulativeGorica[s,])
    minGoricEqual <- min(CumulativeGoricaEqual[s,])
    CumulativeGoricaWeights[s,] <- exp(-0.5*(CumulativeGorica[s,]-minGoric)) / sum(exp(-0.5*(CumulativeGorica[s,]-minGoric)))
    CumulativeGoricaWeightsEqual[s,] <- exp(-0.5*(CumulativeGoricaEqual[s,]-minGoricEqual)) / sum(exp(-0.5*(CumulativeGoricaEqual[s,]-minGoricEqual)))
  }
  CumulativeGorica[(S+1),] <- CumulativeGorica[S,]
  CumulativeGoricaWeights[(S+1),] <- CumulativeGoricaWeights[S,]
  CumulativeGoricaEqual[(S+1),] <- CumulativeGoricaEqual[S,]
  CumulativeGoricaWeightsEqual[(S+1),] <- CumulativeGoricaWeightsEqual[S,]

  final <- list(added = CumulativeGoricaWeights, 
                equal = CumulativeGoricaWeightsEqual)

  return(final)
  
}

performance <- function(row){ # (row, threshold)
  # gets performance indicators per iteration: whether Ht is selected, the relative support, the weight of Ht and the weight of hypothesis with the highest weight (this can be Ht)
  
  ratio <- row$ratios
  set <- row$sets
  if(ratio == "ratio4"){
    true_hyp <- true_hyps_ratio4[set]
    correct_hyps <- grace_hyps_ratio4[set]
  } else {
    true_hyp <- true_hyps[set]
    correct_hyps <- grace_hyps[set]
  }
  
  # obtain weights information
  weights_NA <- row[(n_conditions+1):ncol(row)] # get all weights, including of hypotheses that did not exist in the set which give NA (this happens because not all sets contain the same number of hypotheses)
  weights <- weights_NA[!is.na(weights_NA)]     # remove NAs, get only weights of hyps that exsisted in the set 
  n <- length(weights)
  highest_weight <- max(weights)
  second_highest <- kit::topn(weights, 2, index=F)[2] # gets second highest weigt; topn() is a bit faster than sort()
  selected <- which(weights == highest_weight) # hyp that is selected
  
  # performance indicators
  selected_Ht <- true_hyp == selected     # check if true hypothesis is the selected hypothesis
  relsup <- highest_weight/second_highest # relative support of hyp with highest weight vs hyp with second highest weight
  grace <- selected %in% correct_hyps[[1]] # check if selected hyp is at least correct, although it might not be most parsimonious
  return(c(selected_Ht, relsup, weights[true_hyp], highest_weight, grace))
}
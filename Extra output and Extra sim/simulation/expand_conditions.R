# this script returns data.frame() of unique design factor combinations
# also returns list from which values are extracted that belong to specific design factor level.
source(file.path('sim', 'functions.R'))       # load in functions
current_date <- "2024-09-22"#Sys.Date()

# specify list of all design factors with their simulated levels. Further explanations of these levels are in GoricEvSyn() manuscript.
conditions_list <- list(
  iters = 1:100,         # Iterations per unique design factor.
  sets = c(5,7),
  models = c('hetero'),  # Hetero = different model types
  true_R2 = c(0.02, 0.15, 0.25),             # True effect size in population. 
  ratios = c('ratio1', 'ratio2'),            # Ratio of population parameter values.
  rho = 0.25,                                # Multicollinearity of predictors.
  N = c(25, 75, 200),                        # Sample size per study.
  n_studies = 20,                            # Number of studies to include.
  inverse_ratio = c(1,0),                    # So that sometimes hu is true, otherwise false.
  outlier_study=c("None", "inverse", "extreme")) # Type of outlier study (None = Same R2 and ratio as other studies, "inverse" = inverse ratio; R2 = 0.02, "extreme" = inverse ratio; R2 = 0.25)


# create hyper grid of all 100 iterations per unique combination of design factor
conditions <- do.call(expand.grid,conditions_list)

# reset values to correct data type and values.
conditions$models <- as.character(conditions$models)
conditions$ratios <- as.character(conditions$ratios)
rownames(conditions) <- 1:nrow(conditions)


# specify the sets of hypotheses used in string format to be used in GoricEvSyn() function
hypotheses <- list(
  "",
  "",
  "",
  "",
  set5 = c(H1 = 'beta1 > beta2; beta2 > beta3', H2 = 'beta1 < beta2; beta1 > beta3'),
  "",
  set7 = c(H1 = 'beta1 > beta2; beta2 > beta3')
  )

# the different models used.
n_study_proportions <- get_n_study_proportion(conditions_list[["n_studies"]])
models <- list(hetero = c(rep("print_model('probit', 3)", n_study_proportions[["one_third"]]), # 'print_model' is a function in ./functions.R that returns the R script in string format of the model based on predictors and regression type.
                          rep("print_model('linear', 3)", n_study_proportions[["five_third"]]), 
                          rep("print_model('logit', 3)", n_study_proportions[["rest"]])))

# relative importance of predictors
ratios <- list(ratio1 = c(4,2,1), # 4,2,1 means that 57% of the true_R2 will be accounted for by the first parameter.
               ratio2 = c(1.77, 1.33, 1))

# number of predictors in the different levels for 'models'
npred <- list(hetero =  rep(3, conditions_list[["n_studies"]]))

# Creates a list of the values that are extracted based on design factor level
# for example: if set == 1 then hyp_set = specs$hypotheses_sets[[1]]
# this gives hyp_set = c(H0 = 'beta3 == 0', H1 = 'beta3 > 0', H2 = 'beta3 < 0')
specs <- list(hypotheses_sets = hypotheses,
              npred = npred,
              models = models,
              ratios = ratios)

# Save the 'specs' and 'conditions' in .RData file to be read in in the simulation script.
rm(models, npred, ratios, conditions_list, hypotheses)
save.image(file.path('sim', sprintf('conditions-%s.RData', current_date)))
rm(list=ls())

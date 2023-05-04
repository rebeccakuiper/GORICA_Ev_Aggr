# this script returns data.frame() of unique design factor combinations
# also returns list from which values are extracted that belong to specific design factor level.

# specify list of all design factors with their simulated levels. Further explanations of these levels are in GoricEvSyn() manuscript.
conditions_list <- list(
  iters = 1:100,                             # iterations per unique design factor
  sets = c(1:6,8,9),                         # sets of hypotheses included, set 7 is same as set 4 and is thus not simulated.
  models = c('equal', 'unequal', 'hetero'),  # equal = same model type + equal predictors, unequal: different predictors, hetero: different model types, equal predictors.
  true_R2 = c(0.02, 0.15, 0.25),             # true effect size in population
  ratios = c('ratio1', 'ratio2', 'ratio3', 'ratio4'), # ratio of population parameter values
  rho = c(0, 0.25, 0.50),                             # multicollinearity of predictors
  N = c(25,50,75,100,150,200,300,500,1000)            # sample size per study
)

# create hyper grid of all 100 iterations per unique combination of design factor
conditions <- do.call(expand.grid,conditions_list)

# these combinations of design factor levels do not exist and are excluded.
conditions <- conditions[!(conditions$models %in% c("equal", "hetero") & (conditions$ratios == 'ratio3' | conditions$ratios == 'ratio4')),]
conditions <- conditions[!(conditions$models == "unequal" & (conditions$ratios == 'ratio1' | conditions$ratios ==  'ratio2')),]

# reset values to correct data type and values.
conditions$models <- as.character(conditions$models)
conditions$ratios <- as.character(conditions$ratios)
rownames(conditions) <- 1:nrow(conditions)


# specify the sets of hypotheses used in string format to be used in GoricEvSyn() function
hypotheses <- list(
  set1 = c(H0 = 'beta3 == 0', H1 = 'beta3 > 0', H2 = 'beta3 < 0'),
  set2 = c(H0 = 'beta1 == 0; beta2 == 0; beta3 == 0',H1 = 'beta1 > beta2; beta2 > beta3', H2 = 'beta1 < beta2; beta1 > beta3'),
  set3 = c(H0 = 'beta1 == 0; beta2 == 0; beta3 == 0',H1 = 'beta2 > beta1; beta2 > beta3', H2 = 'beta1 < beta2; beta2 < beta3'),
  set4 = c(H1 = 'beta3 > 0', H2 = 'beta3 < 0'),  # sets 4-6 do not include H0
  set5 = c(H1 = 'beta1 > beta2; beta2 > beta3', H2 = 'beta1 < beta2; beta1 > beta3'),
  set6 = c(H1 = 'beta2 > beta1; beta2 > beta3', H2 = 'beta1 < beta2; beta2 < beta3'),
  set7 = 'not run',                              # set 7 is same as set 4
  set8 = c(H1 = 'beta1 > beta2; beta2 > beta3'), # = beta1 > beta2 > beta3.
  set9 = c(H1 = 'beta1 < beta2; beta2 > beta3')
  )

# the different models used.
models <- list(equal = rep("print_model('linear', 3)",4), # 'print_model' is a function in ./functions.R that returns the R script in string format of the model based on predictors and regression type.
               unequal = rep("print_model('linear', p)",4), # contains variable 'p', because in unequal condition, p can vary between 3, 4 and 5.
               hetero = c("print_model('probit', 3)",
                          rep("print_model('linear', 3)",2), 
                         "print_model('logit', 3)"))

# relative importance of predictors
ratios <- list(ratio1 = c(4,2,1), # 4,2,1 means that 57% of the true_R2 will be accounted for by the first parameter.
            ratio2 = c(1.77, 1.33, 1),
            ratio3 = seq(4,0),
            ratio4 = c(1,2,3,4,0))

# number of predictors in the different levels for 'models'
npred <- list(equal = c(3,3,3,3), 
              unequal = c(3,4,4,5),
              hetero = c(3,3,3,3))

# Creates a list of the values that are extracted based on design factor level
# for example: if set == 1 then hyp_set = specs$hypotheses_sets[[1]]
# this gives hyp_set = c(H0 = 'beta3 == 0', H1 = 'beta3 > 0', H2 = 'beta3 < 0')
specs <- list(hypotheses_sets = hypotheses,
              npred = npred,
              models = models,
              ratios = ratios)

# Save the 'specs' and 'conditions' in .RData file to be read in in the simulation script.
rm(models, npred, ratios, conditions_list, hypotheses)
save.image(file.path('sim', 'conditions.RData'))
rm(list=ls())

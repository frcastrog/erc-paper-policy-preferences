#------------------------------Power Analysis------------------------------#
#------------------------------------------------- Created: March 26, 2024-#
#-R Version: 4.3.1 ------------------------------- Revised: April 16, 2024-#

# 1) Load packages

pacman::p_load(pwr, xtable, InteractionPoweR, equivalence, DescTools, tidyverse)

# 2) Conduct ANOVA test

# Parameters
effect_size_f <- 0.11    # according to PAP
k <- 5                   # Number of groups
n_per_group <- 1000 / k  # Sample size per group
alpha <- 0.05            # Significance level
power <- 0.80            # Desired power

# Minimum effect size
min_effect_size <- pwr.anova.test(k = k, n = n_per_group, sig.level = alpha, power = power, f = NULL)

# Without the sample size, to get the required sample size per group 
min_sample_size_group <- pwr.anova.test(k = k, n = NULL, f = effect_size_f, sig.level = alpha, power = power)

# 3) Outputs

# Convert the results to data frames for xtable
df_min_effect_size <- as.data.frame(t(as.matrix(unlist(min_effect_size))))
df_min_sample_size_group <- as.data.frame(t(as.matrix(unlist(min_sample_size_group))))

# Create LaTeX code for the tables
xtable(df_min_effect_size, caption = "Minimum Detectable Effect Size")
xtable(df_min_sample_size_group, caption = "Required Sample Size Per Group")

# 4) Equivalence test of control vs. treatments

epsilon <- 0.10
paired <- FALSE 
var.equal <- FALSE
conf.level <- 0.95 # CI

# Liberal trump vs. others
control_group_libtrump <- data_long$policy_opinion[data_long$libtrump == 0]
treatment_group_libtrump <- data_long$policy_opinion[data_long$libtrump == 1]

# Conservative trump vs. others
control_group_contrump <- data_long$policy_opinion[data_long$contrump == 0]
treatment_group_contrump <- data_long$policy_opinion[data_long$contrump == 1]

# Liberal friend vs. others
control_group_libfriend <- data_long$policy_opinion[data_long$libfriend == 0]
treatment_group_libfriend <- data_long$policy_opinion[data_long$libfriend == 1]

# Conservative friend vs. others
control_group_confriend <- data_long$policy_opinion[data_long$confriend == 0]
treatment_group_confriend <- data_long$policy_opinion[data_long$confriend == 1]

# Running TOST
tost_result_libtrump <- tost(control_group_libtrump, treatment_group_libtrump, 
                             epsilon = epsilon, paired = paired, var.equal = var.equal, conf.level = conf.level)

tost_result_contrump <- tost(control_group_contrump, treatment_group_contrump, 
                             epsilon = epsilon, paired = paired, var.equal = var.equal, conf.level = conf.level)

tost_result_libfriend <- tost(control_group_libfriend, treatment_group_libfriend, 
                             epsilon = epsilon, paired = paired, var.equal = var.equal, conf.level = conf.level)

tost_result_confriend <- tost(control_group_confriend, treatment_group_confriend, 
                             epsilon = epsilon, paired = paired, var.equal = var.equal, conf.level = conf.level)


## Output to Latex
tost_results_df <- data.frame(
  Comparison = c("Liberal Trump vs. Others", "Conservative Trump vs. Others", 
                 "Liberal Friend vs. Others", "Conservative Friend vs. Others"),
  Mean_of_X = round(c(0.2488165, 0.2534285, 0.2526389, 0.2561651), 3),
  Mean_of_Y = round(c(0.2711771, 0.2529666, 0.2561111, 0.2418631), 3),
  DF = round(c(2813.2, 2908.1, 2762.5, 2713.8), 3),
  Epsilon = rep(0.1, 4),
  CI_Lower = round(c(-0.06164852, -0.03840239, -0.04303112, -0.02551584), 3),
  CI_Upper = round(c(0.01692726, 0.03932626, 0.03608668, 0.05411998), 3),
  P_Value = round(c(0.0005806145, 0.00001292112, 0.0000305196, 0.0002024023), 3)
)

xtable(tost_results_df, caption = "Welch Two Sample TOST Results", label = "tab:tost_results", 
       align = "lcccccccc") 

# 5) Power analysis of interactions

## Get correlations between variables
set.seed(12345)

# - First necessary correlation:
# - Correlation between Y (policy opinion) and X1 (treatment) assumed to be close to zero 
# - as treatment is randomized. It'll be zero for all the interactions

xy_cor <- 0.00

# - Second necessary correlation: between the interacting variable X2 and Y (policy opinion)
# - Different scenarios:
#   (1) X2 = party ID: lower value (democrat) assumed to be correlated to higher values
#       of policy positions (liberal position), therefore, negative correlation.
#   (2) X2 = Trump approval: lower value (disapproves) assumed to be correlated to higher values
#       of policy positions (liberal position), therefore, negative correlation.
#   (3) X2 = Political knowledge: higher values (more knowledge) assumed to be correlated to higher values
#       of policy positions (liberal position), therefore, positive correlation.
#   (4) X2 = Social conformism: higher values (more conformism) assumed to be correlated to lower values
#      of policy positions (conservative position), therefore, negative correlation.

x2y_cor <- -0.05

x2y_cor2 <- 0.05 # only for political knowledge interaction

# - Third necessary correlation: between X1 (treatment) and interacting variable. 
# - is assumed at zero since the treatment is random

x1x2_cor <- 0.00

# - Finally, the interaction effect between X1 X2 and Y is assumed to be amplified
# - when compared to x2y_cor and x2y_cor2, therefore:

x1x2y_cor <- -0.10

x1x2y_cor2 <- 0.10  # only for political knowledge interaction


# - First, using variable correlations, we conduct an overall power for each group sample
# - of n = 200

power_group <- power_interaction_r2(
  alpha = 0.05,             # alpha, for the power analysis
  N = 200,                  # sample size
  r.x1x2.y = .20,           # interaction effect to test (correlation between x1*x2 and y)
  r.x1.y = .0,              # correlation between x1 and y
  r.x2.y = .15,              # correlation between x2 and y
  r.x1.x2 = .0)              # correlation between x1 and x2

power_group


# - Calculation of power for interactions (1), (2) and (4)
inter_power_1 <- power_interaction(
  n.iter = 10000,           # number of simulations per unique combination of input parameters
  alpha = 0.05,             # alpha, for the power analysis
  N = 1000,                 # sample size
  r.x1x2.y = x1x2y_cor,     # interaction effect to test (correlation between x1*x2 and y)
  r.x1.y = xy_cor,          # correlation between x1 and y
  r.x2.y = x2y_cor,         # correlation between x2 and y  
  r.x1.x2 = x1x2_cor,       # correlation between x1 and x2 
  k.y =  3,                 # categories `policy_opinion`
  k.x1 = 5)                 # 5 treatment groups (4 treatment + 1 control)

inter_power_1


# - Calculation of power for interaction (4)
inter_power_2 <- power_interaction(
  n.iter = 10000,           # number of simulations per unique combination of input parameters
  alpha = 0.05,             # alpha, for the power analysis
  N = 1000,                 # sample size
  r.x1x2.y = x1x2y_cor2,     # interaction effect to test (correlation between x1*x2 and y)
  r.x1.y = xy_cor,          # correlation between x1 and y
  r.x2.y = x2y_cor2,         # correlation between x2 and y  
  r.x1.x2 = x1x2_cor,       # correlation between x1 and x2 
  k.y =  3,                 # categories `policy_opinion`
  k.x1 = 5)                 # 5 treatment groups (4 treatment + 1 control)

inter_power_2








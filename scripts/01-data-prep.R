#-------------------------------Data Preparation-------------------------------#
#-Author: Francisca Castro ----------------------- Created: September 18, 2023-#
#-R Version: 4.3.1 ------------------------------- Revised: September 19, 2023-#

# 1) Load necessary packages

pacman::p_load(haven, dplyr, tidyr, magrittr)

# 2) Load data

data_raw <- read_sav("data/01-raw-data/CCES21_BGU_OUTPUT_spss_old.sav")

str(data_raw)

names(data_raw)

## Data cleaning

### NAs replacement
# Some of the varaibles have `__NA__` instead of normal NA, so it's necessary to
# change that. 

data_clean <- data_raw %>%
  mutate_if(is.character, ~ ifelse(. == "__NA__", NA, .)) 

# 3) Create new variables of experimental treatments/policy positions

# Given that the policy positions are constructed giving NAs to all of the individuals
# who didn't responded that prompt given that they were in a different experimental
# treatment, in order to conduct further comparisons is necessary to combine
# the policy positions. Nine new variables are created for each policy positions,
# and for each position, the experimental treatments are combined

data_new <- data_clean %>%
  mutate(minimum_wage = coalesce(BGU_control1, BGU_Trumplib1, BGU_Trumpcon1, BGU_friendlib1, BGU_friendcon1),
         taxes = coalesce(BGU_control2, BGU_Trumplib2, BGU_Trumpcon2, BGU_friendlib2, BGU_friendcon2),
         abortion = coalesce(BGU_control3, BGU_Trumplib3, BGU_Trumpcon3, BGU_friendlib3, BGU_friendcon3),
         immigration = coalesce(BGU_control4, BGU_Trumplib4, BGU_Trumpcon4, BGU_friendlib4, BGU_friendcon4),
         guns = coalesce(BGU_control5, BGU_Trumplib5, BGU_Trumpcon5, BGU_friendlib5, BGU_friendcon5),
         health = coalesce(BGU_control6, BGU_Trumplib6, BGU_Trumpcon6, BGU_friendlib6, BGU_friendcon6),
         b_checks = coalesce(BGU_control7, BGU_Trumplib7, BGU_Trumpcon7, BGU_friendlib7, BGU_friendcon7),
         climate = coalesce(BGU_control8, BGU_Trumplib8, BGU_Trumpcon8, BGU_friendlib8, BGU_friendcon8),
         p_parent = coalesce(BGU_control9, BGU_Trumplib9, BGU_Trumpcon9, BGU_friendlib9, BGU_friendcon9))

# Check if the variables were properly constructed
table(data_clean$BGU_control1, data_new$minimum_wage)
table(data_clean$BGU_Trumplib1, data_new$minimum_wage)
table(data_clean$BGU_friendlib1, data_new$minimum_wage)
table(data_clean$BGU_control1, data_new$minimum_wage)
table(data_clean$BGU_control1, data_new$minimum_wage)

table(data_new$BGU_group)

# 4) Convert the variables of interest to numeric
data_new %<>%
  mutate(BGU_group = as.factor(BGU_group),
         BGU_group = factor(BGU_group, levels = c(1, 2, 3, 4, 5), 
                            labels = c("control", "TL", "TC", "CFL", "CFC")),
         minimum_wage = as.numeric(minimum_wage),
         taxes = as.numeric(taxes),
         abortion = as.numeric(abortion),
         immigration = as.numeric(immigration),
         guns = as.numeric(guns),
         health = as.numeric(health),
         b_checks = as.numeric(b_checks),
         climate = as.numeric(climate),
         p_parent = as.numeric(p_parent))

# 5) Replace `don't know` (category 9) in policy positions as NAs

data_new %<>%
  mutate(minimum_wage = replace(minimum_wage, minimum_wage == 9, NA),
         taxes = replace(taxes, taxes == 9, NA),
         abortion = replace(abortion, abortion == 9, NA),
         immigration = replace(immigration, immigration == 9, NA),
         guns = replace(guns, guns == 9, NA),
         health = replace(health, health == 9, NA),
         b_checks = replace(b_checks, b_checks == 9, NA),
         climate = replace(climate, climate == 9, NA),
         p_parent = replace(p_parent, p_parent == 9, NA))

table(data_new$minimum_wage, exclude = NULL)

# 6) Replace 1 and 2 policy positions for 0 and 1. 0 for oppose 1 for support

data_new %<>%
  mutate(
    minimum_wage = ifelse(minimum_wage == 2, 0, minimum_wage),
    taxes = ifelse(taxes == 2, 0, taxes),
    abortion = ifelse(abortion == 2, 0, abortion), #oppose
    immigration = ifelse(immigration == 2, 0, immigration),
    guns = ifelse(guns == 2, 0, guns), #oppose
    health = ifelse(health == 2, 0, health),
    b_checks = ifelse(b_checks == 2, 0, b_checks),
    climate = ifelse(climate == 2, 0, climate),
    p_parent = ifelse(p_parent == 2, 0, p_parent)
  )

table(data_new$minimum_wage, exclude = NULL)

# 7) Change the order of some policy positions

# For the majority of policy positions, the liberal option is the support option
# whereas the oppose (2) stands for the conservative one, except in two cases:
# Abortion: Do you support or oppose enforcing penalties on women who obtain abortions? Liberal position - oppose (2)
# Guns: Do you support or oppose allowing teachers to carry guns on school property? Liberal position - oppose (2)

# To maintain consistency, the positions will be switched so the lower number also shows the conservative option
# and the higher number. Reminder: now oppose is 0 based on previous changes.

table(data_new$abortion)
table(data_new$guns)

data_new %<>%
  mutate(abortion_recoded = ifelse(abortion == 0, 1, 0),
         guns_recoded = ifelse(guns == 0, 1, 0))

table(data_new$abortion_recoded)
table(data_new$guns_recoded)

# Save data
save(data_new, file = "/Users/franciscacastro/Dropbox/Shared_ERC_Francisca_and_Jenny/r-project/data/02-processed-data/data_final.Rdata")

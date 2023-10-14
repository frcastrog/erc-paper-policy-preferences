#-------------------------------Data Preparation-------------------------------#
#-Author: Francisca Castro ----------------------- Created: September 18, 2023-#
#-R Version: 4.3.1 ------------------------------- Revised: September 27, 2023-#

# 1) Load packages

pacman::p_load(haven, dplyr, tidyr, magrittr, forcats)

# 2) Load data

data_raw <- read_sav("data/01-raw-data/CCES21_BGU_OUTPUT_spss_old.sav")

str(data_raw)

names(data_raw)

# 3) Data cleaning

### NAs replacement
# Some of the varaibles have `__NA__` instead of normal NA, so it's necessary to
# change that. 

data_raw %<>%
  mutate_if(is.character, ~ ifelse(. == "__NA__", NA, .)) 

data_raw %<>%
  mutate(
    BGU_group = as.numeric(as.character(BGU_group)),
    race = as.numeric(as.character(race)),
    gender4 = as.numeric(as.character(gender4)),
    BGU_Trumpapproval = as.numeric(as.character(BGU_Trumpapproval)),
    CC21_330a = as.numeric(as.character(CC21_330a)),
    pid3 = as.numeric(as.character(pid3)),
    faminc_new = as.numeric(as.character(faminc_new)),
    educ = as.numeric(as.character(educ)),
    BGU_conf1 = as.numeric(as.character(BGU_conf1)),
    BGU_conf2 = as.numeric(as.character(BGU_conf2)),
    BGU_conf3 = as.numeric(as.character(BGU_conf3)),
    BGU_conf4 = as.numeric(as.character(BGU_conf4)),
    BGU_conf5 = as.numeric(as.character(BGU_conf5)),
    BGU_conf6 = as.numeric(as.character(BGU_conf6))
  )

### Keep only necessary variables
# So far: caseid, teamweight, birthyr, educ, race, CC21_330a (self ideology),
# pid3 (3 point party ID), pid7 (7 point party id), gender4, BGU_Trumpapproval,
# CC21_315a (Biden approval).
# Plus the variable on the treatment BGU_group
# and all the variables related to policy support: 
#"BGU_control1"          "BGU_control2"          "BGU_control3"          "BGU_control4"          "BGU_control5"         
# "BGU_control6"          "BGU_control7"          "BGU_control8"          "BGU_control9"          "BGU_Trumplib1"        
# "BGU_Trumplib2"         "BGU_Trumplib3"         "BGU_Trumplib4"         "BGU_Trumplib5"         "BGU_Trumplib6"        
# "BGU_Trumplib7"         "BGU_Trumplib8"         "BGU_Trumplib9"         "BGU_Trumpcon1"         "BGU_Trumpcon2"        
# "BGU_Trumpcon3"         "BGU_Trumpcon4"         "BGU_Trumpcon5"         "BGU_Trumpcon6"         "BGU_Trumpcon7"        
# "BGU_Trumpcon8"         "BGU_Trumpcon9"         "BGU_friendlib1"        "BGU_friendlib2"        "BGU_friendlib3"       
# "BGU_friendlib4"        "BGU_friendlib5"        "BGU_friendlib6"        "BGU_friendlib7"        "BGU_friendlib8"       
# "BGU_friendlib9"        "BGU_friendcon1"        "BGU_friendcon2"        "BGU_friendcon3"        "BGU_friendcon4"       
# "BGU_friendcon5"        "BGU_friendcon6"        "BGU_friendcon7"        "BGU_friendcon8"        "BGU_friendcon9"    

data_filtered <- data_raw %>%
  select(caseid, teamweight, birthyr, educ, race, CC21_330a, pid3, pid7, gender4, 
         BGU_Trumpapproval, CC21_315a, BGU_group,
         BGU_control1, BGU_control2, BGU_control3, BGU_control4, BGU_control5,
         BGU_control6, BGU_control7, BGU_control8, BGU_control9, BGU_Trumplib1,
         BGU_Trumplib2, BGU_Trumplib3, BGU_Trumplib4, BGU_Trumplib5, BGU_Trumplib6,
         BGU_Trumplib7, BGU_Trumplib8, BGU_Trumplib9, BGU_Trumpcon1, BGU_Trumpcon2,
         BGU_Trumpcon3, BGU_Trumpcon4, BGU_Trumpcon5, BGU_Trumpcon6, BGU_Trumpcon7,
         BGU_Trumpcon8, BGU_Trumpcon9, BGU_friendlib1, BGU_friendlib2, BGU_friendlib3,
         BGU_friendlib4, BGU_friendlib5, BGU_friendlib6, BGU_friendlib7, BGU_friendlib8,
         BGU_friendlib9, BGU_friendcon1, BGU_friendcon2, BGU_friendcon3, BGU_friendcon4,
         BGU_friendcon5, BGU_friendcon6, BGU_friendcon7, BGU_friendcon8, BGU_friendcon9,
         BGU_conf1,BGU_conf2,BGU_conf3,BGU_conf4,BGU_conf5,BGU_conf6)


### Now, let's modify the database so it's in long format

data_long <- data_filtered %>%
  pivot_longer(
    cols = c(
      BGU_control1, BGU_control2, BGU_control3, BGU_control4, BGU_control5,
      BGU_control6, BGU_control7, BGU_control8, BGU_control9, BGU_Trumplib1,
      BGU_Trumplib2, BGU_Trumplib3, BGU_Trumplib4, BGU_Trumplib5, BGU_Trumplib6,
      BGU_Trumplib7, BGU_Trumplib8, BGU_Trumplib9, BGU_Trumpcon1, BGU_Trumpcon2,
      BGU_Trumpcon3, BGU_Trumpcon4, BGU_Trumpcon5, BGU_Trumpcon6, BGU_Trumpcon7,
      BGU_Trumpcon8, BGU_Trumpcon9, BGU_friendlib1, BGU_friendlib2, BGU_friendlib3,
      BGU_friendlib4, BGU_friendlib5, BGU_friendlib6, BGU_friendlib7, BGU_friendlib8,
      BGU_friendlib9, BGU_friendcon1, BGU_friendcon2, BGU_friendcon3, BGU_friendcon4,
      BGU_friendcon5, BGU_friendcon6, BGU_friendcon7, BGU_friendcon8, BGU_friendcon9
    ),
    names_to = "policy_positions_raw",
    values_to = "policy_opinion" # shows support oppose 
  )

### Create a new policy position variable that stores all the 9 policy positions where:
# 1 minimum wage 2 taxes 3 abortion 4 immigration 5 guns 6 health care 7 background checks
# 8 climate change 9 planned parenthood

data_long <- data_long %>%
  mutate(policy_position = as.numeric(sub(".*([0-9]+)$", "\\1", policy_positions_raw)))

### Modify policy_opinion
# Following Barber & Pope, they code the policy positions as following:
# 1 support 0 don't know -1 oppose
# let's first see how the original variable looks like

table(data_long$policy_opinion)
#1    2    9 
#5162 2882  956 

# 1 (supports) stay the same. 2 (oppose) becomes -1, and 9 (don't know) becomes 0.

data_long %<>%
  mutate(policy_opinion = ifelse(policy_opinion == 9, 0, ifelse(policy_opinion == 2, -1, policy_opinion)))

table(data_long$policy_opinion)
#-1    0    1 
#2882  956 5162 


### Create experimental treatment conditions based on BGU_group
table(data_long$BGU_group)

data_long %<>%
  mutate(control = if_else(BGU_group == 1, 1, 0),
         libtrump = if_else(BGU_group == 2, 1, 0),
         contrump = if_else(BGU_group == 3, 1, 0),
         libfriend = if_else(BGU_group == 4, 1, 0),
         confriend = if_else(BGU_group == 5, 1, 0))

### Double-check consistency
table(data_long$control, data_long$BGU_group)
table(data_long$confriend, data_long$BGU_group)

table(data_long$policy_opinion, data_long$policy_position,  exclude = NULL) # 4000 NAs
table(data_filtered$BGU_control2, exclude = NULL) #usually around 800 NAs
table(data_filtered$BGU_friendcon5, exclude = NULL)
table(data_filtered$BGU_Trumplib9, exclude = NULL)
table(data_filtered$BGU_Trumplib5, exclude = NULL)
# Given that there's usually 800 NAS per position, and there are 5 experimental treatments,
# it's expected that there will be 4000 NAs per policy positio


###############################################################################

# 4) Additional data in wide format (might be useful for testing things)

data_wide <- data_filtered %>%
  mutate(minimum_wage = coalesce(BGU_control1, BGU_Trumplib1, BGU_Trumpcon1, BGU_friendlib1, BGU_friendcon1),
         taxes = coalesce(BGU_control2, BGU_Trumplib2, BGU_Trumpcon2, BGU_friendlib2, BGU_friendcon2),
         abortion = coalesce(BGU_control3, BGU_Trumplib3, BGU_Trumpcon3, BGU_friendlib3, BGU_friendcon3),
         immigration = coalesce(BGU_control4, BGU_Trumplib4, BGU_Trumpcon4, BGU_friendlib4, BGU_friendcon4),
         guns = coalesce(BGU_control5, BGU_Trumplib5, BGU_Trumpcon5, BGU_friendlib5, BGU_friendcon5),
         health = coalesce(BGU_control6, BGU_Trumplib6, BGU_Trumpcon6, BGU_friendlib6, BGU_friendcon6),
         b_checks = coalesce(BGU_control7, BGU_Trumplib7, BGU_Trumpcon7, BGU_friendlib7, BGU_friendcon7),
         climate = coalesce(BGU_control8, BGU_Trumplib8, BGU_Trumpcon8, BGU_friendlib8, BGU_friendcon8),
         p_parent = coalesce(BGU_control9, BGU_Trumplib9, BGU_Trumpcon9, BGU_friendlib9, BGU_friendcon9))

### Convert the variables of interest to numeric
data_wide %<>%
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

### ### Modify policy_opinion
# Following Barber & Pope, they code the policy positions as following:
# 1 support 0 don't know -1 oppose

# Example
table(data_wide$abortion)

data_wide %<>%
  mutate(minimum_wage = ifelse(minimum_wage == 9, 0, ifelse(minimum_wage == 2, -1, minimum_wage)),
         taxes = ifelse(taxes == 9, 0, ifelse(taxes == 2, -1, taxes)),
         abortion = ifelse(abortion == 9, 0, ifelse(abortion == 2, -1, abortion)),
         immigration = ifelse(immigration == 9, 0, ifelse(immigration == 2, -1, immigration)),
         guns = ifelse(guns == 9, 0, ifelse(guns == 2, -1, guns)),
         health = ifelse(health == 9, 0, ifelse(health == 2, -1, health)),
         b_checks = ifelse(b_checks == 9, 0, ifelse(b_checks == 2, -1, b_checks)),
         climate = ifelse(climate == 9, 0, ifelse(climate == 2, -1, climate)),
         p_parent = ifelse(p_parent == 9, 0, ifelse(p_parent == 2, -1, p_parent)))

table(data_wide$abortion)

# Save data
save(data_long, file = "~/Dropbox/Shared_ERC_Francisca_and_Jenny/r-project/data/02-processed-data/data_long.Rdata")
save(data_raw, file = "~/Dropbox/Shared_ERC_Francisca_and_Jenny/r-project/data/02-processed-data/data_raw.Rdata")
save(data_wide, file = "~/Dropbox/Shared_ERC_Francisca_and_Jenny/r-project/data/02-processed-data/data_wide.Rdata")



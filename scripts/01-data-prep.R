#-------------------------------Data Preparation-------------------------------#
#-Author: Francisca Castro ----------------------- Created: September 18, 2023-#
#-R Version: 4.3.1 --------------------------------- Revised: October 14, 2023-#

# 1) Load packages

pacman::p_load(haven, dplyr, tidyr, magrittr, forcats, ggplot2, psych, xtable)

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
    BGU_conf6 = as.numeric(as.character(BGU_conf6)),
    BGU_knowledge1 = as.numeric(as.character(BGU_knowledge1)),
    BGU_knowledge2 = as.numeric(as.character(BGU_knowledge2)),
    BGU_knowledge3 = as.numeric(as.character(BGU_knowledge3)),
    BGU_knowledge4 = as.numeric(as.character(BGU_knowledge4)),
    BGU_knowledge5 = as.numeric(as.character(BGU_knowledge5)),
    BGU_knowledge6 = as.numeric(as.character(BGU_knowledge6)),
    CC21_330a = as.numeric(as.character(CC21_330a)), #ideology
    pid3 = as.numeric(as.character(pid3)) #party id
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
         BGU_conf1,BGU_conf2,BGU_conf3,BGU_conf4,BGU_conf5,BGU_conf6,
         BGU_knowledge1,BGU_knowledge2,BGU_knowledge3,BGU_knowledge4,BGU_knowledge5,
         BGU_knowledge6)


### Now, let's modify the database so it's in long formatm, as in Barber & Pope

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

# 4) Data in wide format (to test each policy position)

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

### Modify policy_opinion
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

# 5) Create interaction variables

### Political knowledge

data_wide %<>%
  mutate(knowledge_index = as.integer(BGU_knowledge1 == 1) + # Constitution
           as.integer(BGU_knowledge2 == 1) + # Deficit
           as.integer(BGU_knowledge3 == 6) + # Term
           as.integer(BGU_knowledge4 == 1) + # Spending
           as.integer(BGU_knowledge5 == 1) + # Nomination
           as.integer(BGU_knowledge6 == 3)) # Veto

data_wide$knowledge_index <- as.numeric(data_wide$knowledge_index)

table(data_wide$knowledge_index)

#### Plot for the appendix

pol_knowledge_plot <- data_wide %>%
  count(knowledge_index) %>%
  ggplot(aes(x = knowledge_index, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of Correct Answers",
       y = "Number of Observations") +
  scale_x_continuous(breaks = seq(min(data_wide$knowledge_index), max(data_wide$knowledge_index), by = 1)) +
  scale_y_continuous(limits = c(0, 250)) +  
  theme_minimal()

ggsave("pol_knowledge.png", pol_knowledge_plot, width = 8, height = 6, dpi = 600)


### Ideology and party id

table(data_wide$CC21_330a) # 1 very liberal 7 very conservative
                            # 8 is 'not sure" so it'll be dropped

data_wide %<>%
  mutate(ideology = ifelse(CC21_330a == 8, NA, CC21_330a))

table(data_wide$pid3)

data_wide %<>%
  mutate(party_id = case_when(
    pid3 == 1 ~ "democrat",
    pid3 == 2 ~ "republican",
    pid3 %in% 3:5 ~ "independent/other"))


### Social conformism

# Please indicate the extent to which you agree or disagree with the following statement:
  
#- BGU_conf1 It’s best for everyone if people try to fit in instead of acting in unusual ways.
#- BGU_conf2 People should be encouraged to express themselves in unique and possibly unusual ways. 
#- BGU_conf3 Obeying the rules and fitting in are signs of a strong and healthy society.
#- BGU_conf4 People who continually emphasize the need for unity will only limit creativity and hurt our society.
#- BGU_conf5 We should admire people who go their own way without worrying about what others think.
#- BGU_conf6 People need to learn to fit in and get along with others.

#1   Strongly agree
#2   Agree
#3   Neither agree nor disagree
#4   Disagree
#5   Strongly disagree
#9   Don't know

#Given that some statements are phrased in a way that measure non-conformism or individualism, we need to change that so all the statements range from low levels of conformism to high levels of conformism.

#Questions that need to be reordered: BGU_conf2, BGU_conf4, and BGU_conf5

#Additionally, neither agree nor disagree and don't know will be mixed in the same one. New categories will be:
#1. Strongly disagree (low social conformism) - 4 and 5
#2. Don't know/neither agree or disagree - 9 and 3
#3. Agree/strongly agree (high social conformism) - 2 and 1

data_wide %<>%
  mutate(
    BGU_conf1_rec = recode(BGU_conf1, 
                           `1` = 3, `2` = 3,  #agree/strongly agree (high SC)
                           `3` = 2, `9` = 2,  #don't know, neither
                           `4` = 1, `5` = 1), #disagree/strongly disagree (low SC)
    
    BGU_conf2_rec = recode(BGU_conf2, 
                           `1` = 1, `2` = 1,  #agree/strongly agree (low SC)
                           `3` = 2, `9` = 2,  #don't know, neither
                           `4` = 3, `5` = 3), #disagree/strongly disagree (high SC)
    
    BGU_conf3_rec = recode(BGU_conf3, 
                           `1` = 3, `2` = 3,  #agree/strongly agree (high SC)
                           `3` = 2, `9` = 2,  #don't know, neither
                           `4` = 1, `5` = 1), #disagree/strongly disagree (low SC)
    
    BGU_conf4_rec = recode(BGU_conf4, 
                           `1` = 1, `2` = 1,  #agree/strongly agree (low SC)
                           `3` = 2, `9` = 2,  #don't know, neither
                           `4` = 3, `5` = 3), #disagree/strongly disagree (high SC)
    
    BGU_conf5_rec = recode(BGU_conf5, 
                           `1` = 1, `2` = 1,  #agree/strongly agree (low SC)
                           `3` = 2, `9` = 2,  #don't know, neither
                           `4` = 3, `5` = 3), #disagree/strongly disagree (high SC)
    
    BGU_conf6_rec = recode(BGU_conf6, 
                           `1` = 3, `2` = 3,  #agree/strongly agree (high SC)
                           `3` = 2, `9` = 2,  #don't know, neither
                           `4` = 1, `5` = 1))  #disagree/strongly disagree (low SC)

table(data_wide$BGU_conf6_rec) #validation

### Test for Cronbach's alpha

items_sci_full <- data_wide %>% 
  select(BGU_conf1_rec, BGU_conf2_rec, BGU_conf3_rec, BGU_conf4_rec, BGU_conf5_rec, BGU_conf6_rec)

items_sci <- data_wide %>% 
  select(BGU_conf1_rec, BGU_conf2_rec, BGU_conf3_rec, BGU_conf5_rec, BGU_conf6_rec)
####The 4th item was removed, reliability of the index is better without it

ca_sci <- alpha(items_sci, check.keys=TRUE)
ca_sci2 <- alpha(items_sci_full, check.keys=TRUE)
print(ca_sci)


####Get a latex output
item_stats <- as.data.frame(ca_sci$item.stats)
item_stats2 <- as.data.frame(ca_sci2$item.stats)

latex_table_items <- xtable(item_stats)
latex_table_items2 <- xtable(item_stats2)
print(latex_table_items, type = 'latex') 
print(latex_table_items2, type = 'latex') 

### Creation of the social conformism index (SCI) as a composite score

data_wide %<>%
  rowwise() %>%
  mutate(SCI = mean(c_across(c(BGU_conf1_rec, BGU_conf2_rec, BGU_conf3_rec, BGU_conf5_rec, BGU_conf6_rec)), na.rm = TRUE)) %>%
  ungroup()

table(data_wide$SCI)

# Save data
### Mac
save(data_long, file = "~/Dropbox/Shared_ERC_Francisca_and_Jenny/r-project/data/02-processed-data/data_long.Rdata")
save(data_raw, file = "~/Dropbox/Shared_ERC_Francisca_and_Jenny/r-project/data/02-processed-data/data_raw.Rdata")
save(data_wide, file = "~/Dropbox/Shared_ERC_Francisca_and_Jenny/r-project/data/02-processed-data/data_wide.Rdata")

### Windows
save(data_long, file = "C:/Users/Francisca/Dropbox/Shared_ERC_Francisca_and_Jenny/r-project/data/02-processed-data/data_long.Rdata")
save(data_raw, file = "C:/Users/Francisca/Dropbox/Shared_ERC_Francisca_and_Jenny/r-project/data/02-processed-data/data_raw.Rdata")
save(data_wide, file = "C:/Users/Francisca/Dropbox/Shared_ERC_Francisca_and_Jenny/r-project/data/02-processed-data/data_wide.Rdata")

#------------------------------Replication Script------------------------------#
#-------------------------------------------------- Created: December 27, 2023-#
#-R Version: 4.4.2 -------------------------------- Revised: December 24, 2024-#

# 1) Load packages

pacman::p_load(haven, dplyr, tidyverse, magrittr, forcats, ggplot2, psych, xtable,
               broom, ggeffects, gridExtra, knitr, modelsummary, purrr, kableExtra,
               pwr)

options(scipen = 999)

# 1.1) Data

# - This script requires the data produced by `00-data-prep`
# - Alternative, one can load the data available in `data/derived-data/`

readRDS("data/derived-data/data_long.rds")


# 1.2) Helpers

# Helper function for calculating confidence intervals
calculate_ci <- function(model, conf_level, relabel_map) {
  tidy(model, conf.int = TRUE, conf.level = conf_level) %>%
    filter(term %in% names(relabel_map)) %>%
    mutate(term = recode(term, !!!relabel_map))
}

# Define treatment group mapping
treatment_labels <- c(
  "treatment_groupTL" = "Liberal Trump",
  "treatment_groupTC" = "Conservative Trump",
  "treatment_groupCFL" = "Liberal Close Friend",
  "treatment_groupCFC" = "Conservative Close Friend"
)

# Helper function for relabeling terms
relabel_terms <- function(data, label_map) {
  data %>%
    mutate(term = if_else(term %in% names(label_map), label_map[term], term)) %>%
    filter(term != "(Intercept)")
}

# 2) Paper replication

### Figure 1 - Average treatment effect of policy cues 
# - Replication of Figure 1 ATE Across Issues on Barber & Pope (2019) 
# - but without separating by party

model1 <- lm(policy_opinion ~ male + pid3 + treatment_group, 
             data = data_long, weights = teamweight) # with weights

# Calculate confidence intervals
ate_m1_95 <- calculate_ci(model1, conf_level = 0.95, relabel_map = treatment_labels)
ate_m1_90 <- calculate_ci(model1, conf_level = 0.90, relabel_map = treatment_labels) %>%
  select(term, conf.low, conf.high) %>%
  rename(conf.low_90 = conf.low, conf.high_90 = conf.high)

# Merge 90% CI into 95% data and set factor levels
ate_m1 <- ate_m1_95 %>%
  left_join(ate_m1_90, by = "term") %>%
  mutate(term = factor(term, levels = rev(treatment_labels)))

# Plot
ate_plot <- ggplot(ate_m1, aes(y = term, x = estimate)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 0.5) +
  geom_errorbarh(aes(xmin = conf.low_90, xmax = conf.high_90), height = 0, linewidth = 1.2, alpha = 0.7) +
  geom_point(size = 3) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(y = "Treatment Conditions", x = "Average Treatment Effect") +
  scale_x_continuous(limits = c(-0.25, 0.25)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20, l = 10))
  )

ate_plot

ggsave("outputs/figures/ate_plot_2.png", plot = ate_plot, dpi = 600, 
       width = 6, height = 4)


### Figure 2 - Interaction with Party ID 
# Relabel terms
term_labels <- c(
  "treatment_groupTL" = "Democrat:Liberal Trump",
  "treatment_groupTC" = "Democrat:Conservative Trump",
  "treatment_groupCFL" = "Democrat:Liberal Close Friend",
  "treatment_groupCFC" = "Democrat:Conservative Close Friend",
  "party_idIndependent/Other:treatment_groupTL" = "Independent/Other:Liberal Trump",
  "party_idRepublican:treatment_groupTL" = "Republican:Liberal Trump",
  "party_idIndependent/Other:treatment_groupTC" = "Independent/Other:Conservative Trump",
  "party_idRepublican:treatment_groupTC" = "Republican:Conservative Trump",
  "party_idIndependent/Other:treatment_groupCFL" = "Independent/Other:Liberal Close Friend",
  "party_idRepublican:treatment_groupCFL" = "Republican:Liberal Close Friend",
  "party_idIndependent/Other:treatment_groupCFC" = "Independent/Other:Conservative Close Friend",
  "party_idRepublican:treatment_groupCFC" = "Republican:Conservative Close Friend"
)

# Model estimation
model2 <- lm(policy_opinion ~ male + party_id + treatment_group * party_id, 
             data = data_long, weights = teamweight)

# Calculate 95% and 90% confidence intervals
ate_m2_95 <- tidy(model2, conf.int = TRUE) %>% relabel_terms(term_labels)
ate_m2_90 <- tidy(model2, conf.int = TRUE, conf.level = 0.90) %>% 
  relabel_terms(term_labels) %>%
  select(term, conf.low, conf.high) %>%
  rename(conf.low_90 = conf.low, conf.high_90 = conf.high)

# Merge 95% and 90% CIs and classify variables
ate_m2 <- ate_m2_95 %>%
  left_join(ate_m2_90, by = "term") %>%
  mutate(
    treatment = case_when(
      str_detect(term, "Liberal Trump") ~ "Liberal Trump",
      str_detect(term, "Conservative Trump") ~ "Conservative Trump",
      str_detect(term, "Liberal Close Friend") ~ "Liberal Close Friend",
      str_detect(term, "Conservative Close Friend") ~ "Conservative Close Friend"
    ),
    party_id = case_when(
      str_detect(term, "^Democrat:") ~ "Democrat",
      str_detect(term, "^Independent/Other:") ~ "Independent/Other",
      str_detect(term, "^Republican:") ~ "Republican"
    )
  ) %>%
  filter(!is.na(treatment)) %>%
  mutate(
    treatment = factor(treatment, levels = rev(c(
      "Liberal Trump",
      "Conservative Trump",
      "Liberal Close Friend",
      "Conservative Close Friend"
    ))),
    party_id = factor(party_id, levels = rev(c("Republican", "Independent/Other", "Democrat")))
  )

# Plot
ate_plot2 <- ggplot(ate_m2, aes(y = treatment, x = estimate, shape = party_id)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, group = party_id), 
                 height = 0, size = 0.5, position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low_90, xmax = conf.high_90, group = party_id), 
                 height = 0, size = 1.2, alpha = 0.7, position = position_dodge(width = 0.5)) +
  geom_point(size = 3, position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(
    limits = c(-0.6, 0.6), 
    breaks = seq(-0.6, 0.6, by = 0.2), 
    labels = scales::number_format(accuracy = 0.1)
  ) +
  scale_shape_manual(values = c("Republican" = 15, "Independent/Other" = 17, "Democrat" = 16)) +
  labs(x = "Interaction Coefficient", y = "Treatment Conditions", shape = "Party Identification") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 11),
    panel.grid = element_blank(),
    axis.title.x = element_text(size = 14, margin = margin(t = 20, b = 10)),
    axis.text = element_text(size = 12)
  ) +
  guides(shape = guide_legend(reverse = TRUE))

ate_plot2


ggsave("outputs/figures/inter_plot_partyid_2.png", plot = ate_plot2, 
       dpi = 600, width =7.6, height = 7)

### Figure 2b - Interaction with Trump approval
model2_ta <- lm(policy_opinion ~ male + trump_approve + 
                treatment_group * trump_approve, data = data_long,
                weights = teamweight)

# Labels for trump_approve interaction terms
term_labels_ta <- c(
  "treatment_groupTL" = "Disapprove:Liberal Trump",
  "treatment_groupTC" = "Disapprove:Conservative Trump",
  "treatment_groupCFL" = "Disapprove:Liberal Close Friend",
  "treatment_groupCFC" = "Disapprove:Conservative Close Friend",
  "trump_approveNeither:treatment_groupTL" = "Neither:Liberal Trump",
  "trump_approveApprove:treatment_groupTL" = "Approve:Liberal Trump",
  "trump_approveNeither:treatment_groupTC" = "Neither:Conservative Trump",
  "trump_approveApprove:treatment_groupTC" = "Approve:Conservative Trump",
  "trump_approveNeither:treatment_groupCFL" = "Neither:Liberal Close Friend",
  "trump_approveApprove:treatment_groupCFL" = "Approve:Liberal Close Friend",
  "trump_approveNeither:treatment_groupCFC" = "Neither:Conservative Close Friend",
  "trump_approveApprove:treatment_groupCFC" = "Approve:Conservative Close Friend")

# Calculate 95% CI
tidy_m2_ta <- tidy(model2_ta, conf.int = TRUE)

# Calculate 90% CI and apply the same labels transformation
tidy_m2_ta_90 <- tidy(model2_ta, conf.int = TRUE, conf.level = 0.90) %>%
  mutate(term = if_else(term %in% names(term_labels_ta), term_labels_ta[term], term)) %>%
  filter(term != "(Intercept)") %>%
  select(term, conf.low, conf.high) %>%
  rename(conf.low_90 = conf.low, conf.high_90 = conf.high)

# Process both CIs
ate_m2_ta <- tidy_m2_ta %>%
  mutate(term = if_else(term %in% names(term_labels_ta), term_labels_ta[term], term)) %>%
  filter(term != "(Intercept)") %>%
  # Join with transformed 90% CI
  left_join(tidy_m2_ta_90, by = "term") %>%
  mutate(
    treatment = case_when(
      str_detect(term, "Liberal Trump") ~ "Liberal Trump",
      str_detect(term, "Conservative Trump") ~ "Conservative Trump",
      str_detect(term, "Liberal Close Friend") ~ "Liberal Close Friend",
      str_detect(term, "Conservative Close Friend") ~ "Conservative Close Friend",
      TRUE ~ as.character(term)
    ),
    trump_approve = case_when(
      str_detect(term, "^Disapprove:") ~ "Disapprove",
      str_detect(term, "^Neither:") ~ "Neither",
      str_detect(term, "^Approve:") ~ "Approve"
    )
  ) %>%
  filter(!is.na(trump_approve), !is.na(treatment)) %>%
  mutate(
    treatment = factor(treatment, levels = rev(c(
      "Liberal Trump",
      "Conservative Trump",
      "Liberal Close Friend",
      "Conservative Close Friend"
    ))),
    trump_approve = factor(trump_approve, levels = rev(c("Approve", "Neither", "Disapprove")))
  )

# Plot 
ate_plot2_ta <- ggplot(ate_m2_ta, aes(y = treatment, x = estimate, shape = trump_approve)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, group = trump_approve), 
                 height = 0, size = 0.5, color = "black",
                 position = position_dodge(width = 0.5)) +
  geom_errorbarh(aes(xmin = conf.low_90, xmax = conf.high_90, group = trump_approve), 
                 height = 0, size = 1.2, alpha = 0.7, color = "black",
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 3, color = "black", 
             position = position_dodge(width = 0.5)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_x_continuous(limits = c(-0.80, 0.80), breaks = seq(-0.80, 0.80, by = 0.20)) +
  labs(x = "Interaction Coefficient", y = "Treatment Conditions", shape = "Trump Approval") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.title.x = element_text(size = 14, margin = margin(t = 20, b = 10)),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) +
  scale_shape_manual(values = c("Disapprove" = 16,
                                "Neither" = 17,
                                "Approve" = 15)) +
  guides(shape = guide_legend(reverse = TRUE))

ate_plot2_ta

ggsave("outputs/figures/inter_plot_trump_app_2.png", plot = ate_plot2_ta, dpi = 600, width = 7, height = 6)

### Figure 3 - Interaction with political knowledge

model3b <- lm(policy_opinion ~ male + pid3 + knowledge + 
              treatment_group*knowledge, data = data_long,
              weights = teamweight)

m3_TL <- ggpredict(model3b, terms = c("knowledge", "treatment_group[TL]"))
m3_TC <- ggpredict(model3b, terms = c("knowledge", "treatment_group[TC]"))
m3_CFL <- ggpredict(model3b, terms = c("knowledge", "treatment_group[CFL]"))
m3_CFC <- ggpredict(model3b, terms = c("knowledge", "treatment_group[CFC]"))

model3_table <- rbind(transform(m3_TL, treatment = "TL"),
                      transform(m3_TC, treatment = "TC"),
                      transform(m3_CFL, treatment = "CFL"),
                      transform(m3_CFC, treatment = "CFC"))

model3_table$treatment <- factor(model3_table$treatment,
                                 levels = c("TL", "TC", "CFL", "CFC"),
                                 labels = c("Liberal Trump", "Conservative Trump",
                                            "Liberal Close Friend", 
                                            "Conservative Close Friend"))

model3_plot <- ggplot(model3_table, aes(x = x, y = predicted, group = treatment)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, group = treatment), 
              fill = "grey80", alpha = 0.5) +  # Confidence intervals
  geom_line(color = "black", linewidth = 1) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~treatment, scales = "free", ncol = 2) +
  labs(y = "Predicted Values", x = "Level of Political Knowledge") +
  theme_minimal(base_size = 14) +  
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, color = "black", linewidth = 0.5),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),   
        axis.text.y = element_text(size = 12)) +  
  coord_cartesian(ylim = c(-0.2, 0.65)) +
  scale_x_continuous(breaks = 0:6)


model3_plot

ggsave("outputs/figures/pol_knowledge.png", plot = model3_plot, dpi = 600, width = 8, height = 6)

### Figure 4 - Interaction with social conformism

model_4 <- lm(policy_opinion ~ male + pid3 + treatment_group*SCI, 
              data = data_long, weights = teamweight)

m4_TL <- ggpredict(model_4, terms = c("SCI", "treatment_group[TL]"))
m4_TC <- ggpredict(model_4, terms = c("SCI", "treatment_group[TC]"))
m4_CFL <- ggpredict(model_4, terms = c("SCI", "treatment_group[CFL]"))
m4_CFC <- ggpredict(model_4, terms = c("SCI", "treatment_group[CFC]"))

model4_table <- rbind(transform(m4_TL, treatment = "TL"),
                      transform(m4_TC, treatment = "TC"),
                      transform(m4_CFL, treatment = "CFL"),
                      transform(m4_CFC, treatment = "CFC"))

model4_table$treatment <- factor(model4_table$treatment, 
                                 levels = c("TL", "TC", "CFL", "CFC"), 
                                 labels = c("Liberal Trump", "Conservative Trump",
                                            "Liberal Close Friend", 
                                            "Conservative Close Friend"))

# Subset the data for Trump 
m4_trump_effects <- subset(model4_table, treatment %in% c("Liberal Trump", "Conservative Trump"))

# Subset the data for Close Friend 
m4_cf_effects <- subset(model4_table, treatment %in% c("Liberal Close Friend", "Conservative Close Friend"))

# Plot for Trump treatments
m4_plot_trump <- ggplot(m4_trump_effects, aes(x = x, y = predicted, group = treatment)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = treatment), fill = "grey80", alpha = 0.5) +  
  geom_line(color = "black", size = 1) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~treatment, scales = "free", ncol = 2) +
  labs(y = "Predicted Values", x = "Social Conformism Index (SCI)") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, color = "black", linewidth = 0.5),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),   
        axis.text.y = element_text(size = 12)) +  
  scale_x_continuous(breaks = seq(1, 3, by = 0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.8))


m4_plot_trump

# Plot for Close Friend treatments
m4_plot_cf <- ggplot(m4_cf_effects, aes(x = x, y = predicted, group = treatment)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = treatment), fill = "grey80", alpha = 0.5) +  
  geom_line(aes(color = treatment), linewidth = 1, color = "black") +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  facet_wrap(~treatment, scales = "free", ncol = 2) +
  labs(y = "Predicted Values", x = "Social Conformism Index (SCI)") +
  theme_minimal(base_size = 14) +  
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    strip.text.x = element_text(size = 14),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(linetype = "solid", 
                                fill = NA, color = "black", linewidth = 0.5),
    axis.title.x = element_text(size = 14),  
    axis.title.y = element_text(size = 14),  
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12)) +  
  scale_x_continuous(breaks = seq(1, 3, by = 0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.6))

m4_plot_cf

m4_plot_final <- grid.arrange(m4_plot_trump, m4_plot_cf, nrow = 2) 

ggsave("outputs/figures/social_conf.png", plot = m4_plot_final, dpi = 600, width = 7, height = 6)

# 3) Supplementary Information Replication

### Section A - Power Analysis

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

# Outputs

# Convert the results to data frames for xtable
df_min_effect_size <- as.data.frame(t(as.matrix(unlist(min_effect_size))))
df_min_sample_size_group <- as.data.frame(t(as.matrix(unlist(min_sample_size_group))))

# Create LaTeX code for the tables
xtable(df_min_effect_size, caption = "Minimum Detectable Effect Size")
xtable(df_min_sample_size_group, caption = "Required Sample Size Per Group")

### Section B - Data and Variables

### Table B1.1 - Mean Demographic Values by Treatment Group

data_balance <- data_long[data_long$policy_issue == "1",]
table(data_balance$treatment_group)

by(data_balance$age, data_balance$treatment_group, mean)
summary(model1 <- lm(age ~ libtrump + contrump + libfriend + confriend, data = data_balance))

by(data_balance$race_white, data_balance$treatment_group, mean)
summary(model1 <- lm(race_white ~  libtrump + contrump + libfriend + confriend, data = data_balance))

by(data_balance$male, data_balance$treatment_group, mean)
summary(model1 <- lm(male ~  libtrump + contrump + libfriend + confriend, data = data_balance)) # significant

by(data_balance$knowledge, data_balance$treatment_group, mean)
summary(model1 <- lm(knowledge ~  libtrump + contrump + libfriend + confriend, data = data_balance))

data_balance$trump_approve_numeric <- as.numeric(data_balance$trump_approve) # to numeric
by(data_balance$trump_approve_numeric, data_balance$treatment_group, mean, na.rm = T)
summary(model1 <- lm(trump_approve_numeric ~ libtrump + contrump + libfriend + confriend, data = data_balance))

by(data_balance$ideo5, data_balance$treatment_group, function(x) mean(x, na.rm = T))
summary(model1 <- lm(ideo5 ~  libtrump + contrump + libfriend + confriend, data = data_balance))

by(data_balance$pid7, data_balance$treatment_group, function(x) mean(x, na.rm = T))
summary(model1 <- lm(pid7 ~  libtrump + contrump + libfriend + confriend, data = data_balance))

by(data_balance$faminc_new, data_balance$treatment_group, function(x) mean(x, na.rm = T))
summary(model1 <- lm(faminc_new ~  libtrump + contrump + libfriend + confriend, data = data_balance))

by(data_balance$educ, data_balance$treatment_group, function(x) mean(x, na.rm = T))
summary(model1 <- lm(educ ~  libtrump + contrump + libfriend + confriend, data = data_balance))

by(data_balance$newsint, data_balance$treatment_group, function(x) mean(x, na.rm = T))
summary(model1 <- lm(newsint ~  libtrump + contrump + libfriend + confriend, data = data_balance))

by(data_balance$republican, data_balance$treatment_group, function(x) mean(x, na.rm = T))
summary(model1 <- lm(republican ~  libtrump + contrump + libfriend + confriend, data = data_balance))

by(data_balance$democrat, data_balance$treatment_group, function(x) mean(x, na.rm = T))
summary(model1 <- lm(democrat ~  libtrump + contrump + libfriend + confriend, data = data_balance))

by(data_balance$independent, data_balance$treatment_group, function(x) mean(x, na.rm = T))
summary(model1 <- lm(independent ~ libtrump + contrump + libfriend + confriend, 
                     data = data_balance)) #party id somewhat significant, we might want to control

descriptive_stats <- data_balance %>%
  group_by(treatment_group) %>%
  summarise(across(c(age, race_white, male, knowledge, trump_approve_numeric, ideo5, pid7, 
                     faminc_new, educ, newsint, republican, democrat, independent),
                   list(mean = ~mean(., na.rm = TRUE)), .names = "{.col}_{.fn}"))

descriptive_stats_long <- descriptive_stats %>%
  pivot_longer(cols = -treatment_group,
               names_to = "metric",
               values_to = "value") %>%
  pivot_wider(names_from = treatment_group,
              values_from = value) %>%
  mutate(metric = c("Age", "White", "Male", "Knowledge", 
                    "Trump Approval", "5-point Ideology", "7-point Ideology", 
                    "Income", "Education", "Political Interest", 
                    "Republican", "Democrat", "Independent")) %>%
  rename("Variable" = metric,
         "Control" = `control`,
         "Liberal Trump" = `TL`,
         "Conservative Trump" = `TC`,
         "Liberal Close Friend" = `CFL`,
         "Conservative Close Friend" = `CFC`) %>%
  mutate(across(-Variable, ~round(., 2)))


latex_table <- kable(descriptive_stats_long, format = "latex", booktabs = TRUE) 
descriptive_stats_long

### Figure B.2.1 - Distribution political knowledge

pol_knowledge_plot <- data_long[data_long$policy_issue == 1, ] %>%
  count(knowledge) %>%
  ggplot(aes(x = knowledge, y = n)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of Correct Answers",
       y = "Number of Observations") +
  scale_x_continuous(breaks = seq(min(data_long$knowledge), 
                                  max(data_long$knowledge), by = 1)) +
  scale_y_continuous(limits = c(0, 250)) +  
  theme_minimal()

pol_knowledge_plot

ggsave("outputs/figures/pol_knowledge_hist.png", plot = pol_knowledge_plot, dpi = 300, width = 8, height = 6)

### Table B2.1 - Test for Cronbach's alpha

items_sci_full <- data_long %>% 
  dplyr::select(BGU_conf1_rec, BGU_conf2_rec, BGU_conf3_rec, BGU_conf4_rec, 
         BGU_conf5_rec, BGU_conf6_rec)

ca_sci <- alpha(items_sci_full, check.keys=TRUE)
print(ca_sci)

item_stats <- as.data.frame(ca_sci$item.stats)
latex_table_items <- xtable(item_stats)
print(latex_table_items, type = 'latex') 

### Section C

### Table C1.1 Average Treatment Effect of Policy Cue

msummary(model1,
         title = "Average Treatment Effect of Policy Cue",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         estimate = "{estimate}{stars} ({std.error})",
         statistic = NULL,
         output = 'latex')

### Table C1.2 - Treatment effect per policy issues

models_pol_issues <- list(
  "Wages" = lm(policy_opinion ~ male + pid3 + treatment_group, 
               data = data_long[data_long$policy_issue == 1, ],
               weights = data_long[data_long$policy_issue == 1, ]$teamweight),
  "Taxes" = lm(policy_opinion ~ male + pid3 + treatment_group, 
               data = data_long[data_long$policy_issue == 2, ],
               weights = data_long[data_long$policy_issue == 2, ]$teamweight),
  "Abortion" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                  data = data_long[data_long$policy_issue == 3, ],
                  weights = data_long[data_long$policy_issue == 3, ]$teamweight),
  "Immigration" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                     data = data_long[data_long$policy_issue == 4, ],
                     weights = data_long[data_long$policy_issue == 4, ]$teamweight),
  "Guns" = lm(policy_opinion ~ male + pid3 + treatment_group, 
              data = data_long[data_long$policy_issue == 5, ],
              weights = data_long[data_long$policy_issue == 5, ]$teamweight),
  "Health Care" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                     data = data_long[data_long$policy_issue == 6, ],
                     weights = data_long[data_long$policy_issue == 6, ]$teamweight),
  "Background Checks" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                           data = data_long[data_long$policy_issue == 7, ],
                           weights = data_long[data_long$policy_issue == 7, ]$teamweight),
  "Climate Change" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                        data = data_long[data_long$policy_issue == 8, ],
                        weights = data_long[data_long$policy_issue == 8, ]$teamweight),
  "Planned Parenthood" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                            data = data_long[data_long$policy_issue == 9, ],
                            weights = data_long[data_long$policy_issue == 9, ]$teamweight))

msummary(models_pol_issues,
         stars = c('*' = .1, '**' = .05, '***' = .01),
         output = 'markdown')

### Table C3.1 With Party ID

msummary(model2,
         title = "Interaction with Party Identification",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         estimate = "{estimate}{stars} ({std.error})",
         statistic = NULL,
         output = 'markdown')

### Table C3.2 With Trump Approval

msummary(model2_ta,
         title = "Interaction with Trump Approval",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         estimate = "{estimate}{stars} ({std.error})",
         statistic = NULL,
         output = 'markdown')

### Table C3.3 With Political Knowledge

msummary(model3b,
         title = "Interaction with Political Knowledge",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         estimate = "{estimate}{stars} ({std.error})",
         statistic = NULL,
         output = 'markdown')

### Table C3.4 With Social Conformism

msummary(model_4,
         title = "Interaction with Social Conformism Index",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         estimate = "{estimate}{stars} ({std.error})",
         statistic = NULL,
         output = 'markdown')


### Section D Robustness Checks

### Figure D.1.1 - Distribution realism

discussion_counts <- data_long %>%
  pivot_longer(
    cols = starts_with("inverted_BGU_discussion"),
    names_to = "policy_variable", # This will hold the names of your discussion variables
    values_to = "discussion_level") %>%
  filter(!is.na(discussion_level)) %>%
  # We need a new variable to distinguish between different policy issues if it's not already present in the data.
  mutate(policy_issue = str_extract(policy_variable, "\\d+")) %>%
  group_by(policy_issue, discussion_level) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(policy_issue) %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup()

# Rename the levels with the actual policy names
policy_issue_names <- c("Minimum Wage", "Taxes", "Abortion", "Immigration", 
                        "Guns", "Health Care", "Background Checks", "Climate Change", 
                        "Planned Parenthood")

# reorder the levels
levels(discussion_counts$policy_issue) <- rev(policy_issue_names)

discussion_counts$policy_issue <- factor(discussion_counts$policy_issue, 
                                         levels = rev(unique(discussion_counts$policy_issue)))

discussion_counts$discussion_level <- factor(discussion_counts$discussion_level, levels = c(6, 5, 4, 3, 2, 1))

realism_plot <- ggplot(discussion_counts, aes(x = policy_issue, y = percentage, fill = discussion_level)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_grey(start = 0.2, end = 0.8) +  # Light to dark for levels 1 to 6
  labs(x = "Policy Issue", y = "Percentage", fill = "Discussion Frequency") +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme(legend.position = "none") +
  coord_flip() +
  annotate("text", x = "Minimum Wage", y = 0.95, label = "> Once a\nWeek", size = 3, hjust = 0.5, vjust = 0.5, color = "white") +
  annotate("text", x = "Minimum Wage", y = 0.865, label = "Once a\nWeek", size = 3, hjust = 0.5, vjust = 0.5, color = "white") +
  annotate("text", x = "Minimum Wage", y = 0.75, label = "Once/Twice\na Month", size = 3, hjust = 0.5, vjust = 0.5, color = "black") +
  annotate("text", x = "Minimum Wage", y = 0.54, label = "Few Times\na Year", size = 3, hjust = 0.5, vjust = 0.5, color = "black") +
  annotate("text", x = "Minimum Wage", y = 0.32, label = "Seldom", size = 3, hjust = 0.5, vjust = 0.5, color = "black") +
  annotate("text", x = "Minimum Wage", y = 0.11, label = "Never", size = 3, hjust = 0.5, vjust = 0.5, color = "black")

realism_plot


ggsave("outputs/figures/realism_hist.png", plot = realism_plot, dpi = 600, width = 8, height = 6)

### Table D.1.1. - Realism check

# - To check if the influence (or lack of it) of the close friend treatment
# - relates to how much the respondent talks about given political issues

#- New models for political realism
#- For this, instead of dividing the sample into very frequent and infrequent discussants, we'll use softer approach
#- and we'll create a theoretical midpoint of 3.5. Respondents over that will be classified as frequent discussants

data_long <- data_long %>%
  mutate(frequent_discussant = ifelse(pol_dis_friend >= 3.0, 1, 0))

models_realism <- list(
  "Wages" = lm(policy_opinion ~ male + pid3 + treatment_group*frequent_discussant, 
               data = data_long[data_long$policy_issue == 1, ],
               weights = data_long[data_long$policy_issue == 1, ]$teamweight),
  
  "Taxes" = lm(policy_opinion ~ male + pid3 + treatment_group*frequent_discussant, 
               data = data_long[data_long$policy_issue == 2, ],
               weights = data_long[data_long$policy_issue == 2, ]$teamweight),
  
  "Abortion" = lm(policy_opinion ~ male + pid3 + treatment_group*frequent_discussant, 
                  data = data_long[data_long$policy_issue == 3, ],
                  weights = data_long[data_long$policy_issue == 3, ]$teamweight),
  
  "Immigration" = lm(policy_opinion ~ male + pid3 + treatment_group*frequent_discussant, 
                     data = data_long[data_long$policy_issue == 4, ],
                     weights = data_long[data_long$policy_issue == 4, ]$teamweight),
  
  "Guns" = lm(policy_opinion ~ male + pid3 + treatment_group*frequent_discussant, 
              data = data_long[data_long$policy_issue == 5, ],
              weights = data_long[data_long$policy_issue == 5, ]$teamweight),
  
  "Health Care" = lm(policy_opinion ~ male + pid3 + treatment_group*frequent_discussant, 
                     data = data_long[data_long$policy_issue == 6, ],
                     weights = data_long[data_long$policy_issue == 6, ]$teamweight),
  
  "Background Checks" = lm(policy_opinion ~ male + pid3 + treatment_group*frequent_discussant, 
                           data = data_long[data_long$policy_issue == 7, ],
                           weights = data_long[data_long$policy_issue == 7, ]$teamweight),
  
  "Climate Change" = lm(policy_opinion ~ male + pid3 + treatment_group*frequent_discussant, 
                        data = data_long[data_long$policy_issue == 8, ],
                        weights = data_long[data_long$policy_issue == 8, ]$teamweight),
  
  "Planned Parenthood" = lm(policy_opinion ~ male + pid3 + treatment_group*frequent_discussant, 
                            data = data_long[data_long$policy_issue == 9, ],
                            weights = data_long[data_long$policy_issue == 9, ]$teamweight))


# Create table with results
msummary(models_realism,
         stars = c('*' = .1, '**' = .05, '***' = .01),
         title = "ATE Policy Positions - Interaction with Discussion Frequency",
         output = 'markdown')


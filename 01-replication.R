#------------------------------Replication Script------------------------------#
#-------------------------------------------------- Created: December 27, 2023-#
#-R Version: 4.3.1 ----------------------------------- Revised: March 26, 2024-#

# 1) Load packages

pacman::p_load(haven, dplyr, tidyverse, magrittr, forcats, ggplot2, psych, xtable,
               broom, ggeffects, gridExtra, knitr, modelsummary, purrr, kableExtra)

options(scipen = 999)

# 1.1) Data

# - This script requires the data produced by `00-data-prep`
# - Alternative, one can load the data available in `data/derived-data/`

readRDS("data/derived-data/data_long.rds")


# 2) Paper replication

### Figure 1 - Average treatment effect of policy cues 
# - Replication of Figure 1 ATE Across Issues on Barber & Pope (2019) 
# - but without separating by party

model1 <- lm(policy_opinion ~ male + pid3 + treatment_group, data = data_long)

ate_m1 <- tidy(model1, conf.int = TRUE) %>%
  filter(term %in% c("treatment_groupTL", "treatment_groupTC", 
                     "treatment_groupCFL", "treatment_groupCFC")) %>%
  mutate(term = case_when(
    term == "treatment_groupTL" ~ "Liberal Trump",
    term == "treatment_groupTC" ~ "Conservative Trump",
    term == "treatment_groupCFL" ~ "Liberal Close Friend",
    term == "treatment_groupCFC" ~ "Conservative Close Friend",
    TRUE ~ term))

ate_m1$term <- factor(ate_m1$term, levels = c("Liberal Trump", 
                                              "Conservative Trump",
                                              "Liberal Close Friend",
                                              "Conservative Close Friend"))

# Plot
ate_plot <- ggplot(ate_m1, aes(x = term, y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.7), width = 0.05) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  xlab("Treatment Conditions") +
  ylab("Average Treatment Effect") +
  scale_x_discrete(labels = c("Liberal\nTrump", 
                              "Conservative\nTrump", 
                              "Liberal\nClose Friend", 
                              "Conservative\nClose Friend")) +
  scale_y_continuous(limits = c(-0.25, 0.25)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(margin = margin(t = 20, b = 10)))

ate_plot

ggsave("outputs/figures/ate_plot.png", plot = ate_plot, dpi = 300, width = 8, height = 6)


### Figure 2 - Interaction with Party ID 

model2 <- lm(policy_opinion ~ male + party_id + treatment_group*party_id, data = data_long)

# CIs
tidy_m2 <- tidy(model2, conf.int = TRUE) 

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
  "party_idRepublican:treatment_groupCFC" = "Republican:Conservative Close Friend")

# Relabel terms and combine main and interaction effects
ate_m2 <- tidy_m2 %>%
  mutate(term = if_else(term %in% names(term_labels), term_labels[term], term)) %>%
  filter(term != "(Intercept)")  # Exclude intercept from this dataframe

# Extract intercept (effect for Democrats)
intercept_m2 <- filter(tidy_m2, term == "(Intercept)")

# Add the intercept as the effect for Democrats
ate_m2 %<>%
  add_row(term = "Democrat:Baseline",
          estimate = intercept_m2$estimate,
          std.error = intercept_m2$std.error,
          conf.low = intercept_m2$estimate - 1.96 * intercept_m2$std.error,
          conf.high = intercept_m2$estimate + 1.96 * intercept_m2$std.error)

# Add column for treatment and party id to use them in the plot
ate_m2 %<>%
  mutate(
    treatment = case_when(
      str_detect(term, "Liberal Trump") ~ "Liberal Trump",
      str_detect(term, "Conservative Trump") ~ "Conservative Trump",
      str_detect(term, "Liberal Close Friend") ~ "Liberal Close Friend",
      str_detect(term, "Conservative Close Friend") ~ "Conservative Close Friend"),
    party_id = case_when(
      str_detect(term, "^Democrat:") ~ "Democrat",
      str_detect(term, "^Independent/Other:") ~ "Independent/Other",
      str_detect(term, "^Republican:") ~ "Republican")) %>%
  filter(!is.na(treatment))

# Plot
ate_plot2 <- ggplot(ate_m2, aes(x = term, y = estimate)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.7), width = 0.05) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  xlab("Treatment Conditions") +
  ylab("Average Treatment Effect") +
  scale_x_discrete(labels = c("Liberal\nTrump",
                              "Conservative\nTrump",
                              "Liberal\nClose Friend",
                              "Conservative\nClose Friend")) +
  scale_y_continuous(limits = c(-0.25, 0.25)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

# Relevel
ate_m2$treatment <- factor(ate_m2$treatment, levels = c(
  "Liberal Trump", 
  "Conservative Trump", 
  "Liberal Close Friend", 
  "Conservative Close Friend"))

ate_plot2 <- ggplot(ate_m2, aes(x = treatment, y = estimate, color = party_id)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.7), width = 0.15) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Treatments", y = "Interaction Coefficient", color = "Party Identification") +
  scale_y_continuous(limits = c(-0.5, 0.5)) +
  theme_minimal(base_size = 14) +  # 
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),  
        legend.text = element_text(size = 12),   
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_text(size = 14, margin = margin(t = 20, b = 10)),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),  
        axis.text.y = element_text(size = 12)) +  
  scale_color_manual(values = c("Democrat" = "black", 
                                "Independent/Other" = "#787878", 
                                "Republican" = "gray")) +
  scale_x_discrete(labels = c("Liberal\nTrump", 
                              "Conservative\nTrump", 
                              "Liberal\nClose Friend", 
                              "Conservative\nClose Friend"))

ate_plot2

ggsave("outputs/figures/inter_plot_partyid.png", plot = ate_plot2, dpi = 600, width = 7, height = 6)


### Figure 2b - Interaction with Trump approval
#  Levels for trump approval
data_long$trump_approve <- factor(data_long$trump_approve, 
                                  levels = c(1, 2, 3), labels = c("Disapprove", "Neither", "Approve"))

# Factor interaction
model2_ta <- lm(policy_opinion ~ male + trump_approve + treatment_group * trump_approve, data = data_long)

tidy_m2_ta <- tidy(model2_ta, conf.int = TRUE)

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


# Relabel terms for trump_approve
ate_m2_ta <- tidy_m2_ta %>%
  mutate(term = if_else(term %in% names(term_labels_ta), term_labels_ta[term], term)) %>%
  filter(term != "(Intercept)")  # Exclude intercept 

# Extract and handle intercept (effect for Disapprove as baseline)
intercept_m2_ta <- filter(tidy_m2_ta, term == "(Intercept)")

ate_m2_ta %<>%
  add_row(term = "Disapprove:Baseline",
          estimate = intercept_m2_ta$estimate,
          std.error = intercept_m2_ta$std.error,
          conf.low = intercept_m2_ta$estimate - 1.96 * intercept_m2_ta$std.error,
          conf.high = intercept_m2_ta$estimate + 1.96 * intercept_m2_ta$std.error)

# Add column for treatment and trump_approve to use in the plot
ate_m2_ta %<>%
  mutate(
    treatment = case_when(
      str_detect(term, "Liberal Trump") ~ "Liberal Trump",
      str_detect(term, "Conservative Trump") ~ "Conservative Trump",
      str_detect(term, "Liberal Close Friend") ~ "Liberal Close Friend",
      str_detect(term, "Conservative Close Friend") ~ "Conservative Close Friend",
      TRUE ~ as.character(term)  # To handle the Disapprove:Baseline
    ),
    trump_approve = case_when(
      str_detect(term, "^Disapprove:") ~ "Disapprove",
      str_detect(term, "^Neither:") ~ "Neither",
      str_detect(term, "^Approve:") ~ "Approve")) %>%
  filter(!is.na(trump_approve)) 


ate_m2_ta$treatment <- factor(ate_m2_ta$treatment, levels = c(
  "Liberal Trump", 
  "Conservative Trump", 
  "Liberal Close Friend", 
  "Conservative Close Friend"))

ate_m2_ta %<>%
  filter(!is.na(treatment))

# Reorder levels of trump_approve
ate_m2_ta$trump_approve <- factor(ate_m2_ta$trump_approve, levels = c("Disapprove", "Neither", "Approve"))


# Plot
ate_plot2_ta <- ggplot(ate_m2_ta, aes(x = treatment, y = estimate, color = trump_approve)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.7), width = 0.15) +
  geom_point(position = position_dodge(width = 0.7), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Treatments", y = "Interaction Coefficient", color = "Trump Approval") +
  scale_color_manual(values = c("Disapprove" = "black", 
                                "Neither" = "#787878", 
                                "Approve" = "gray")) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 14, margin = margin(t = 20, b = 10)),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(size = 12),  
    legend.text = element_text(size = 12),   
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) +
  scale_x_discrete(labels = c("Liberal\nTrump", 
                              "Conservative\nTrump", 
                              "Liberal\nClose Friend", 
                              "Conservative\nClose Friend")) +
  scale_y_continuous(limits = c(-0.6, 0.6))

ate_plot2_ta

ggsave("outputs/figures/inter_plot_trump_app.png", plot = ate_plot2_ta, dpi = 600, width = 7, height = 6)

### Figure 3 - Interaction with political knowledge

model3 <- lm(policy_opinion ~ male + pid3 + factor(knowledge) + 
               treatment_group*factor(knowledge), data = data_long)

model3b <- lm(policy_opinion ~ male + pid3 + knowledge + 
               treatment_group*knowledge, data = data_long)

m3_TL <- ggpredict(model3, terms = c("knowledge", "treatment_group[TL]"))
m3_TC <- ggpredict(model3, terms = c("knowledge", "treatment_group[TC]"))
m3_CFL <- ggpredict(model3, terms = c("knowledge", "treatment_group[CFL]"))
m3_CFC <- ggpredict(model3, terms = c("knowledge", "treatment_group[CFC]"))

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

model_4 <- lm(policy_opinion ~ male + pid3 + treatment_group*SCI, data = data_long)

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
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = treatment), fill = "grey80", alpha = 0.5) +  # Confidence intervals
  geom_line(color = "black", size = 1) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~treatment, scales = "free", ncol = 2) +
  labs(y = "Predicted Values", x = "Social Conformism Index (SCI)") +
  scale_x_continuous(breaks = 0:6) + 
  theme_minimal(base_size = 14) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 14), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, color = "black", size = 0.5),
        axis.title.x = element_text(size = 14),  
        axis.title.y = element_text(size = 14),  
        axis.text.x = element_text(size = 12),   
        axis.text.y = element_text(size = 12)) +  
  scale_x_continuous(breaks = seq(1, 3, by = 0.5)) +
  coord_cartesian(ylim = c(-0.1, 0.6))


m4_plot_trump

# Plot for Close Friend treatments
m4_plot_cf <- ggplot(m4_cf_effects, aes(x = x, y = predicted, group = treatment)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = treatment), fill = "grey80", alpha = 0.5) +  
  geom_line(aes(color = treatment), size = 1, color = "black") +  
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
                                    fill = NA, color = "black", size = 0.5),
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

by(data_balance$trump_approve, data_balance$treatment_group, mean, na.rm = T)
summary(model1 <- lm(trump_approve ~ libtrump + contrump + libfriend + confriend, data = data_balance))

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
  summarise(across(c(age, race_white, male, knowledge, trump_approve, ideo5, pid7, 
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

### Table B2.1 - Test for Cronbach's alpha

items_sci_full <- data_long %>% 
  dplyr::select(BGU_conf1_rec, BGU_conf2_rec, BGU_conf3_rec, BGU_conf4_rec, 
         BGU_conf5_rec, BGU_conf6_rec)

ca_sci <- alpha(items_sci_full, check.keys=TRUE)
print(ca_sci)

item_stats <- as.data.frame(ca_sci$item.stats)
latex_table_items <- xtable(item_stats)
print(latex_table_items, type = 'latex') 

### Table C1.1 Average Treatment Effect of Policy Cue

msummary(model1,
         title = "Average Treatment Effect of Policy Cue",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         estimate = "{estimate}{stars} ({std.error})",
         statistic = NULL,
         output = 'markdown')

### Table C1.2 - Treatment effect per policy issues

models_pol_issues <- list(
  "Wages" = lm(policy_opinion ~ male + pid3 + treatment_group, 
               data = data_long[data_long$policy_issue == 1, ]),
  "Taxes" = lm(policy_opinion ~ male + pid3 + treatment_group, 
               data = data_long[data_long$policy_issue == 2, ]),
  "Abortion" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                  data = data_long[data_long$policy_issue == 3, ]),
  "Immigration" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                     data = data_long[data_long$policy_issue == 4, ]),
  "Guns" = lm(policy_opinion ~ male + pid3 + treatment_group, 
              data = data_long[data_long$policy_issue == 5, ]),
  "Health Care" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                     data = data_long[data_long$policy_issue == 6, ]),
  "Background Checks" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                           data = data_long[data_long$policy_issue == 7, ]),
  "Climate Change" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                        data = data_long[data_long$policy_issue == 8, ]),
  "Planned Parenthood" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                            data = data_long[data_long$policy_issue == 9, ]))

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

### Table C3.2 With Political Knowledge

msummary(model3,
         title = "Interaction with Political Knowledge",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         estimate = "{estimate}{stars} ({std.error})",
         statistic = NULL,
         output = 'markdown')

### Table C3.3 With Social Conformism

msummary(model_4,
         title = "Interaction with Social Conformism Index",
         stars = c('*' = .1, '**' = .05, '***' = .01),
         estimate = "{estimate}{stars} ({std.error})",
         statistic = NULL,
         output = 'markdown')

### Table D1.1 - Bonferroni corrections

# - Extract Model Summaries

model1_tidy <- tidy(model1)
model2_tidy <- tidy(model2)
model3_tidy <- tidy(model3b)
model4_tidy <- tidy(model_4)

# - Combine models and apply bonferroni correction

all_models_tidy <- bind_rows(mutate(model1_tidy, model = "Model 1"),
                             mutate(model2_tidy, model = "Model 2"),
                             mutate(model3_tidy, model = "Model 3"),
                             mutate(model4_tidy, model = "Model 4"))

all_models_tidy <- all_models_tidy %>%
  group_by(term) %>%
  mutate(
    Bonferroni = p.adjust(p.value, method = "bonferroni"),
    p.value = round(p.value, 3),         
    Bonferroni = round(Bonferroni, 3),   
    estimate = round(estimate, 3)) %>%
  ungroup() %>%
  select(model, term, estimate, p.value, Bonferroni)  


kable(all_models_tidy, "latex", booktabs = TRUE, 
      col.names = c("Model", "Term", "Estimate", "P-Value", "Bonferroni")) 

### Table D.2. - Realism check

# - To check if the influence (or lack of it) of the close friend treatment
# - relates to how much the respondent talks about given political issues

models_realism_low <- list(
  "Wages" = lm(policy_opinion ~ male + pid3 + treatment_group, 
               data = data_long[data_long$policy_issue == 1 & data_long$BGU_discussion1 == 1, ]),
  "Taxes" = lm(policy_opinion ~ male + pid3 + treatment_group, 
               data = data_long[data_long$policy_issue == 2 & data_long$BGU_discussion2 == 1, ]),
  "Abortion" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                  data = data_long[data_long$policy_issue == 3 & data_long$BGU_discussion3 == 1, ]),
  "Immigration" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                     data = data_long[data_long$policy_issue == 4 & data_long$BGU_discussion4 == 1, ]),
  "Guns" = lm(policy_opinion ~ male + pid3 + treatment_group, 
              data = data_long[data_long$policy_issue == 5 & data_long$BGU_discussion5 == 1, ]),
  "Health Care" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                     data = data_long[data_long$policy_issue == 6 & data_long$BGU_discussion6 == 1, ]),
  "Background Checks" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                           data = data_long[data_long$policy_issue == 7 & data_long$BGU_discussion7 == 1, ]),
  "Climate Change" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                        data = data_long[data_long$policy_issue == 8 & data_long$BGU_discussion8 == 1, ]),
  "Planned Parenthood" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                            data = data_long[data_long$policy_issue == 9 & data_long$BGU_discussion9 == 1, ]))

msummary(models_realism_low,
         stars = c('*' = .1, '**' = .05, '***' = .01),
         title = "ATE Policy Positions - Does Not Discuss with Friends Subsample",
         output = 'markdown')

models_realism_med_high <- list(
  "Wages" = lm(policy_opinion ~ male + pid3 + treatment_group, 
               data = data_long[data_long$policy_issue == 1 & data_long$BGU_discussion1 >= 2, ]),
  "Taxes" = lm(policy_opinion ~ male + pid3 + treatment_group, 
               data = data_long[data_long$policy_issue == 2 & data_long$BGU_discussion2 >= 2, ]),
  "Abortion" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                  data = data_long[data_long$policy_issue == 3 & data_long$BGU_discussion3 >= 2, ]),
  "Immigration" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                     data = data_long[data_long$policy_issue == 4 & data_long$BGU_discussion4 >= 2, ]),
  "Guns" = lm(policy_opinion ~ male + pid3 + treatment_group, 
              data = data_long[data_long$policy_issue == 5 & data_long$BGU_discussion5 >= 2, ]),
  "Health Care" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                     data = data_long[data_long$policy_issue == 6 & data_long$BGU_discussion6 >= 2, ]),
  "Background Checks" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                           data = data_long[data_long$policy_issue == 7 & data_long$BGU_discussion7 >= 2, ]),
  "Climate Change" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                        data = data_long[data_long$policy_issue == 8 & data_long$BGU_discussion8 >= 2, ]),
  "Planned Parenthood" = lm(policy_opinion ~ male + pid3 + treatment_group, 
                            data = data_long[data_long$policy_issue == 9 & data_long$BGU_discussion9 >= 2, ]))

msummary(models_realism_med_high,
         stars = c('*' = .1, '**' = .05, '***' = .01),
         title = "ATE Policy Positions - Discuss with Friends Subsample",
         output = 'markdown')

### Figure 1 - Distribution political knowledge

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

### Figure 2 - Distribution realism

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

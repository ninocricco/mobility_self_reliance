#********************************************************
# PROJECT: MOBILITY SELF-RELIANCE
# FILE: VIZ- TRANSITION MATRICES X GENDER, COHORT
# DATA: PSID
# AUTHOR: NINO CRICCO
# CONTACT: ncricco@g.harvard.edu
#********************************************************

source("1-processing.R")

data_p1 <- unrestricted_offspring_summarized %>%
  filter(complete.cases(Parent_hdspIncome_quintile_label)) %>%
  group_by(female, cohort, Parent_hdspIncome_quintile_label) %>%
  count(Offspring_hdspIncome_quintile_label) %>%
  mutate(proportion = n/sum(n), 
         class = "Family Income") %>%
  rename(ChildQuintile = Offspring_hdspIncome_quintile_label) %>%
  select(female, cohort, Parent_hdspIncome_quintile_label, ChildQuintile, class, proportion)

data_p2 <- unrestricted_offspring_summarized %>%
  filter(complete.cases(Parent_hdspIncome_quintile_label)) %>%
  group_by(female, cohort, Parent_hdspIncome_quintile_label) %>%
  count(Offspring_ownIncome_quintile_label) %>%
  mutate(proportion = n/sum(n), 
         class = "Own Income") %>%
  rename(ChildQuintile = Offspring_ownIncome_quintile_label) %>%
  select(female, cohort, Parent_hdspIncome_quintile_label,
         ChildQuintile, class, proportion)

data <- bind_rows(data_p1, data_p2) %>%
  mutate(female = ifelse(female == 1, "Women", "Men"),
         interaction_var = interaction(Parent_hdspIncome_quintile_label, class, sep = " "),
         interaction_var = factor(interaction_var,
                                  levels = c("20th Family Income", "20th Own Income",
                                             "40th Family Income", "40th Own Income",
                                             "60th Family Income", "60th Own Income",
                                             "80th Family Income", "80th Own Income",
                                             "80th+ Family Income", "80th+ Own Income")))

c1 <- data %>% filter(cohort == "cohort_1") %>%
  ggplot(aes(x = interaction_var, y = proportion, fill = ChildQuintile)) +
  geom_bar(stat = "identity") +
  scale_fill_grey(start = 0.8, end = 0.2) + # Adjust the colors accordingly
  theme_minimal() +
  labs(x = "Parent's Quintile", y = "Transition Probability", 
       title = "Cohort 1: 1960-1971") +
  scale_x_discrete(breaks = levels(data$interaction_var), 
                   labels = function(x) gsub(" .*", "", x)) +# Clean up the bottom x-axis labels
  facet_wrap(~female) + # Keep the gender split, move labels to top
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(face = "bold"), # Bold class labels on the top
        axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate the x-axis labels for better readability

c2 <- data %>% filter(cohort == "cohort_2") %>%
  ggplot(aes(x = interaction_var, y = proportion, fill = ChildQuintile)) +
  geom_bar(stat = "identity") +
  scale_fill_grey(start = 0.8, end = 0.2) + # Adjust the colors accordingly
  theme_minimal() +
  labs(x = "Parent's Quintile", y = "Transition Probability", 
       title = "Cohort 2: 1972-1982") +
  scale_x_discrete(breaks = levels(data$interaction_var), 
                   labels = function(x) gsub(" .*", "", x)) +# Clean up the bottom x-axis labels
  facet_wrap(~female) + # Keep the gender split, move labels to top
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        strip.text.x = element_text(face = "bold"), # Bold class labels on the top
        axis.text.x = element_text(angle = 45, hjust = 1)) # Rotate the x-axis labels for better readability

p1 <- unrestricted_offspring_summarized %>%
  filter(complete.cases(Parent_hdspIncome_quintile_label), cohort == "cohort_2") %>%
  mutate(Family_Rank = Offspring_hdspIncome_pct_rank - Parent_hdspIncome_pct_rank,
         Own_Rank_Gender = Offspring_GenderRef_ownIncome_pct_rank- Parent_hdspIncome_pct_rank,
         Own_Rank = Offspring_ownIncome_pct_rank - Parent_hdspIncome_pct_rank, 
         female = ifelse(female == 1, "Women", "Men")) %>%
  select(Parent_hdspIncome_pct_rank, Family_Rank, Own_Rank, Own_Rank_Gender,
         female, cohort) %>%
  gather(key, value, -c(Parent_hdspIncome_pct_rank, female, cohort)) %>%
  ggplot(aes(x = Parent_hdspIncome_pct_rank, y = value, color = key)) +
  geom_smooth(se = F) +
  facet_grid(~female) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(y = "Child Rank - Parent Rank", x = "Parents' Income Rank") +
  theme(legend.position = "bottom", legend.title=element_blank()) #+
  #scale_color_manual(values = c("blue4", "dodgerblue1"))

p1 <- unrestricted_offspring_summarized %>%
  filter(complete.cases(Parent_hdspIncome_quintile_label), cohort == "cohort_1") %>%
  mutate(Family_Rank = Offspring_hdspIncome_pct_rank - Parent_hdspIncome_pct_rank,
         Own_Rank_Gender = Offspring_GenderRef_ownIncome_pct_rank- Parent_hdspIncome_pct_rank,
         Own_Rank = Offspring_ownIncome_pct_rank - Parent_hdspIncome_pct_rank, 
         female = ifelse(female == 1, "Women", "Men"),
         cohort = ifelse(cohort == "cohort_1", "1965-1974", "1974-1982")) %>%
  select(Parent_hdspIncome_pct_rank, Family_Rank, Own_Rank, Own_Rank_Gender,
         female, cohort) %>%
  gather(key, value, -c(Parent_hdspIncome_pct_rank, female, cohort)) %>%  
  filter(key != "Own_Rank") %>%
  ggplot(aes(x = Parent_hdspIncome_pct_rank, y = value, color = key, linetype = cohort)) +
  geom_smooth(se = F) +
  facet_grid(~female) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(y = "Child Rank - Parent Rank", x = "Parents' Income Rank") +
  theme(legend.position = "bottom", legend.title=element_blank())# +
  #scale_color_manual(values = c("blue4", "dodgerblue1"))

p2 <- unrestricted_offspring_summarized %>%
  filter(complete.cases(Parent_hdspIncome_quintile_label), cohort == "cohort_2") %>%
  mutate(Family_Rank = Offspring_hdspIncome_pct_rank - Parent_hdspIncome_pct_rank,
         Own_Rank_Gender = Offspring_GenderRef_ownIncome_pct_rank- Parent_hdspIncome_pct_rank,
         Own_Rank = Offspring_ownIncome_pct_rank - Parent_hdspIncome_pct_rank, 
         female = ifelse(female == 1, "Women", "Men"),
         cohort = ifelse(cohort == "cohort_1", "1965-1974", "1974-1982")) %>%
  select(Parent_hdspIncome_pct_rank, Family_Rank, Own_Rank, Own_Rank_Gender,
         female, cohort) %>%
  gather(key, value, -c(Parent_hdspIncome_pct_rank, female, cohort)) %>%
  filter(key != "Own_Rank") %>%
  ggplot(aes(x = Parent_hdspIncome_pct_rank, y = value, color = key, linetype = cohort)) +
  geom_smooth(se = F) +
  facet_grid(~female) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed") + 
  labs(y = "Child Rank - Parent Rank", x = "Parents' Income Rank") +
  theme(legend.position = "bottom", legend.title=element_blank())# +
#scale_color_manual(values = c("blue4", "dodgerblue1"))

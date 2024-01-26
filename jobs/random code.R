

unrestricted_offspring %>% 
  group_by(cohort, female, ever_married) %>%
  filter(complete.cases(hdspIncomeQuartile))%>%
  count(hdspIncomeQuartile) %>%
  mutate(proportion = n/sum(n)) %>%
  ggplot(aes(x = hdspIncomeQuartile, y = proportion, fill = as.factor(ever_married))) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_grid(cols = vars(female), rows = vars(cohort))

# Test plot
mobility_plot_cohort1_mob <- unrestricted_offspring %>%
  group_by(cohort, hdspIncomeQuartile, female, fam.mob) %>%
  summarise(self_reliance_cor = cor(ownIncome, hdspIncome.x),
            n = n()) %>%
  left_join(., unrestricted_offspring %>%
              group_by(cohort, hdspIncomeQuartile, female) %>%
              summarise(total= n())) %>%
  mutate(percent = n/total,
         total = 1,
         Gender = ifelse(female == 1, "Women", "Men"),
         Mobility = ifelse(fam.mob == 1, "Upward", "Downward")) %>%
  filter(complete.cases(hdspIncomeQuartile),
         cohort == "cohort_1") %>%
  ggplot(aes(x = hdspIncomeQuartile, y = percent, fill = self_reliance_cor, color = "Mobility"
  ))+
  geom_col(position = "stack") +
  facet_wrap(~Gender) +
  labs(x = "", y = "Percent", title = "1960-1974 Birth Cohort: Full",
       fill = "Corr(Own Income, Family Income)") +
  theme_minimal() + 
  scale_fill_gradient(limits = c(0, 1)) +
  scale_color_brewer("black") +
  guides(fill = guide_legend(title.position = "top", label.position = "bottom"),
         color = "none") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal") 


mobility_plot_cohort1_joint <- unrestricted_offspring %>%
  group_by(cohort, hdspIncomeQuartile, female, fam.mob) %>%
  summarise(n = n(),
            self_reliance_cor = cor(ownIncome, hdspIncome.x)) %>%
  mutate(percent = 1,
         Gender = ifelse(female == 1, "Women", "Men"),
         Mobility = ifelse(fam.mob == 1, "Upward", "Downward")) %>%
  filter(complete.cases(hdspIncomeQuartile),
         cohort == "cohort_1") %>%
  ggplot(aes(x = hdspIncomeQuartile, y = percent, fill = self_reliance_cor, #color = married
  ))+
  geom_col(position = "stack") +
  facet_grid(cols = vars(Mobility), rows = vars(Gender)) +
  labs(x = "", y = "Percent", title = "1960-1974 Birth Cohort: Full",
       fill = "Corr(Own Income, Family Income)") +
  theme_minimal() + 
  scale_fill_gradient(limits = c(0, 1)) +
  guides(fill = guide_legend(title.position = "top", label.position = "bottom")) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal") 

mobility_plot_cohort1 <- unrestricted_offspring %>%
  group_by(cohort, hdspIncomeQuartile, married, female, fam.mob) %>%
  summarise(n = n(),
            self_reliance_cor = cor(ownIncome, hdspIncome.x)) %>%
  left_join(., unrestricted_offspring %>%
              group_by(cohort, hdspIncomeQuartile,  female, fam.mob) %>%
              summarise(total = n())) %>%
  mutate(percent = n/total,
         Gender = ifelse(female == 1, "Women", "Men"),
         Mobility = ifelse(fam.mob == 1, "Upward", "Downward")) %>%
  filter(complete.cases(hdspIncomeQuartile),
         cohort == "cohort_1") %>%
  mutate(self_reliance_cor = ifelse(married == 0, 1, self_reliance_cor)) %>%
  ggplot(aes(x = hdspIncomeQuartile, y = percent, fill = self_reliance_cor, #color = married
  ))+
  geom_col(position = "stack") +
  facet_grid(cols = vars(Mobility), rows = vars(Gender)) +
  labs(x = "Parent Income Quintile", y = "Percent", title = "1960-1974 Birth Cohort: by Marital Status") +
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient(limits = c(0, 1))

# Extract the legend from one of the plots
plot1_grob <- ggplotGrob(mobility_plot_cohort1_joint)
legend <- gtable::gtable_filter(plot1_grob, "guide-box")


mobility_plot_cohort2_joint <- unrestricted_offspring %>%
  group_by(cohort, hdspIncomeQuartile, female, fam.mob) %>%
  summarise(n = n(),
            self_reliance_cor = cor(ownIncome, hdspIncome.x)) %>%
  mutate(percent = 1,
         Gender = ifelse(female == 1, "Women", "Men"),
         Mobility = ifelse(fam.mob == 1, "Upward", "Downward")) %>%
  filter(complete.cases(hdspIncomeQuartile),
         cohort == "cohort_2") %>%
  ggplot(aes(x = hdspIncomeQuartile, y = percent, fill = self_reliance_cor, #color = married
  ))+
  geom_col(position = "stack") +
  facet_grid(cols = vars(Mobility), rows = vars(Gender)) +
  labs(x = "", y = "", title = "1975-1982 Birth Cohort: Full",
       fill = "Corr(Own Income, Family Income)") +
  theme_minimal() + 
  scale_fill_gradient(limits = c(0, 1)) +
  guides(fill = guide_legend(title.position = "top", label.position = "bottom")) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.direction = "horizontal") 

mobility_plot_cohort2 <- unrestricted_offspring %>%
  group_by(cohort, hdspIncomeQuartile, married, female, fam.mob) %>%
  summarise(n = n(),
            self_reliance_cor = cor(ownIncome, hdspIncome.x)) %>%
  left_join(., unrestricted_offspring %>%
              group_by(cohort, hdspIncomeQuartile,  female, fam.mob) %>%
              summarise(total = n())) %>%
  mutate(percent = n/total,
         Gender = ifelse(female == 1, "Women", "Men"),
         Mobility = ifelse(fam.mob == 1, "Upward", "Downward")) %>%
  filter(complete.cases(hdspIncomeQuartile),
         cohort == "cohort_2") %>%
  mutate(self_reliance_cor = ifelse(married == 0, 1, self_reliance_cor)) %>%
  ggplot(aes(x = hdspIncomeQuartile, y = percent, fill = self_reliance_cor, #color = married
  ))+
  geom_col(position = "stack") +
  facet_grid(cols = vars(Mobility), rows = vars(Gender)) +
  labs(x = "Parent Income Quintile", y = "", title = "1975-1982 Birth Cohort: by Marital Status") +
  theme_minimal() + 
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient(limits = c(0, 1))

gridplot <- grid.arrange(mobility_plot_cohort1_joint + theme(legend.position = "none"),
                         mobility_plot_cohort2_joint + theme(legend.position = "none"),
                         mobility_plot_cohort1, 
                         mobility_plot_cohort2,
                         ncol = 2, nrow = 2,
                         bottom = legend,
                         top = "Absolute Mobility and Self-Reliance across the Parental Income Distribution"
)

ggsave("gridplot.pdf", gridplot, units = "in", height = 8, width = 8)

# Descriptive plots, Class Gradient in Marriage

p1 <- unrestricted_offspring %>% 
  ggplot(aes(x = pct_rank_hdspIncome, y = ever_married, color = female, linetype = cohort)) +
  geom_smooth(method = "loess", se = F) +
  labs(x = "Parent Income Rank", y = "Percent Married", 
       title = "Current Marital Status") +
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

p2 <- unrestricted_offspring %>% 
  group_by(person, pct_rank_hdspIncome, female, cohort) %>%
  summarise(ever_married = mean(ever_married, na.rm = T)) %>%
  ggplot(aes(x = pct_rank_hdspIncome, y = ever_married, color = female, linetype = cohort)) +
  geom_smooth(method = "loess", se = F) +
  labs(x = "Parent Income Rank", y = "Percent Married", 
       title = "Ever Married by 42, Person-Specific") +
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

grid.arrange(p1, p2, nrow = 1, top = "Class Gradient in Marriage")

# Descriptive plots, Absolute mobility
# Percent of people with higher income than parents by birthyear

unrestricted_offspring %>% filter(age %in% c(35:42), complete.cases(hdspIncomeQuartile)) %>%
  mutate(Gender = ifelse(female == 1, "Women", "Men")) %>%
  group_by(Gender, cohort, hdspIncomeQuartile) %>% 
  summarise(fam.mob = mean(fam.mob, na.rm = T), 
            own.mob = mean(own.mob, na.rm = T), 
            dad.mob = mean(dad.mob, na.rm = T),
            mom.mob = mean(mom.mob, na.rm = T)) %>%
  filter(cohort == "cohort_1") %>%
  gather(type, value, -c(Gender, cohort, hdspIncomeQuartile)) %>%
  filter(type %in% c("fam.mob", "own.mob")) %>%
  ggplot(aes(x = hdspIncomeQuartile, y = value, fill = type)) +
  geom_col(position = "dodge") + # Use geom_col when you have pre-assigned y values
  facet_wrap(~Gender) + # Replace facet_var1 and facet_var2 with your faceting variables
  labs(x = "Parent Income Quantile", y = "% Earning More than Parents", title = "Cross-Section Aged 30-35") +
  theme_bw()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))


##########



data_p1 <- unrestricted_offspring_summarized %>%
  filter(complete.cases(hdspIncomeQuartile)) %>%
  group_by(female, cohort, hdspIncomeQuartile) %>%
  count(FamilyIncome_OffQuartile) %>%
  mutate(proportion = n/sum(n), 
         class = "Family Income") %>%
  rename(ChildQuantile = FamilyIncome_OffQuartile) %>%
  select(female, cohort, hdspIncomeQuartile, ChildQuantile, class, proportion)

data_p2 <- unrestricted_offspring_summarized %>%
  filter(complete.cases(hdspIncomeQuartile)) %>%
  group_by(female, cohort, hdspIncomeQuartile) %>%
  count(OwnIncome_OffQuartile) %>%
  mutate(proportion = n/sum(n), 
         class = "Own Income") %>%
  rename(ChildQuantile = OwnIncome_OffQuartile) %>%
  select(female, cohort, hdspIncomeQuartile, ChildQuantile, class, proportion)

data <- bind_rows(data_p1, data_p2) %>%
  mutate(female = ifelse(female == 1, "Women", "Men"),
         interaction_var = interaction(hdspIncomeQuartile, class, sep = " "),
         interaction_var = fct_inorder(interaction_var))

c1 <- data %>% filter(cohort == "cohort_1") %>%
  ggplot(aes(x = interaction_var, y = proportion, fill = ChildQuantile)) +
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
  ggplot(aes(x = interaction_var, y = proportion, fill = ChildQuantile)) +
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

############


p2 <- data_p2 %>% filter(cohort == "cohort_1") %>%
  ggplot(aes(x = hdspIncomeQuartile, y = proportion, fill = OwnIncome_OffQuartile)) +
  geom_bar(stat = "identity") +
  scale_fill_grey(start = 0.8, end = 0.2) + # Adjust the colors accordingly
  theme_minimal() +
  labs(x = "Parent's Quintile", y = "Transition Probability") +
  facet_wrap(~female) +
  theme(legend.title = element_blank(), legend.position = "bottom")

grid.arrange(p1, p2, nrow = 2)

unrestricted_offspring_summarized %>%
  mutate(Gender = ifelse(female == 1, "Women", "Men")) %>%
  group_by(Gender, cohort, hdspIncomeQuartile) %>% 
  summarise(fam.mob = mean(fam.mob, na.rm = T), 
            own.mob = mean(own.mob, na.rm = T), 
            dad.mob = mean(dad.mob, na.rm = T),
            mom.mob = mean(mom.mob, na.rm = T)) %>%
  filter(cohort == "cohort_1") %>%
  gather(type, value, -c(Gender, cohort, hdspIncomeQuartile)) %>%
  filter(type %in% c("fam.mob", "own.mob")) %>%
  ggplot(aes(x = hdspIncomeQuartile, y = value, fill = type)) +
  geom_col(position = "dodge") + # Use geom_col when you have pre-assigned y values
  facet_wrap(~Gender) + # Replace facet_var1 and facet_var2 with your faceting variables
  labs(x = "Parent Income Quantile", y = "% Earning More than Parents", title = "Permanent Income") +
  theme_bw()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

unrestricted_offspring %>% filter(age %in% c(30:35)) %>%
  group_by(female, birth_year) %>%
  summarise(fam.mob = mean(fam.mob, na.rm = T), 
            own.mob = mean(own.mob, na.rm = T),
            dad.mob = mean(dad.mob, na.rm = T), 
            mom.mob = mean(mom.mob, na.rm = T)) %>%
  gather(key, value, -c(female, birth_year)) %>%
  mutate(key = case_when(key == "fam.mob" ~ "Own + Spouse's, Mother + Father", 
                         key == "own.mob" ~ "Own, Mother + Father", 
                         key == "dad.mob" ~ "Own, Father",
                         key == "mom.mob" ~ "Own, Mother")) %>%
  ggplot(aes(x = birth_year, y = value, color = female)) +
  geom_smooth() +
  geom_jitter() +
  facet_wrap(~key) +
  labs(x = "Birth Cohort", y = "% Earning More than Parents", 
       title = "Absolute Mobility by Cohort and Income Concept") +
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5))

c1 <- unrestricted_offspring %>% filter(age %in% c(35:40), complete.cases(hdspIncomeQuartile)) %>%
  mutate(Gender = ifelse(female == 1, "Women", "Men")) %>%
  group_by(Gender, cohort, hdspIncomeQuartile, ever_married) %>% 
  summarise(fam.mob = mean(fam.mob, na.rm = T), 
            own.mob = mean(own.mob, na.rm = T), 
            dad.mob = mean(dad.mob, na.rm = T),
            mom.mob = mean(mom.mob, na.rm = T)) %>%
  filter(cohort == "cohort_1") %>%
  gather(type, value, -c(Gender, cohort, ever_married, hdspIncomeQuartile)) %>%
  filter(type %in% c("fam.mob", "own.mob")) %>%
  ggplot(aes(x = hdspIncomeQuartile, y = value, fill = type)) +
  geom_col(position = "dodge") + # Use geom_col when you have pre-assigned y values
  facet_grid(rows = vars(Gender), cols = vars(ever_married)) + # Replace facet_var1 and facet_var2 with your faceting variables
  labs(x = "Parent Income Quantile", y = "% Earning More than Parents", title = "Cross-Section Aged 30-35") +
  theme_bw()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1)

c2 <- unrestricted_offspring %>% filter(age %in% c(35:40), complete.cases(hdspIncomeQuartile)) %>%
  mutate(Gender = ifelse(female == 1, "Women", "Men")) %>%
  group_by(Gender, cohort, hdspIncomeQuartile, ever_married) %>% 
  summarise(fam.mob = mean(fam.mob, na.rm = T), 
            own.mob = mean(own.mob, na.rm = T), 
            dad.mob = mean(dad.mob, na.rm = T),
            mom.mob = mean(mom.mob, na.rm = T)) %>%
  filter(cohort == "cohort_2") %>%
  gather(type, value, -c(Gender, cohort, ever_married, hdspIncomeQuartile)) %>%
  filter(type %in% c("fam.mob", "own.mob")) %>%
  ggplot(aes(x = hdspIncomeQuartile, y = value, fill = type)) +
  geom_col(position = "dodge") + # Use geom_col when you have pre-assigned y values
  facet_grid(rows = vars(Gender), cols = vars(ever_married)) + # Replace facet_var1 and facet_var2 with your faceting variables
  labs(x = "Parent Income Quantile", y = "% Earning More than Parents", title = "Cross-Section Aged 30-35") +
  theme_bw()+
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1)

# Need these last plots, but collapsing "type" into four "types":
# both types of mobility among all men and women
# both types of mobility among married men and women
# own/family (it's the same) mobility among unmarried men and women

# Married data
marstat <- unrestricted %>% 
  filter(period == "offspring") %>% 
  arrange(person, age) %>% 
  group_by(person, female, period, cohort) %>%
  mutate(ever_married = any(marstat == 1)) %>%
  # Ungroup to perform further operations
  ungroup() %>%
  group_by(person, female, cohort) %>%
  summarise(ever_married = mean(ever_married, na.rm = T))

t <- unrestricted %>% filter(period == "offspring") %>% 
  select(person, age, female, period, cohort, marstat) %>%
  mutate(ever_married = ifelse(marstat != 2, 1, 0)) %>%
  left_join(., unrestricted_summarized_parent %>% ungroup() %>%
              select(person, ends_with("Quartile")),
            by = c("person"))

t %>% group_by(cohort, female, familyIncomeQuartile) %>%
  summarise(pct_ever_married = mean(ever_married, na.rm = T)) %>%
  filter(complete.cases(familyIncomeQuartile)) %>%
  mutate(familyIncomeQuartile = factor(familyIncomeQuartile, 
                                       levels = c("25th", "50th", "75th", "above 75th")),
         cohort = ifelse(cohort == "1965-1974","cohort1", "cohort2" )) %>%
  pivot_wider(names_from = cohort, values_from = pct_ever_married) %>%
  mutate(change = cohort2-cohort1)

t %>% group_by(age, female, cohort, familyIncomeQuartile) %>%
  summarise(pct.married = mean(ever_married, na.rm = T)) %>%
  filter(complete.cases(familyIncomeQuartile)) %>%
  ggplot(aes(x = age, y = pct.married, color = as.factor(female), 
             linetype = as.factor(cohort))) + 
  geom_jitter() +
  geom_smooth(method = "loess", se= F) + 
  facet_wrap(~familyIncomeQuartile, nrow = 1) + 
  theme_minimal()

t %>% arrange(person, age) %>% 
  group_by(person, female, period, cohort) %>%
  mutate(ever_married = any(marstat == 1)) %>%
  ungroup() %>%
  group_by(person, female, cohort) %>%
  summarise(ever_married = mean(ever_married, na.rm = T))


ggplot(aes(x = familyIncomeQuartile, y = pct_ever_married, 
           fill = female)) +
  geom_col() + 
  facet_grid(rows = vars(as.factor(female)), cols = vars(cohort)) + # Replace facet_var1 and facet_var2 with your faceting variables
  theme_minimal()

ggplot(df_summary, aes(x = familyIncome, y = percent_ever_married, color = cohort, group = cohort)) +
  geom_line() +
  facet_wrap(~female, scales = "free_y") +
  labs(
    title = "Percent Ever Married Across Parental Income Distribution by Cohort and female",
    x = "Parental Income",
    y = "Percent Ever Married"
  ) +
  theme_minimal()



unrestricted_summarized_parent <- unrestricted %>%
  mutate(ownIncome = case_when(relhead == 10 ~ headIncome,
                               relhead %in% c(20, 22) ~ spouseIncome),
         hdspIncome = headIncome + spouseIncome) %>%
  # Convert to permanent income
  group_by(person, period, female) %>%
  summarize(familyIncome = mean(familyIncome, na.rm = T),
            ownIncome = mean(ownIncome, na.rm = T),
            headIncome = mean(headIncome, na.rm = T),
            spIncome = mean(spouseIncome, na.rm = T), 
            hdspIncome = mean(hdspIncome, na.rm = T),
            age = mean(age, na.rm = T),
            observations = n(),
            marstat = paste(unique(marstat), collapse = " "),
            relhead = paste(unique(relhead), collapse = " "),
            # This mean simply aggregates since the num is constant
            num_observedInChildhood = mean(num_observedInChildhood)) %>%
  # Restrict to those observed in childhood and adulthood
  group_by(person) %>%
  filter(n() == 2) %>%
  group_by() %>%
  mutate(num_observedInAdulthood = n_distinct(person)) %>%
  #melt(id = c("person", "female", "period", "num_observedInChildhood","num_observedInAdulthood")) %>%
  melt(id = c("person", "female", "period", "cohort", "num_observedInChildhood","num_observedInAdulthood")) %>%
  mutate(variable = paste0(period,"_",variable)) %>%
  select(-period) %>% 
  spread(key = variable, value = value) %>%
  mutate(offspring_ownmobilityIncome = offspring_ownIncome - parent_hdspIncome,
         offspring_fammobilityIncome = offspring_hdspIncome - parent_hdspIncome,
         female = ifelse(female == 2, "Women", "Men"), 
         noInc = ifelse(offspring_ownIncome == 0, 1, 0),
         married = ifelse(offspring_marstat == 1, "Married", "Unmarried")) %>%
  filter(noInc == 0) %>%
  #group_by(female) %>%
  group_by(female, cohort) %>%
  mutate(across(ends_with("Income"), 
                ~ rank(., ties.method = "random") / length(.) * 100, 
                .names = "pct_rank_{.col}")) %>%
  select(person, female, married, offspring_marstat, parent_marstat, ends_with("Income")) %>%
  mutate(absmolfam = case_when(offspring_fammobilityIncome <= -5000 ~ "Downward",
                               offspring_fammobilityIncome <=5000 ~ "Persistent",
                               TRUE ~ "Upward"))

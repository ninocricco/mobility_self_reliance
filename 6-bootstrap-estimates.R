bs_estimates <- list()

for(i in 1:10000){
  
  set.seed(i)
  
  bs_sample <- test %>%
    group_by(cohort, female) %>%
    nest() %>%              
    ungroup() %>% 
    arrange(cohort, female) %>%
    mutate(n = c(1157, 1283, 977, 1073)) %>% mutate(samp = map2(data, n, replace = T, sample_n)) %>%
    select(-c(data, n)) %>%
    unnest(samp)
  
  bs_estimates[[i]] <- generate_fig1(data = bs_sample, object = "values")
}

# Combine all data frames into one data frame
df_combined <- do.call(rbind, bs_estimates) %>%
  pivot_wider(values_from = mob_sr_ratio, names_from = c(Gender, key)) %>%
  unnest()

df_percentiles <- sapply(df_combined, function(x) quantile(x, probs=c(0.05, 0.95)))

fig1_values <- fig1_values %>%
  left_join(df_percentiles %>% 
              as.data.frame() %>% 
              mutate(quantile = rownames(.)) %>% gather(key, value, -quantile) %>%
              separate(key, into = c("Gender", "key"), sep = "_"), 
            by = c("Gender", "key"))


fig1 <- fig1_values %>%
  mutate(quantile = case_when(quantile == "5%" ~ "fifth",
                              quantile == "95%" ~ "ninetyfifth")) %>%
  pivot_wider(names_from = quantile, values_from = value) %>%
  # Change this line so that it's not hard coded- use regex to extract
  transmute(cohort = gsub("cohort_(\\d{4})_(\\d{4})", "\\1-\\2", key), 
            Gender, mob_sr_ratio, fifth, ninetyfifth) %>%
  ggplot(aes(x = cohort, y = mob_sr_ratio, fill = cohort)) +
  geom_col(position = "identity", fill = "gray", color = "black") +
  geom_errorbar(aes(ymin = fifth, ymax = ninetyfifth), width = 0.2) +
  geom_text(aes(label = round(mob_sr_ratio, 2)), vjust = -0.5, hjust = 2) +  # Add labels on top of the bars
  theme_bw() +
  labs(y = "IMER", x = "Birth Cohort", 
       title = "Figure 1: Income Mobility Earnings Ratio by Gender and Cohort",
       caption = "Note: Data from the Panel Study of Income Dynamics.
       \ Error bars show 95 % bootstrapped confidence intervals.") +
  theme(plot.title = element_text(hjust = .5),
        legend.position = "",
        plot.caption = element_text(face = "italic", hjust = 1)) +
  facet_wrap(~Gender) +
  ylim(0,1)

data <- gen_data() %>% ungroup()

models <- data %>%
  nest_by(cohort, female) %>%
  mutate(model = list(lm(offspring_hdsp_income_pct_rank ~ 
                           parent_hdsp_income_pct_rank, data = data)),
         tidy_model = list(tidy(model, conf.int = TRUE)))

# Test for differences within female across cohorts
female_within_cohort <- data %>%
  nest_by(cohort) %>%
  mutate(model = list(lm(offspring_hdsp_income_pct_rank  ~ parent_hdsp_income_pct_rank  * female, data = data)),
         tidy_model = list(tidy(model, conf.int = TRUE)))

# Test for differences within cohort across females
cohort_within_female <- data %>%
  nest_by(female) %>%
  mutate(model = list(lm(offspring_hdsp_income_pct_rank  ~ parent_hdsp_income_pct_rank  * cohort, data = data)),
         tidy_model = list(tidy(model, conf.int = TRUE)))

# Test for the female difference across cohorts
female_diff_across_cohorts <- lm(offspring_hdsp_income_pct_rank  ~ parent_hdsp_income_pct_rank  * cohort * female, data = data)

# Combine models for stargazer
all_models <- c(models$model, female_within_cohort$model, cohort_within_female$model, list(female_diff_across_cohorts))

# Stargazer output
stargazer(all_models,
          type = "latex",
          title = "Regression Results",
          dep.var.labels = "Adult Income",
          covariate.labels = c("Parent Income", "female Interaction", "Cohort Interaction"),
          omit = "Constant",
          ci = TRUE,
          add.lines = list(
            c("Test: female within Cohort", "", "", "", ""),
            c("Test: Cohort within female", "", "", "", ""),
            c("Test: female Difference across Cohorts", "", "", "", "")
          ),
          column.labels = c("Cohort 1 Male", "Cohort 1 Female", "Cohort 2 Male", "Cohort 2 Female",
                            "female within Cohort", "Cohort within female", "female Difference"),
          keep.stat = c("n", "rsq"))

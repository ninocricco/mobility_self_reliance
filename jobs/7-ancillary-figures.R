#------------------------------------------------------------------------------
# Distributions by Income type, gender, cohort, parent quintile
#------------------------------------------------------------------------------
# Reshape the data
data_long <- gen_data() %>%
  pivot_longer(
    cols = c(hdsp_income, own_income),
    names_to = "income_type",
    values_to = "income"
  ) %>%
  mutate(
    income_type = factor(income_type, 
                         levels = c("hdsp_income", "own_income"),
                         labels = c("Family Income", "Own Earnings")),
    parent_hdsp_income_quintile = factor(parent_hdsp_income_quintile,
                                         levels = 1:5,
                                         labels = paste("Q", 1:5)),
    cohort = factor(cohort, levels = c("1950-1969", "1970-1985"), labels = c("1950-1969", "1970-1985")),
    female = factor(female, levels = c(0, 1), labels = c("Men", "Women"))
  )

# Create a copy of the data with "All" as the quintile
data_all <- data_long %>%
  mutate(parent_hdsp_income_quintile = factor("All", levels = c("All", paste("Q", 1:5))))

# Combine the datasets
data_combined <- bind_rows(data_all, data_long)

# Calculate means for each group
means <- data_combined %>%
  group_by(female, cohort, income_type, parent_hdsp_income_quintile) %>%
  summarize(mean_income = mean(log(income + 1), na.rm = TRUE), .groups = "drop")

# Calculate the requested measures
measures <- means %>%
  group_by(income_type, parent_hdsp_income_quintile) %>%
  summarize(
    change_men = mean_income[female == "Men" & cohort == "1970-1985"] / mean_income[female == "Men" & cohort == "1950-1969"],
    change_women = mean_income[female == "Women" & cohort == "1970-1985"] / mean_income[female == "Women" & cohort == "1950-1969"],
    gap_old = mean_income[female == "Women" & cohort == "1950-1969"] / mean_income[female == "Men" & cohort == "1950-1969"],
    gap_young = mean_income[female == "Women" & cohort == "1970-1985"] / mean_income[female == "Men" & cohort == "1970-1985"],
    change_gap = gap_young - gap_old,
    .groups = "drop"
  ) %>%
  mutate(
    label = sprintf("Within-gender change:\nMen: %.2f, Women: %.2f\nGender gap:\n1950-1969: %.2f, 1970-1985: %.2f\nChange in gap: %.2f",
                    change_men, change_women, gap_old, gap_young, change_gap)
  )


# Create the plot
distributons_by_parentq <- ggplot(data_combined, aes(log(income + 1), color = female, linetype = cohort)) +
  geom_density() +
  geom_vline(data = means, 
             aes(xintercept = mean_income, color = female, linetype = cohort),
             alpha = 0.8) +
  geom_text(data = measures, 
            aes(x = -Inf, y = Inf, label = label),
            hjust = -0.1, vjust = 1.1, size = 2.5, color = "black",
            inherit.aes = FALSE) +  # Add this line
  facet_grid(rows = vars(income_type), 
             cols = vars(parent_hdsp_income_quintile), 
             scales = "free") +
  theme_bw() +
  labs(x = "Log(Income + 1)", y = "Density",
       title = "Income Distributions by Type, Cohort, Gender, and Parent Income Quartile") +
  scale_color_discrete(name = "Gender") +
  scale_linetype_discrete(name = "Cohort") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


#------------------------------------------------------------------------------
# Annualized Gender Gap/Persistence Estimates
#------------------------------------------------------------------------------
data_annualized <- unrestricted_offspring %>%
  filter(complete.cases(Parent_hdspIncome_pct_rank)) %>%
  filter(year >= 1980) %>%
  mutate(age_annualized = age-40,
         year = as.factor(year)
  ) %>%
  group_by(year, female) %>%
  mutate(across(c(hdspIncome, ownIncome, headIncome, spouseIncome), 
                ~ rank(., ties.method = "random") / length(.) * 100, 
                .names = "Offspring_{.col}_pct_rank"))

model_men <- lm(Offspring_ownIncome_pct_rank ~ year * Parent_hdspIncome_pct_rank + 
                  I(age_annualized^2) + I(age_annualized^3) + I(age_annualized^4) + 
                  Parent_hdspIncome_pct_rank + I(Parent_hdAge^2) + I(Parent_hdAge^3) + 
                  I(Parent_hdAge^4) + age_annualized * Parent_hdspIncome_pct_rank +
                  age_annualized + Parent_hdAge, 
                data = data_annualized %>% filter(female == 0))

model_women <- lm(Offspring_ownIncome_pct_rank ~ year * Parent_hdspIncome_pct_rank + 
                    I(age_annualized^2) + I(age_annualized^3) + I(age_annualized^4) + 
                    Parent_hdspIncome_pct_rank + I(Parent_hdAge^2) + I(Parent_hdAge^3) + 
                    I(Parent_hdAge^4) + age_annualized * Parent_hdspIncome_pct_rank +
                    age_annualized + Parent_hdAge, 
                  data = data_annualized %>% filter(female == 1))

tidy_men_own <- broom::tidy(model_men) %>%
  filter(grepl("Parent_hdspIncome_pct_rank", term)) %>%
  separate(term, into = c("year", "Parent_hdspIncome_pct_rank"), sep = ":") %>%
  mutate(year = if_else(row_number() == 1, "1980", year)) %>%
  slice(-n()) %>%
  mutate(year = as.numeric(stringr::str_replace(year, "year", ""))) %>%
  mutate(
    estimate_with_intercept = ifelse(year > 1980, estimate + first(estimate), estimate), # adjust this if your intercept is not the first row
    ci_lower = ifelse(year > 1980, estimate_with_intercept - 1.96 * std.error, estimate - 1.96 * std.error),
    ci_upper = ifelse(year > 1980, estimate_with_intercept + 1.96 * std.error, estimate + 1.96 * std.error),
    Gender = "Men's IRP"
  ) %>% transmute(year, est = estimate_with_intercept, ci_lower, ci_upper, Gender)

tidy_women_own <- broom::tidy(model_women) %>%
  filter(grepl("Parent_hdspIncome_pct_rank", term)) %>%
  separate(term, into = c("year", "Parent_hdspIncome_pct_rank"), sep = ":") %>%
  mutate(year = if_else(row_number() == 1, "1980", year)) %>%
  slice(-n()) %>%
  mutate(year = as.numeric(stringr::str_replace(year, "year", ""))) %>%
  mutate(
    estimate_with_intercept = ifelse(year > 1980, estimate + first(estimate), estimate), # adjust this if your intercept is not the first row
    ci_lower = ifelse(year > 1980, estimate_with_intercept - 1.96 * std.error, estimate - 1.96 * std.error),
    ci_upper = ifelse(year > 1980, estimate_with_intercept + 1.96 * std.error, estimate + 1.96 * std.error),
    Gender = "Women's IRP"
  ) %>% transmute(year, est = estimate_with_intercept, ci_lower, ci_upper, Gender)

dodge_width = .8

gap <- unrestricted %>%
  filter(year >= 1980) %>%
  mutate(Gender = ifelse(sex == 2, "Women", "Men"),
         ownIncome = case_when(relhead %in% c(1,10) ~ headIncome,
                               relhead %in% c(2, 20, 22) ~ spouseIncome)) %>%
  filter(age %in% c(25:55), 
         relhead %in% c(1, 2, 10, 20, 22)) %>%
  group_by(year, Gender) %>%
  summarise(ownIncome = Hmisc::wtd.mean(ownIncome, weight_cross, na.rm = T), 
            n = n()) %>%
  pivot_wider(names_from = Gender, values_from = c(ownIncome, n)) %>%
  mutate(ratio_income = ownIncome_Women/ownIncome_Men) %>%
  transmute(year, est = ratio_income, ci_upper = NA, ci_lower = NA, Gender = "Gender Gap in Income")

fig1 <- bind_rows(tidy_men_own, tidy_women_own, gap) %>%
  ggplot(aes(x = year, y = est, linetype = Gender, shape = Gender)) +
  geom_point(position = position_dodge(width = dodge_width)) +
  geom_line(position = position_dodge(width = dodge_width)) +
  geom_smooth(se = F) +
  #geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2,
  #             position = position_dodge(width = dodge_width), 
  #            alpha = .5) +
  labs(
    x = "Year",
    y = "",
    title = "Gender-Specific Intergenerational Rank Persistence over Time"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = .5),
        legend.title = element_blank()) +
  ylim(0, .7)

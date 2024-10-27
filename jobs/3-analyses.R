#------------------------------------------------------------------------------
# PROJECT: MOBILITY SELF-RELIANCE
# FILE: ANALYSIS 
# AUTHOR: NINO CRICCO
# CONTACT: ncricco@g.harvard.edu
#------------------------------------------------------------------------------

# Loading dependencies 
source("jobs/2-gen_analysis_data.R")

# TABLE 1: DESCRIPTIVE STATISTICS OF THE ANALYTIC SAMPLE
t1_object <- unrestricted_offspring_summarized %>%
  mutate(gender = ifelse(female == 1, "Women", "Men")) %>%
  group_by(cohort, gender) %>%
  summarise(across(
    c(num_observedinchildhood, parent_hdsp_income, num_observedinadulthood,
      parent_head_age, parent_age, hdsp_income, 
      own_income, age, ever_married), 
    list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
    .names = "{.col}_{.fn}"), 
    n = n()) %>%
  gather(key, value, -c(cohort, gender)) %>%
  filter(key %!in% c("num_observedinchildhood_sd",
                     "num_observedinadulthood_sd",
                     "ever_married_sd")) %>%
  mutate(value = round(value, digits = 2)) %>%
  pivot_wider(names_from = c(cohort, gender), values_from = value)

t1_latex <- kable(t1_object %>% 
                    select(key, starts_with("cohort_1"), everything()) %>%
                    rename(Variable = key, 
                           "Men" = cohort_1_Men,
                           "Men " = cohort_2_Men,
                           "Women" = cohort_1_Women,
                           "Women " = cohort_2_Women),
            format = "latex", booktabs = TRUE,
            caption = "Descriptive Statistics by Gender and Cohort") %>%
  add_header_above(c(" ", "1950-1969 Cohort" = 2, "1970-1984 Cohort" = 2)) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

#------------------------------------------------------------------------------
# ESTIMATING DESCRIPTIVE PARAMETERS BY GENDER AND COHORT
#------------------------------------------------------------------------------

# Creating a nested dataframe by cohort and gender
nested_data <- unrestricted_offspring_summarized %>%
  group_by(cohort, female) %>%
  nest()

# Defining formulas for each model
formulas <- list(
  own_parent = offspring_gender_own_income_pct_rank ~ 
    parent_hdsp_income_pct_rank,
  family_own = offspring_hdsp_income_pct_rank ~ 
    offspring_gender_own_income_pct_rank,
  family_parent = offspring_hdsp_income_pct_rank ~ 
    parent_hdsp_income_pct_rank
)

# Create a function that creates tidy outputs from model results
fit_models <- function(data_to_fit, formula) {
  lm(formula, data = data_to_fit)
}

# Function to calculate residuals and fit a new model
fit_residual_model <- function(model, data_to_fit, formula_for_residuals) {
  # Calculate residuals from the initial model and add them to the data frame
  residuals_data <- data_to_fit %>% 
    mutate(residuals = resid(model))
  
  # Fit a new model to these residuals
  lm(formula_for_residuals, data = residuals_data)
}

# Apply initial models
initial_models <- map(formulas, ~ nested_data %>%
                        mutate(fitted_models = map(data, fit_models, .x))) %>%
  imap(., ~ mutate(.x, model_name = .y))

# Fitting residual models
residual_models <- map(initial_models, ~ .x %>%
                         mutate(fitted_models = map2(
                           fitted_models, data, 
                           ~ fit_residual_model(
                             .x, .y, "residuals ~ parent_hdsp_income_pct_rank"
                             )))) %>%
  imap(., ~ mutate(.x, model_name = paste("residual", .y, sep = "_")))

names(residual_models) <- paste(names(residual_models), "_residuals", sep = "")

# Combining primary models with residualized models
models_all <- c(initial_models, residual_models)

# Processing function to extract parameters of interest
process_models <- function(df, group_keys) {
  map2_df(df$fitted_models, df$model_name, ~ {
    model <- .x
    model_name <- .y
    
    # Extract cohort and female from group_keys
    cohort <- group_keys$cohort
    female <- group_keys$female
    
    tidy_data <- broom::tidy(model, conf.int = T)
    glance_data <- broom::glance(model)
    
    tidy_data %>%
      mutate(cohort = cohort,
             female = female,
             model_name = model_name,
             adj_r_squared = glance_data$adj.r.squared[1])
  })
}

# Apply processing function to model list to output estimated parameters by
# cohort and gender
combined_results <- map_df(models_all, ~ {
  # Group by cohort and female to handle unique combinations within each tibble
  grouped_df <- group_by(.x, cohort, female)
  
  # Use group_map to apply process_models to each subgroup
  processed_models <- group_map(grouped_df, process_models, .keep = TRUE) %>% 
    bind_rows()
  
  return(processed_models)
})

#------------------------------------------------------------------------------
# FIGURE 1: CHANGES IN MOBILITY SELF-RELIANCE ACROSS COHORTS, BY GENDER
#------------------------------------------------------------------------------
# Creating table showing estimated parameters
change_table_all <- combined_results %>% filter(term != "(Intercept)") %>%
  select(cohort, female, model_name, estimate, adj_r_squared) %>%
  gather(key, value, -c(cohort, female, model_name)) %>%
  pivot_wider(names_from = cohort, values_from = value) %>%
  mutate(change = (cohort_2-cohort_1) * 100,
         Gender = ifelse(female == 0, "Men", "Women"),
         key = ifelse(key == "estimate", "Slope", "Adj.R2"),
         key = factor(key, levels = c("Slope", "Adj.R2"))) %>%
  rename(estimate = key, cohort_1950_1969 = cohort_1, cohort_1970_1985 = cohort_2) %>%
  arrange(Gender, model_name, estimate) %>%
  select(Gender, model_name, estimate, everything(), -female)

change_table <- change_table_all %>%
  filter(estimate == "Slope") %>%
  select(-estimate) %>%
  gather(key, value, - c(Gender, model_name))

fig1_values <- change_table %>% pivot_wider(names_from = model_name, values_from = value) %>%
  transmute(Gender, key, mob_sr_ratio = own_parent * family_own/family_parent) %>%
  filter(key != "change")

fig1 <- fig1_values %>%
  # Change this line so that it's not hard coded- use regex to extract
  transmute(cohort = ifelse(key %in% "cohort_1950_1969", "1950-1969", "1970-1985"), 
            Gender, mob_sr_ratio) %>%
  ggplot(aes(x = cohort, y = mob_sr_ratio)) +
  geom_col(position = "identity", fill = "gray", color = "black") +
  theme_minimal() +
  labs(y = "Mobility Self-Reliance Ratio", x = "Birth Cohort", 
       title = "Changes in Mobility Self-Reliance by Cohort") +
  theme(plot.title = element_text(hjust = .5)) +
  facet_wrap(~Gender) +
  ylim(0,1)

# Testing, output of test should be all "TRUE"
change_table %>%
  filter(key != "change") %>%
  pivot_wider(names_from = model_name, values_from = value) %>%
  mutate(thru_income = own_parent * family_own, 
         faminc_test = thru_income + residual_family_own, 
         test = faminc_test - family_parent < 0.0000001) %>%
  tabyl(test)

#------------------------------------------------------------------------------
# FIG 2: All Estimated Parameters by Gender and cohort
#------------------------------------------------------------------------------
fig2 <- combined_results %>%
  filter(term != "(Intercept)") %>%
  filter(model_name %in% c("own_parent", "family_own", 
                           "family_parent", "residual_family_own")) %>%
  mutate(Gender = ifelse(female == 1, "Women", "Men"), 
         model_name = case_when(
           model_name == "own_parent" ~ "Beta Own",
           model_name == "family_own" ~ "Lambda",
           model_name == "family_parent" ~ "Beta Family",
           model_name == "residual_family_own" ~ "Delta")) %>%
  ggplot(aes(x = model_name, y = estimate, shape = cohort, color = Gender)) +
  geom_point(size = 2, position = position_dodge(width = 0.2)) +                  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),     # Add error bars for CI
                width = 0.2, position = position_dodge(width = 0.2)) +
  theme_minimal() +
  labs(title = "Linear Parameter Estimates by Gender and Cohort",
       y = "Estimate of the Rank-Rank Slope", x = "Model") +
  scale_color_manual(values = c("navyblue", "goldenrod")) +
  theme(legend.position = "none",
        #axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1)

fig2
#------------------------------------------------------------------------------
# FIGURE 3: CHANGING PATHWAYS TO FAMILY INCOME PERSISTENCE
#------------------------------------------------------------------------------
fig3_values <- change_table %>%
  filter(key != "change") %>%
  pivot_wider(names_from = model_name, values_from = value) %>%
  mutate(through_income = own_parent *family_own, 
         sum_through_income = own_parent + family_own) %>%
  select(Gender, key, own_parent, family_own, 
         through_income, sum_through_income, residual_family_own) %>%
  gather(parameter, value, -c(
    Gender, key, sum_through_income, own_parent, family_own)) %>%
  mutate(
    share_own_parent = case_when(
      parameter == "residual_family_own" ~ 1,
      parameter == "through_income" ~ own_parent/sum_through_income),
    share_family_own = case_when(
      parameter == "residual_family_own" ~ 1,
      parameter == "through_income" ~ family_own/sum_through_income)) %>%
  select(Gender, key, parameter, value, starts_with("share")) %>% 
  pivot_longer(cols = starts_with("share"), 
               names_to = "share_type", 
               values_to = "share_value") %>%
  mutate(share_type = ifelse(
    parameter == "through_income", share_type, parameter), 
    value = ifelse(
      parameter == "through_income", value * share_value, value)) %>%
  unique()

fig3 <- fig3_values %>%
  mutate(Cohort = str_replace(key, "cohort_", ""), 
         share_type = factor(case_when(
           share_type == "residual_family_own" ~ "Residual Transmission net of Earnings",
           share_type == "share_family_own" ~ "Self-Reliance",
           share_type == "share_own_parent" ~ "Earnings Persistence"), 
           levels = c("Residual Transmission net of Earnings",
                      "Earnings Persistence", "Self-Reliance"))) %>%
  ggplot(aes(x = Cohort, y = value, fill = share_type)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Changing Pathways towards Intergenerational Income Persistence",
    y = "Rank-Rank Slope, Family Labor Income") +
  scale_fill_manual(values = c("gray59", "grey38", "gray20"))+
  facet_wrap(~Gender) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = .5),
        legend.position = c(.9, .9),
        legend.justification = c("right", "top"))  +
  ylim(0, .75)

fig3

grid.arrange(fig1, fig2, fig3, nrow = 1)

#------------------------------------------------------------------------------
# TABLE 2: ESTIMATED PARAMETERS AND COUNTERFACTUAL SIMULATIONS AMONG WOMEN
#------------------------------------------------------------------------------

simulating_counterfactual_change <- change_table %>%
  pivot_wider(names_from = model_name, values_from = value) %>%
  filter(key != "change") %>%
  select(-c(residual_family_parent, residual_own_parent)) %>%
  arrange(Gender) %>%
  group_by(Gender) %>%
  mutate(family_own_prior = lag(family_own),
         own_parent_prior = lag(own_parent),
         residual_family_own_prior = lag(residual_family_own)) %>%
  mutate(
    msr_ratio_c2 = (
      own_parent * family_own) / 
      ((own_parent * family_own) + residual_family_own), 
    msr_ratio_c1 = lag(msr_ratio_c2),
    "Only Lambda Changes" = (
      own_parent_prior * family_own)/
      ((own_parent_prior * family_own) + residual_family_own_prior), 
    "Only Delta Changes" = (
      own_parent_prior * family_own_prior)/
      ((own_parent_prior * family_own_prior) + residual_family_own), 
    "Only Beta Own Changes" = (
      own_parent * family_own_prior)/
      ((own_parent * family_own_prior) + residual_family_own_prior),
    "Beta Own and Lambda Change" = (own_parent * family_own)/
      ((own_parent * family_own) + residual_family_own_prior)) %>% 
  # Change hard-coding here
  filter(key == "cohort_1970_1985") %>% 
  select(Gender, starts_with("msr"), starts_with("Only"), starts_with("Beta")) %>% 
  gather(key, c, -c(Gender, msr_ratio_c2, msr_ratio_c1)) %>% 
  select(Gender, key, msr_ratio_c1, msr_ratio_c2, c) %>%
  mutate(real_change = msr_ratio_c2 - msr_ratio_c1, 
         c_change = c - msr_ratio_c1,
         pct.exp= c_change/real_change) %>% 
  arrange(Gender) %>% 
  select(Gender, key, msr_ratio_c1, msr_ratio_c2, c, real_change, c_change, pct.exp)

parameters_object <- bind_rows(
  msr %>% 
    pivot_wider(names_from = key, values_from = mob_sr_ratio) %>%
    mutate(key = "msr_ratio", change = (cohort_1970_1985-cohort_1950_1969) * 100) %>%
    select(Gender, key, cohort_1950_1969, cohort_1970_1985, change),
  change_table %>% 
    rename(cohort = key, key = model_name) %>%
    pivot_wider(names_from = cohort, values_from = value) %>%
    filter(key %!in% c("residual_family_parent", "residual_own_parent")) %>%
    mutate(key = factor(key, levels = c("family_parent", "own_parent", 
                                        "family_own", "residual_family_own"))) %>%
    arrange(key))

parameters_simulations_object <- bind_rows(parameters_object,
  simulating_counterfactual_change %>% 
    transmute(Gender, key, cohort_1950_1969 = rep(NA, 4),
              cohort_1970_1985 = c, change = c_change*100))

t2_latex <- kable(
  parameters_simulations_object %>% 
    filter(Gender == "Women") %>%
    select(-Gender) %>%
    rename(Parameter = key),
  format = "latex", booktabs = TRUE,
  caption = "Cohort Changes in Women's Mobility Self-Reliance") %>%
  pack_rows("Estimated Parameters", 1, 5, bold = F) %>%
  pack_rows("Counterfactual Simulations", 6, 9, bold = F) %>%
  kable_styling(latex_options = c("striped", "scale_down"))

#------------------------------------------------------------------------------
# TABLE 3: ANALYSES BY MARITAL STATUS AMONG WOMEN
#------------------------------------------------------------------------------

data <- gen_data()

data_augmented <- data %>%
  group_by(cohort, female) %>%
  do(augmented = broom::augment(lm(
    offspring_hdsp_income_pct_rank ~ 
      offspring_gender_own_income_pct_rank, data = .))) %>%
  ungroup() %>%
  unnest(augmented) %>%
  select(female, cohort, own_family_resid = .resid)

data_with_resids <- data %>%
  bind_cols(own_family_resid = data_augmented$own_family_resid)

meanvar_marstat <- data_with_resids %>%
  group_by(cohort, female, ever_married) %>%
  summarise(across(
    c(parent_hdsp_income_pct_rank, 
      offspring_hdsp_income_pct_rank, 
      offspring_gender_own_income_pct_rank, 
      own_family_resid), 
    list(mean = ~ mean(.x), var = ~ var(.x)),
    .names = "{.col}_{.fn}"), 
    n = n()) %>%
  left_join(data_with_resids %>% group_by(cohort, female) %>% summarise(total = n()), 
            by = c("female", "cohort")) %>%
  mutate(prop = n/total) %>%
  left_join(data_with_resids %>%
              group_by(cohort, female) %>%
              summarise(across(
                c(parent_hdsp_income_pct_rank, 
                  offspring_hdsp_income_pct_rank, 
                  offspring_gender_own_income_pct_rank,
                  own_family_resid), 
                list(mean = ~ mean(.x), 
                     var = ~ var(.x)),
                .names = "{.col}_{.fn}_global")))


estimate_parameters(gen_data(), by_marstat = T) %>%
  filter(term != "(Intercept)", 
         model_name %!in% c("residual_family_parent", "residual_own_parent")) %>% 
  ungroup() 


models_means_marstat <- estimate_parameters(gen_data(), by_marstat = T) %>%
  filter(term != "(Intercept)", 
         model_name %!in% c("residual_family_parent", "residual_own_parent")) %>% 
  ungroup() %>% 
  select(female, cohort, ever_married, model_name, estimate) %>%
  left_join(., meanvar_marstat, by = c("female", "cohort", "ever_married")) 

results_marstat_full <- models_means_marstat %>%
  filter(model_name != "family_parent") %>% 
  mutate(
    between = case_when(
      model_name == "family_own" ~ (
        (offspring_hdsp_income_pct_rank_mean - 
           offspring_hdsp_income_pct_rank_mean_global) * 
          (offspring_gender_own_income_pct_rank_mean - 
             offspring_gender_own_income_pct_rank_mean_global)) /
        offspring_gender_own_income_pct_rank_var_global, 
      model_name == "own_parent" ~ (
        (offspring_gender_own_income_pct_rank_mean - 
           offspring_gender_own_income_pct_rank_mean_global) * 
          (parent_hdsp_income_pct_rank_mean - 
             parent_hdsp_income_pct_rank_mean_global)) /
        parent_hdsp_income_pct_rank_var_global, 
      model_name == "residual_family_own" ~ (
        (own_family_resid_mean - 
           own_family_resid_mean_global) * 
          (parent_hdsp_income_pct_rank_mean - 
             parent_hdsp_income_pct_rank_mean_global)) /
        parent_hdsp_income_pct_rank_var_global),
    within = case_when(
      model_name == "family_own" ~ estimate * 
        (offspring_gender_own_income_pct_rank_var /
           offspring_gender_own_income_pct_rank_var_global), 
      model_name == "own_parent" ~ estimate * 
        (parent_hdsp_income_pct_rank_var /
           parent_hdsp_income_pct_rank_var_global),
      model_name == "residual_family_own" ~ estimate * 
        (parent_hdsp_income_pct_rank_var /
           parent_hdsp_income_pct_rank_var_global)), 
    sum_within_between = within + between,
    sum_within_between_scaled = sum_within_between * prop)

results_marstat_full %>%
  group_by(female, cohort, model_name) %>%
  summarise(estimate = sum(sum_within_between_scaled))

results_marstat_withlag <- results_marstat_full %>%
  select(-c(n, total)) %>%
  mutate(ever_married = ifelse(
    ever_married == 1,"Ever Married", "Never Married")) %>%
  group_by(female, ever_married, model_name) %>%
  mutate(across(where(is.numeric), lag, .names = "{.col}_lag")) %>%
  filter(cohort == "1970-1985")

t4_decomp <- results_marstat_withlag %>%
  transmute(between_change = prop_lag * (between - between_lag),
            within_change = prop_lag * (within-within_lag),
            prop_change = estimate * (prop - prop_lag))

t4_decomp_sum <- t4_decomp %>%
  group_by(female, model_name) %>%
  summarise(between_change = sum(between_change),
            within_change = sum(within_change),
            prop_change = sum(prop_change)) %>% 
  mutate(total_change = between_change + within_change + prop_change)

t4_decomp_sum_pct <- t4_decomp_sum %>%
  gather(key, value, -c(female, model_name, total_change)) %>%
  mutate(pct_change = (value/total_change)*100) %>%
  pivot_wider(names_from = key, values_from = c(value, pct_change))

t4_decomp_pct <- t4_decomp %>%
  select(female, model_name, ever_married, ends_with("change")) %>%
  left_join(t4_decomp_sum_pct %>% select(female, model_name, 
                                         total_change),
            by = c("female", "model_name")) %>%
  mutate(across(c(between_change, within_change, prop_change), 
                list(pct_change = ~.x/total_change), 
                .names = "{.col}_pct"),
         total = between_change_pct + within_change_pct + prop_change_pct)

fig4 <- t4_decomp_pct %>% 
  gather(key, value, -c(female, model_name, ever_married)) %>% 
  filter(female == 1) %>%
  filter(key %in% c(
    "between_change_pct", "within_change_pct", "prop_change_pct")) %>% 
  mutate(
    param = case_when(model_name == "own_parent" ~ 
                        "Earnings Persistence",
                      model_name == "family_own" ~ 
                        "Self-Reliance",
                      model_name == "residual_family_own" ~ 
                        "Residual Transmission Net of Earnings"),
    key = case_when(key == "between_change_pct" ~ "Between-Group",
                    key == "within_change_pct" ~ "Within-Group",
                    key == "prop_change_pct" ~ "Share")
    ) %>%
  ggplot(aes(x = ever_married, y = value, fill = key)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  facet_grid(rows = vars(female), cols = vars(param)) +
  scale_fill_manual(values = c("gray59", "grey38", "gray20"))+
  theme_minimal() +
  labs(title = "Decomposing Changes in MSR Parameters by Marital Status, Women",
       x = "", y = "% Explained") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = .5), 
        legend.title = element_text("none"))

#------------------------------------------------------------------------------
# TABLE 5: SIMULATIONS UNDER ASSUMPTIONS OF CHANGES IN INCOME DISTRIBUTIONS
#------------------------------------------------------------------------------



# FOR THESE ANAYSES: 
# RANK MEN AND WOMEN ACCORDING TO THE OVERALL DISTRIBUITON
# ASSIGN OFFSPRING WOMEN THE ABSOLUTE VALUES OF MEN WITH THE SAME EARNING RANK, 
# THEN RECOMPUTE SUM OF HEAD AND SPOUSE INCOME AND RE-RERANK
# ASSIGN OFFSPRING MEN THE ABSOLUTE VALUES OF WOMEN WITH THE SAME EARNING RANK, 
# THEN RECOMPUTE SUM OF HEAD AND SPOUSE INCOME AND RE-RERANK

#------------------------------------------------------------------------------
# TABLE/FIGURE 6: RELAX LINEARITY ASSUMPTIONS AND RE-COMPUTE
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# FURTHER: DO STUFF WITH ABSOLUTE INCOME? COMPARE IGE-E
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# CONTINUOUS FIGURE OF RANK PERSISTENCE OVER TIME AND GENDER GAP IN INCOME?
#------------------------------------------------------------------------------


#------------------------------------------------------------------------------
# RANDOM CODE- NOT YET DISCARDING
# Among individuals for whom we observe parental _income in any year, at what ages
# do we observe them, by birth cohort? 
# Based on this- do we select _income observations in a particular age range? 
# Throws away a lot of information but makes cohorts more "comparable" in a way
# Or is there another strategy- maybe residualizing _income on age with cohort fixed effects,
# or controlling for average age at which _income is observed?

# For now, tabling

unrestricted_offspring %>% filter(complete.cases(parent_hdsp__income)) %>%
  tabyl(age, birth_year)






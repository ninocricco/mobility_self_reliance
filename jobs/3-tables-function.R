#------------------------------------------------------------------------------
# PROJECT: MOBILITY SELF-RELIANCE
# FILE: FUNCTION TO GENERATE MAIN TABLES
# AUTHOR: NINO CRICCO
# CONTACT: ncricco@g.harvard.edu
#------------------------------------------------------------------------------

source("jobs/1-generate-analysis-data.R")
source("jobs/2-estimate-parameters.R")

#------------------------------------------------------------------------------
# TABLE 1: DESCRIPTIVE STATS
#------------------------------------------------------------------------------

generate_desc_stats <- function(data) {
  
  desc_stats_object <- data %>%
    mutate(gender = ifelse(female == 1, "Women", "Men")) %>%
    group_by(cohort, gender) %>%
    summarise(across(
      c(num_observedinchildhood, parent_hdsp_income, num_observedinadulthood,
        parent_head_age, parent_age, hdsp_income, 
        own_income, age, ever_married, 
        married, zero_earnings, not_in_lf, own_income_share,
        offspring_own_income_pct_rank), 
      list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"), 
      n = n()) %>%
    pivot_longer(cols = -c(cohort, gender),
                 names_to = "key", values_to = "value") %>%
    filter(key %!in% c("num_observedinchildhood_sd",
                       "num_observedinadulthood_sd",
                       "ever_married_sd")) %>%
    mutate(value = round(value, digits = 2)) %>%
    pivot_wider(names_from = c(gender, cohort), 
                values_from = value, names_sep = "_")
  
  return(desc_stats_object)

}

generate_desc_stats_table <- function(data){
  
  desc_stats_object <- generate_desc_stats(data)
  
  cohort_labels <- unique(data$cohort) 
  cohort_header <- setNames(rep(2, length(cohort_labels)), 
                            paste(cohort_labels, "Cohort"))
  
  desc_stats_latex <- kable(
    desc_stats_object, format = "latex", booktabs = TRUE,
    caption = "Descriptive Statistics by Gender and Cohort") %>%
    add_header_above(c(" " = 1, cohort_header))
  
  return(desc_stats_latex)
}



#------------------------------------------------------------------------------
# TABLE 2: ESTIMATED PARAMETERS AND COUNTERFACTUAL SIMULATIONS
#------------------------------------------------------------------------------

generate_params_sims <- function(data) {
  
  change_table <- generate_change_table(data) %>%
    rename(cohort = key)
  
  # Dynamic calculation of mobility self-reliance ratio
  msr <- change_table %>%
    pivot_wider(names_from = model_name, values_from = value) %>%
    transmute(Gender, cohort, 
              mob_sr_ratio = own_parent * family_own / family_parent)
  
  # Simulation of counterfactual changes dynamically
  simulating_counterfactual_change <- change_table %>%
    pivot_wider(names_from = model_name, values_from = value) %>%
    select(-c(residual_family_parent, residual_own_parent)) %>%
    arrange(Gender) %>%
    group_by(Gender) %>%
    mutate(family_own_prior = lag(family_own),
           own_parent_prior = lag(own_parent),
           residual_family_own_prior = lag(residual_family_own)) %>%
    mutate(
      msr_ratio_c2 = (own_parent * family_own) / 
        ((own_parent * family_own) + residual_family_own), 
      msr_ratio_c1 = lag(msr_ratio_c2),
      "Only Lambda Changes" = (own_parent_prior * family_own) /
        ((own_parent_prior * family_own) + residual_family_own_prior), 
      "Only Delta Changes" = (own_parent_prior * family_own_prior) /
        ((own_parent_prior * family_own_prior) + residual_family_own), 
      "Only Beta Own Changes" = (own_parent * family_own_prior) /
        ((own_parent * family_own_prior) + residual_family_own_prior),
      "Beta Own and Lambda Change" = (own_parent * family_own) /
        ((own_parent * family_own) + residual_family_own_prior)) %>%
    # Drops "first" cohort
    drop_na() %>%
    ungroup() %>%
    select(Gender, cohort, starts_with("msr"), 
           starts_with("Only"), starts_with("Beta")) %>% 
    gather(key, c, -c(Gender, cohort, msr_ratio_c2, msr_ratio_c1)) %>% 
    select(Gender, cohort, key, msr_ratio_c1, msr_ratio_c2, c) %>%
    mutate(real_change = msr_ratio_c2 - msr_ratio_c1, 
           c_change = c - msr_ratio_c1,
           pct.exp= c_change/real_change) %>% 
    arrange(Gender) %>% 
    select(Gender, cohort, key, msr_ratio_c1, msr_ratio_c2, c, 
           real_change, c_change, pct.exp)
  
  # Create a table of parameters object dynamically
  parameters_object <- msr %>%
    pivot_wider(names_from = cohort, values_from = mob_sr_ratio) %>%
    mutate(model_name = "mobility_self_reliance") %>%
    bind_rows(change_table %>%
                pivot_wider(names_from = cohort, values_from = value) %>%
                filter(model_name %!in% c(
                  "residual_family_parent", "residual_own_parent")))
  
  params_sims_object <- bind_rows(
    parameters_object %>%
      select(Gender, model_name, everything()), 
    simulating_counterfactual_change %>%
      transmute(Gender,model_name = key, cohort, c) %>%
      pivot_wider(names_from = cohort, values_from = c))
  
  return(params_sims_object)
  
}

generate_params_sims_table <- function(data, gender = "Women"){
  
  params_sims_object <- generate_params_sims(data)
  
  # Conditional output for LaTeX or object data
  params_sims_latex <- kable(
    params_sims_object %>%
      filter(Gender == gender) %>%
      select(-Gender) %>%
      rename(Parameter = model_name) %>%
      mutate(across(where(is.numeric), round, digits = 2)),
    format = "latex", booktabs = TRUE,
    caption = sprintf(
      "Cohort Changes in %s's Mobility Self-Reliance", gender)) %>%
    pack_rows("Estimated Parameters", 1, 5, bold = F) %>%
    pack_rows("Counterfactual Simulations", 6, 9, bold = F) 
  
  return(params_sims_latex)
  
}
  

generate_marstat_params <- function(data){
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
    left_join(data_with_resids %>% 
                group_by(cohort, female) %>% 
                summarise(total = n()), 
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
  
  models_means_marstat <- estimate_parameters(data, by_marstat = T) %>%
    filter(term != "(Intercept)", 
           model_name %!in% c(
             "residual_family_parent", "residual_own_parent")) %>%
    ungroup() %>% 
    select(female, cohort, ever_married, model_name, estimate) %>%
    left_join(., meanvar_marstat, by = c("female", "cohort", "ever_married")) 
  
  return(models_means_marstat)
  
}
#------------------------------------------------------------------------------
# TABLE 3: ANALYSES BY MARITAL STATUS
#------------------------------------------------------------------------------
generate_decomp_marstat <- function(data) {
  
  models_means_marstat <- generate_marstat_params(data)
  
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
  
  # Here, sums are within rounding error for beta and alpha but off for delta
  check_marstat_sums <- results_marstat_full %>%
    group_by(female, cohort, model_name) %>%
    summarise(estimate = sum(sum_within_between_scaled))
  
  latest_cohort <- unique
  
  results_marstat_withlag <- results_marstat_full %>%
    select(-c(n, total)) %>%
    mutate(ever_married = ifelse(
      ever_married == 1,"Ever Married", "Never Married")) %>%
    group_by(female, ever_married, model_name) %>%
    mutate(across(where(is.numeric), lag, .names = "{.col}_lag")) %>%
    filter(cohort == unique(data$cohort)[2])
  
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
  
  return(t4_decomp_pct)

}

generate_t3 <- function(data, gender = 1){
  
  marstat_decomp_latex <- kable(
    generate_decomp_marstat(data) %>%
      filter(female == gender) %>% 
      ungroup() %>%
      dplyr::select(-c(female, model_name)) %>%
      mutate(across(where(is.numeric), round, digits = 2)),
    format = "latex", booktabs = TRUE,
    caption = sprintf(
      "Decomposing Changes in Parameters by Marital Status", gender)) %>%
    pack_rows("Earnings Persistence", 1, 2, bold = F) %>%
    pack_rows("Earnings-Income Correlation", 3, 4, bold = F) %>%
    pack_rows("Residual Transmission", 5, 6, bold = F)
  
  return(marstat_decomp_latex)
  
}

generate_t4 <- function(data){
  
  t4a <- kable(generate_params_sims(gen_data()) %>%
    filter(Gender == "Men") %>%
    select(-Gender) %>%
    rename(Parameter = model_name) %>%
    mutate(across(where(is.numeric), round, digits = 2)),
    format = "latex", booktabs = TRUE,
    caption = sprintf("Cohort Changes in Men's Mobility Self-Reliance")) %>%
    pack_rows("Estimated Parameters", 1, 5, bold = F) %>%
    pack_rows("Counterfactual Simulations", 6, 9, bold = F) 
  
  t4b <- kable(generate_decomp_marstat(data) %>%
    filter(female == 0) %>% 
    ungroup() %>%
    select(female, model_name, ever_married,
           ends_with("pct"), total) %>%
    gather(key, value, -c(female, model_name, ever_married)) %>%
    pivot_wider(names_from = ever_married, values_from = value) %>%
    mutate(key = factor(key, levels = c("between_change_pct", 
               "within_change_pct",
               "prop_change_pct", 
               "total")),
           Gender = ifelse(female == 1, "Women", "Men")) %>%
      select(Gender, model_name, everything(), -c(female)) %>%
      arrange(model_name, key) %>%
      mutate(across(where(is.numeric), round, digits = 3)) %>%
      select(-c(Gender, model_name)),
    format = "latex", booktabs = TRUE,
    caption = sprintf("Decomposing Changes in Men's Parameters by Marital Status")) %>%
    pack_rows("Earnings Persistence", 1, 4, bold = F) %>%
    pack_rows("Earnings-Income Correlation", 5, 8, bold = F) %>%
    pack_rows("Residual Transmission", 9, 12, bold = F)
  
  return(c(t4a, t4b))
    
}




  
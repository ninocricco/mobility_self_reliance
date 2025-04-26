#------------------------------------------------------------------------------
# PROJECT: MOBILITY SELF-RELIANCE
# FILE: GENERATING ANALYSIS DATA: CREATING FEATURES AND RESHAPING
# AUTHOR: NINO CRICCO
# CONTACT: ncricco@g.harvard.edu
#------------------------------------------------------------------------------

estimate_parameters <- function(
    data, 
    by_marstat = FALSE, 
    by_parentq = FALSE,
    by_race = FALSE,
    var_specs = list(
      offspring_own = "offspring_own_income_gender_pct_rank",
      offspring_hdsp = "offspring_hdsp_income_gender_pct_rank",
      parent_hdsp = "parent_hdsp_income_gender_pct_rank"
    )
) {
  # Create grouped and nested dataframe
  nested_df <- data %>%
    group_by(cohort, female) %>%
    {if (by_marstat) group_by(., ever_married, .add = TRUE)
      else if (by_parentq) group_by(., parent_hdsp_income_quintile, .add = TRUE)
      else if (by_race) group_by(., race_ever, .add = TRUE)
      else .} %>%
    nest()
  
  # Define model specifications using the variable specifications
  model_specs <- tribble(
    ~model_name, ~formula,
    "own_parent", paste(var_specs$offspring_own, "~", var_specs$parent_hdsp),
    "family_own", paste(var_specs$offspring_hdsp, "~", var_specs$offspring_own),
    "family_parent", paste(var_specs$offspring_hdsp, "~", var_specs$parent_hdsp)
  )
  
  # Add residual model specification
  residual_spec <- tribble(
    ~model_name, ~formula,
    "family_own_residuals", paste("residuals ~", var_specs$parent_hdsp)
  )
  
  # Function to fit models and extract parameters
  results_df <- nested_df %>%
    mutate(models = map(data, function(df) {
      # Fit initial models
      initial_models <- model_specs %>%
        mutate(
          fitted_model = map(formula, ~lm(as.formula(.x), data = df)),
          model_summary = map(fitted_model, broom::tidy, conf.int = TRUE),
          model_stats = map(fitted_model, broom::glance)
        )
      
      # Calculate residuals for family_own model
      family_own_model <- initial_models %>%
        filter(model_name == "family_own") %>%
        pull(fitted_model) %>%
        .[[1]]
      
      df <- df %>%
        mutate(residuals = resid(family_own_model))
      
      # Fit residual model
      residual_model <- residual_spec %>%
        mutate(
          fitted_model = map(formula, ~lm(as.formula(.x), data = df)),
          model_summary = map(fitted_model, broom::tidy, conf.int = TRUE),
          model_stats = map(fitted_model, broom::glance)
        )
      
      # Combine all models
      bind_rows(initial_models, residual_model)
    })) %>%
    select(-data) %>%
    unnest(models) %>%
    unnest(model_summary) %>%
    mutate(adj_r_squared = map_dbl(model_stats, ~.x$adj.r.squared)) %>%
    select(-c(fitted_model, model_stats, formula))
  
  return(results_df)
  
}

# Modified generate_change_table function
generate_change_table <- function(
    data = data, 
    object = "summ",
    by_race = FALSE,
    var_specs = list(
      offspring_own = "offspring_own_income_gender_pct_rank",
      offspring_hdsp = "offspring_hdsp_income_gender_pct_rank",
      parent_hdsp = "parent_hdsp_income_gender_pct_rank"
    )
) {
  estimated_parameters <- estimate_parameters(
    data, var_specs = var_specs, by_race = by_race)
  
  if(by_race == FALSE){
  # Creating table showing estimated parameters
  change_table_all <- estimated_parameters %>% 
    filter(term != "(Intercept)") %>%
    select(cohort, female, model_name, estimate, adj_r_squared) %>%
    gather(key, value, -c(cohort, female, model_name)) %>%
    pivot_wider(names_from = cohort, values_from = value) %>%
    mutate(Gender = ifelse(female == 0, "Men", "Women"),
           key = ifelse(key == "estimate", "Slope", "Adj.R2"),
           key = factor(key, levels = c("Slope", "Adj.R2"))) %>%
    rename(estimate = key) %>%
    arrange(Gender, model_name, estimate) %>%
    ungroup() %>%
    select(Gender, model_name, estimate, everything(), -female)
  
  change_table <- change_table_all %>%
    filter(estimate == "Slope") %>%
    select(-estimate) %>%
    gather(key, value, - c(Gender, model_name))
  }
  else{
    change_table_all <- estimated_parameters %>% 
      filter(term != "(Intercept)") %>%
      select(cohort, female, model_name, race_ever, estimate, adj_r_squared) %>%
      gather(key, value, -c(cohort, female, race_ever, model_name)) %>%
      pivot_wider(names_from = c(cohort), values_from = value) %>%
      mutate(Gender = ifelse(female == 0, "Men", "Women"),
             key = ifelse(key == "estimate", "Slope", "Adj.R2"),
             key = factor(key, levels = c("Slope", "Adj.R2"))) %>%
      rename(estimate = key) %>%
      arrange(Gender, model_name, estimate) %>%
      ungroup() %>%
      select(Gender, race_ever, model_name, estimate, everything(), -female)
    
    change_table <- change_table_all %>%
      filter(estimate == "Slope") %>%
      select(-estimate) %>%
      gather(key, value, - c(Gender, race_ever, model_name))
  }
  
  if(object == "all"){
    return(change_table_all)
  }
  else{
    return(change_table)
  }
}

estimate_share <- function(data){
  
  share <- generate_change_table(data) %>% 
    pivot_wider(names_from = model_name, values_from = value) %>%
    transmute(Gender, key, 
              share_earnings = (own_parent * family_own)/family_parent)
  return(share)
  
}

estimate_ci_share_bs <- function(data, n_bootstrap = 10000, conf = .95){
  
  # Calculate cohort sizes dynamically
  cohort_sizes <- data %>%
    group_by(cohort, female) %>%
    summarize(n = n(), .groups = 'drop') %>%
    arrange(cohort, female) %>%
    pull(n)
  
  bs_estimates <- list()
  
  for(i in 1:n_bootstrap) {
    set.seed(i)
    bs_sample <- data %>%
      group_by(cohort, female) %>%
      nest() %>%
      ungroup() %>%
      arrange(cohort, female) %>%
      mutate(n = cohort_sizes) %>%
      mutate(samp = map2(data, n, ~sample_n(.x, .y, replace = TRUE))) %>%
      select(-c(data, n)) %>%
      unnest(samp)
      
      bs_estimates[[i]] <- estimate_share(data = bs_sample)
  }

  # Process bootstrap results
  df_combined <- do.call(rbind, bs_estimates) %>%
    pivot_wider(values_from = share_earnings, names_from = c(Gender, key)) %>%
    unnest()
  
  # Calculate percentiles
  df_percentiles <- sapply(df_combined, function(x) quantile(x, probs=c(1-conf, conf)))
  rownames(df_percentiles) <- c("min", "max")
  
  return(df_percentiles)
  
}

estimate_interactions <- function(
    data, 
    by_marstat = FALSE, 
    by_parentq = FALSE,
    return_tests = TRUE,
    var_specs = list(
      offspring_own = "offspring_own_income_gender_pct_rank",
      offspring_hdsp = "offspring_hdsp_income_gender_pct_rank",
      parent_hdsp = "parent_hdsp_income_gender_pct_rank"
    )
) {
  
  # Function to get residuals from family_own model
  get_residuals <- function(data) {
    family_own_model <- lm(paste(var_specs$offspring_hdsp, "~", var_specs$offspring_own), 
                           data = data)
    data$residuals <- resid(family_own_model)
    return(data)
  }
  
  # Add residuals to data
  data <- data %>%
    group_by(cohort) %>%
    group_modify(~get_residuals(.x)) %>%
    ungroup()
  
  # Define model specifications including residual model
  model_specs <- tribble(
    ~model_name, ~y_var, ~x_var,
    "own_parent", var_specs$offspring_own, var_specs$parent_hdsp,
    "family_own", var_specs$offspring_hdsp, var_specs$offspring_own,
    "family_parent", var_specs$offspring_hdsp, var_specs$parent_hdsp,
    "family_own_residuals", "residuals", var_specs$parent_hdsp
  )
  
  # Function to run both types of tests for a single model
  test_model <- function(data, y_var, x_var, model_name) {
    # Get unique cohorts
    cohorts <- sort(unique(data$cohort))
    
    # 1. Test gender differences within each cohort
    gender_tests <- map_dfr(cohorts, function(c) {
      # Subset data for this cohort
      cohort_data <- data %>%
        filter(cohort == c) %>%
        mutate(gender = factor(female, levels = c(0, 1), labels = c("Men", "Women")))
      
      # Fit model with gender interaction
      model <- lm(paste(y_var, "~", x_var, "* gender"), data = cohort_data)
      
      # Extract results
      broom::tidy(model, conf.int = TRUE) %>%
        filter(str_detect(term, "gender")) %>%
        mutate(
          model = model_name,
          cohort = c,
          test_type = paste("Gender Difference,", c)
        )
    })
    
    # 2. Test cohort differences within each gender
    cohort_tests <- data %>%
      mutate(gender = factor(female, levels = c(0, 1), labels = c("Men", "Women"))) %>%
      group_by(gender) %>%
      group_modify(~{
        model <- lm(paste(y_var, "~", x_var, "* cohort"), data = .x)
        broom::tidy(model, conf.int = TRUE) %>%
          filter(str_detect(term, "cohort")) %>%
          mutate(
            model = model_name,
            test_type = paste("Cohort Change,", .y$gender)
          )
      }) %>%
      ungroup()
    
    # Combine all results
    bind_rows(gender_tests, cohort_tests)
  }
  
  # Run tests for all models
  test_results <- model_specs %>%
    pmap_dfr(~test_model(data, ..2, ..3, ..1)) %>%
    mutate(
      stars = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.1 ~ "*",
        TRUE ~ ""
      ),
      formatted_result = sprintf("%.3f%s\n(%.3f, %.3f)\n[%.3f]", 
                                 estimate, stars, conf.low, conf.high, p.value)
    ) %>%
    arrange(model, test_type)
  
  return(test_results)
}

# Modified generate_supplement_estimates function
generate_supplement_estimates <- function(
    data = data,
    var_specs = list(
      offspring_own = "offspring_own_income_gender_pct_rank",
      offspring_hdsp = "offspring_hdsp_income_gender_pct_rank",
      parent_hdsp = "parent_hdsp_income_gender_pct_rank"
    )
) {
  est <- generate_change_table(data, var_specs = var_specs) %>% 
    pivot_wider(names_from = model_name, values_from = value) %>%
    transmute(Gender, key, family_parent, 
              share_earnings = (own_parent * family_own)/family_parent, 
              own_parent, family_own) %>%
    gather(param, value, -c(Gender, key)) %>%
    pivot_wider(names_from = c(Gender, param), values_from = value) %>%
    select(key, starts_with("Men"), everything())
  
  return(est)
}


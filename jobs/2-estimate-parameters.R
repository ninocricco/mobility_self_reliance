#------------------------------------------------------------------------------
# PROJECT: MOBILITY SELF-RELIANCE
# FILE: GENERATING ANALYSIS DATA: CREATING FEATURES AND RESHAPING
# AUTHOR: NINO CRICCO
# CONTACT: ncricco@g.harvard.edu
#------------------------------------------------------------------------------

#  Creating a nested dataframe by cohort and gender
estimate_parameters <- function(data, by_marstat = F){
  
  if(by_marstat == T){
    nested_data <- data %>%
      group_by(cohort, female, ever_married) %>%
      nest()
  }
  else{
    nested_data <- data %>%
      group_by(cohort, female) %>%
      nest()
  }
  
  # Defining formulas for each model
  formulas <- list(
    own_parent = offspring_gender_own_income_pct_rank ~ 
      parent_hdsp_income_pct_rank,
    family_own = offspring_hdsp_income_pct_rank ~ 
      offspring_gender_own_income_pct_rank + parent_hdsp_income_pct_rank,
    family_parent = offspring_hdsp_income_pct_rank ~ 
      parent_hdsp_income_pct_rank)
  
  # Create a function that creates tidy outputs from model result
  fit_models <- function(data_to_fit, formula) {
    lm(formula, data = data_to_fit)
    }
  
  # Apply models
  initial_models <- map(
    formulas, ~ nested_data %>%
      mutate(fitted_models = map(data, fit_models, .x))) %>%
    imap(., ~ mutate(.x, model_name = .y))
  
  # Processing function to extract parameters of interest
  process_models <- function(df, group_keys) {
    map2_df(df$fitted_models, df$model_name, ~ {
      model <- .x
      model_name <- .y
    
      # Extract cohort and female from group_keys
      cohort <- group_keys$cohort
      female <- group_keys$female
      if(by_marstat == T){
        ever_married <- group_keys$ever_married
      }
      
    
      tidy_data <- broom::tidy(model, conf.int = T)
      glance_data <- broom::glance(model)
      
      if(by_marstat == T){
        tidy_data %>%
          mutate(cohort = cohort,
                 female = female,
                 ever_married = ever_married,
                 model_name = model_name,
                 adj_r_squared = glance_data$adj.r.squared[1])
      }
      else{
        tidy_data %>%
          mutate(cohort = cohort,
                 female = female,
                 model_name = model_name,
                 adj_r_squared = glance_data$adj.r.squared[1])
      }
    })
    }

  # Apply processing function to model list to output estimated parameters by
  # cohort and gender
  combined_results <- map_df(initial_models, ~ {
    # Group by cohort and female to handle unique combinations within each tibble
    if(by_marstat == T){
      grouped_df <- group_by(.x, cohort, female, ever_married)
    }
    else{
      grouped_df <- group_by(.x, cohort, female)
    }
  
    # Use group_map to apply process_models to each subgroup
    processed_models <- group_map(grouped_df, process_models, .keep = TRUE) %>% 
      bind_rows()
    
    return(processed_models)
    })
  return(combined_results)
}

#------------------------------------------------------------------------------
# FUNCTION TO GENERATE TABLE NEEDED FOR FIGURES
#------------------------------------------------------------------------------

generate_change_table <- function(data = data, object = "summ"){
  estimated_parameters <- estimate_parameters(data)
  
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
    select(Gender, model_name, estimate, everything(), -female)
  
  change_table <- change_table_all %>%
    filter(estimate == "Slope") %>%
    select(-estimate) %>%
    gather(key, value, - c(Gender, model_name))
  
  if(object == "all"){
    return(change_table_all)
  }
  else{
    return(change_table)
  }
  
}

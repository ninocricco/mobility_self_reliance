# Write this as a function, then apply across specifications, merge to table, generate mean and range
# For fun, can generate combinations of specifications to do multiverse type thing 
est_param %>%
  filter(term != ("(Intercept)"), 
         model_name %in% c("own_parent", "family_own", 
                           "family_parent", "residual_family_own")) %>%
  select(model_name, estimate, cohort, female) %>%
  mutate(female = ifelse(female == 0, "Men", "Women"), 
         model_name = factor(
           model_name, levels = c(
             "family_parent", "own_parent", 
             "family_own", "residual_family_own"))) %>%
  pivot_wider(names_from = c("female"), values_from = estimate) %>%
  mutate(diff = Men - Women) %>%
  gather(key, value, -c(model_name, cohort)) %>%
  pivot_wider(names_from = c("cohort", "key"), values_from = value) %>%
  mutate(spec = "Main") %>%
  select(spec, everything()) %>%
  arrange(model_name)


run_parameter_analysis <- function(parameter_grid) {
  # Initialize empty list to store results
  results_list <- list()
  
  # Loop through each parameter combination
  for(i in seq_len(nrow(parameter_grid))) {
    # Extract current parameter set
    params <- parameter_grid[i,]
    
    # Generate data with current parameters
    data <- do.call(gen_data, as.list(params))
    
    # Run estimation
    est_param <- estimate_parameters(data)
    
    # Format results using your existing pipeline
    formatted_results <- est_param %>%
      filter(term != ("(Intercept)"), 
             model_name %in% c("own_parent", "family_own", 
                               "family_parent", "residual_family_own")) %>%
      select(model_name, estimate, cohort, female) %>%
      mutate(female = ifelse(female == 0, "Men", "Women"), 
             model_name = factor(
               model_name, levels = c(
                 "family_parent", "own_parent", 
                 "family_own", "residual_family_own"))) %>%
      pivot_wider(names_from = c("female"), values_from = estimate) %>%
      mutate(diff = Men - Women) %>%
      gather(key, value, -c(model_name, cohort)) %>%
      pivot_wider(names_from = c("cohort", "key"), values_from = value) %>%
      mutate(spec = paste("Spec", i)) %>%  # Unique identifier for each parameter combination
      select(spec, everything()) %>%
      arrange(model_name)
    
    # Add parameter values as columns
    formatted_results <- cbind(formatted_results, params)
    
    # Store in results list
    results_list[[i]] <- formatted_results
  }
  
  # Combine all results
  final_results <- bind_rows(results_list)
  
  return(final_results)
}

# Example usage:
# Create a parameter grid using expand.grid for all combinations
 parameter_grid <- expand.grid(
   parental_age_range = list(10:18, 12:17),
   offspring_age_range = list(30:45, 25:30, 30:35, 35:40, 40:45),
   cohorts = list(
     list("1950-1969" = 1950:1969, "1970-1985" = 1970:1985),
     list("1950-1960" = 1950:1960, "1970-1980" = 1970:1980),
     list("1950-1960" = 1960:1970, "1970-1980" = 1970:1980)
   ),
   familyid_vector = list(1:3000, c(1:3000, 5001:6872)),  
   minobs_childhood = 1:3,
   minobs_adulthood = 1:3,
   outcome = "observed",
   refdist = c("gender", "pooled")
 )
# 
results <- run_parameter_analysis(parameter_grid)

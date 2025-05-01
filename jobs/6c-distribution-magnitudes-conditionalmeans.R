# Rank-Based Analysis of Gender Differences in Intergenerational Mobility
# This approach models own income and family income as functions of parental income,
# separately by gender, using a rank-based methodology

# Load required packages
library(dplyr)
library(ggplot2)
library(tidyr)
library(splines) # For flexible modeling with splines

# Function to prepare data and calculate ranks
prepare_and_rank_data <- function(data) {
  # Ensure gender is properly formatted
  data <- data %>%
    mutate(gender = ifelse(female == 0, "Men", "Women")) 
  
  # Calculate ranks within the full sample
  data <- data %>%
    mutate(
      own_income_rank = offspring_own_income_pct_rank,
      family_income_rank = offspring_hdsp_income_pct_rank,
      parental_income_rank = parent_hdsp_income_pct_rank, # Already in rank form based on your data
    )
  
  return(data)
}

# Function to fit rank-rank models separately by gender
fit_models <- function(data, outcome_mod = "abs") {
  
  # Fit models
  # Use splines for flexible non-linear relationships
  if(outcome_mod == "rank"){
    own_model <- lm(own_income_rank ~ splines::ns(parental_income_rank, knots = c(25, 75)):gender, data = data)
    family_model <- lm(family_income_rank ~ splines::ns(parental_income_rank, knots = c(25, 75)):gender, data = data)
    family_model_adj <- lm(family_income_rank ~ splines::ns(parental_income_rank, knots = c(25, 75)):gender, data = data)
  }else{
    own_model <- lm(log(own_income+5) ~ splines::ns(parental_income_rank, knots = c(25, 75)):gender, data = data)
    family_model <- lm(log(family_income+5) ~ splines::ns(parental_income_rank, knots = c(25, 75)):gender, data = data)
    family_model_adj <- lm(log(family_income+5) ~ splines::ns(parental_income_rank, knots = c(25, 75)):gender, data = data)
    
  }
  # Return all models
  return(list(
    own = own_model,
    family = family_model
  ))
}

# Function to predict ranks across parental income distribution
predict_mobility <- function(models) {

  # Combine predictions
  predictions <- data %>%
    mutate(family_pred = predict(models$family, newdata = data),
           own_pred = predict(models$own, newdata = data))
  
  return(predictions)
}

# Function to predict ranks across parental income distribution
predict_mobility_grid <- function(models) {
  
  grid <- data_frame(
    parental_income_rank = c(seq(1, 100, 1), seq(1,100,1)),
    gender = c(rep("Men", 100), rep("Women", 100)))
  
  parent_income_quantile <- function(p) {quantile(data$parent_hdsp_income, probs = p/100, na.rm = TRUE)}
  
  # Combine predictions
  predictions_grid <- grid %>%
    mutate(family_pred = predict(models$family, newdata = grid),
           own_pred = predict(models$own, newdata = grid), 
           parent_hdsp_income = sapply(parental_income_rank, parent_income_quantile))

  
  return(predictions_grid)
}

#Function to convert ranks back to absolute values
convert_to_absolute <- function(data, predictions, outcome_mod = "abs") {
  # Create quantile functions for income variables
  own_income_quantile <- function(p) {quantile(data$own_income, probs = p/100, na.rm = TRUE)}
  family_income_quantile <- function(p) {
     #Family income 
    quantile(data$hdsp_income, probs = p/100, na.rm = TRUE)
  }

  # Apply quantile functions to convert ranks to absolute values
      if(outcome_mod == "rank"){
        predictions_convert <- predictions %>%
          mutate(
      own_pred = sapply(own_pred, own_income_quantile),
      family_pred = sapply(family_pred, family_income_quantile))
      }else{
        predictions_convert <- predictions %>% 
          mutate(
        own_pred = exp(own_pred) -5,
        family_pred = exp(family_pred) - 5
          )
      }
  
  return(predictions_convert)
}

# Function to calculate mobility distances
calculate_distances <- function(predictions) {
  # Calculate distances between own/family income and parental income
  distances <- predictions_convert %>%
    mutate(own_distance.pred = own_pred - parent_hdsp_income, 
           family_distance.pred = family_pred - parent_hdsp_income, 
           partner_contribution.pred = family_distance.pred - own_distance.pred, 
           own_distance.obs = own_income - parent_hdsp_income, 
           family_distance.obs = hdsp_income - parent_hdsp_income, 
           partner_contribution.obs = spouse_income)
  
  return(distances)
}

# Function to reshape data for plotting
reshape_for_plotting <- function(distances) {
  # Reshape to long format for ggplot
  plot_data <- distances %>%
    select(gender, 
      parental_income_rank, parent_hdsp_income, 
      ends_with("distance.pred"), ends_with("distance.obs"), 
      partner_contribution.pred, partner_contribution.obs, 
      own_income.pred = own_pred, own_income.obs = own_income
    ) %>%
    gather(key, value, -c(gender,parental_income_rank, parent_hdsp_income)) %>%
    separate_wider_delim(key, delim = ".", names = c("key", "type"))
  
  plot_data_grid <- distances_grid %>%
    select(gender, 
           parental_income_rank, parent_hdsp_income, 
           ends_with("distance.pred"), ends_with("distance.obs"), 
           partner_contribution.pred, 
           own_income.pred = own_pred
    ) %>%
    gather(key, value, -c(gender,parental_income_rank, parent_hdsp_income)) %>%
    separate_wider_delim(key, delim = ".", names = c("key", "type"))
  
  return(plot_data)
}

# Function to create income mobility plots
plot_mobility_distances <- function(plot_data) {
  # 1. Main plot for own and family income by gender
  main_plot <- ggplot() +
    geom_point(aes(x = parental_income_rank, y = value/1000, 
                  color = key, shape = key),
               data = plot_data %>% 
                 filter(type == "obs", 
                        key %in% c("family_distance","own_distance"),
                        parental_income_rank < 98) %>%
                 mutate(key = ifelse(key == "family_distance", "Family Income", 
                                     "Personal Earnings")), alpha = .2) +
    geom_line(aes(x = parental_income_rank, y = value/1000, 
                  color = key, linetype = key), linewidth = 1, 
              data = plot_data %>%
                filter(type == "pred", 
                       key %in% c("family_distance","own_distance"),
                       parental_income_rank < 98) %>%
                mutate(key = ifelse(key == "family_distance", "Family Income", 
                                    "Personal Earnings"))
              ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("Personal Earnings" = "dodgerblue", "Family Income" = "tomato3")) +
    scale_linetype_manual(values = c("Family Income" = "solid", "Personal Earnings" = "dashed")) +
    labs(
      x = "Parental Income Rank",
      y = "Child Income - Parental Income (Thousands)",
      title = "Upward Mobility Magnitudes by Gender Across the Distribution",
      subtitle = "Individual and Family Labor Income, Log Specification",
      color = "Gender",
      linetype = "Income Type"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_blank(),
      plot.title = element_text(size = 14, hjust = .5, face = "bold"),
      plot.subtitle = element_text(size = 12, hjust = .5),
      strip.text = element_text(size = 12),
      legend.text=element_text(size = 12),
      plot.caption = element_text(face = "italic"),
      axis.text.x =element_text(size = 8, angle = 45),
      axis.text.y =element_text(size = 8)
    ) +
    facet_grid(cols = vars(gender)) +
    ylim(-200, 200) +
    guides(shape = "none", linetype = "none")
  
  # 2. Plot for partner contribution by gender
  income_plot <- ggplot() +
    geom_point(aes(x = parental_income_rank, y = value/1000, 
                   color = key),
               data = plot_data %>% 
                 filter(type == "obs", key %in% c("partner_contribution", "own_income"),
                        parental_income_rank < 98) %>%
                 mutate(key = ifelse(key == "own_income", "Personal Earnings", "Partner Earnings")), alpha = .2) +
    geom_smooth(aes(x = parental_income_rank, y = value/1000, 
                   color = key), linetype = "dashed",
               data = plot_data %>% 
                 filter(type == "obs", key %in% c("partner_contribution", "own_income"),
                        parental_income_rank < 98) %>%
                 mutate(key = ifelse(key == "own_income", "Personal Earnings", "Partner Earnings")), alpha = .2) +
    geom_line(aes(x = parental_income_rank, y = value/1000, 
                  color = key), linewidth = 1, 
              data = plot_data %>% 
                filter(type == "pred", key %in% c("partner_contribution", "own_income"),
                       parental_income_rank < 98) %>%
                mutate(key = ifelse(key == "own_income", "Personal Earnings", "Partner Earnings")), 
    ) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c("Personal Earnings" = "dodgerblue", "Partner Earnings" = "orange2")) +
    labs(
      x = "Parental Income Rank",
      y = "Labor Earnings (Thousands)",
      title = "Personal and Partner Earnings Contributions by Gender Across the Distribution",
      color = "Gender"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_blank(),
      plot.title = element_text(size = 14, hjust = .5, face = "bold"),
      plot.subtitle = element_text(size = 12, hjust = .5),
      strip.text = element_text(size = 12),
      legend.text=element_text(size = 12),
      plot.caption = element_text(face = "italic"),
      axis.text.x =element_text(size = 8, angle = 45),
      axis.text.y =element_text(size = 8)
    ) +
    facet_grid(cols = vars(gender)) +
    guides(shape = "none") + 
    ylim(0, 200) 
  
  # 3. Direct comparison plot (gender gaps by income type)
  gender_gap_plot <- ggplot() + 
    geom_point(data = plot_data %>% mutate(
      parent_ptile = as.numeric(as.character(
        factor(cut(parental_income_rank,
                   breaks = seq(0, 100, by = 1),
                   include.lowest = TRUE,
                   right = FALSE),
               labels = seq(1, 100, by = 1))))) %>%
        select(-c(parental_income_rank, parent_hdsp_income)) %>%
        group_by(gender, parent_ptile, key, type) %>%
        summarise(value = mean(value)) %>%
        filter(type == "obs", key %in% c("own_distance", "family_distance", "partner_contribution")) %>%
        mutate(key = case_when(key == "own_distance"~ "Personal Earnings", 
                               key == "family_distance" ~ "Family Income", 
                               key == "partner_contribution" ~ "Partner Earnings")) %>%
        pivot_wider(names_from = c(gender), values_from = c(value)) %>%
        mutate(diff = (Men - Women)/1000), 
      aes(x = parent_ptile, shape = key, color = key, y = diff), 
      alpha = .5) +
    geom_line(aes(x = parental_income_rank, y = gender_gap/1000, color = key), size = 1,
              plot_data_grid %>%
                filter(key %in% c("own_distance", "family_distance", "partner_contribution")) %>%
                pivot_wider(names_from = gender, values_from = value) %>%
                mutate(gender_gap = Men-Women, 
                       key = case_when(key == "own_distance"~ "Personal Earnings", 
                                       key == "family_distance" ~ "Family Income", 
                                       key == "partner_contribution" ~ "Partner Earnings"))) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_manual(values = c(
      "Personal Earnings" = "dodgerblue", 
      "Family Income" = "tomato3", 
      "Partner Earnings" = "orange2"
    )) +
    labs(
      x = "Parental Income Rank",
      y = "Mean Differences (Men - Women)",
      title = "Gender Gaps in Mobility Magnitude Components Across the Distibution",
      color = ""
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.title = element_blank(),
      plot.title = element_text(size = 14, hjust = .5, face = "bold"),
      plot.subtitle = element_text(size = 12, hjust = .5),
      strip.text = element_text(size = 12),
      legend.text=element_text(size = 12),
      plot.caption = element_text(face = "italic"),
      axis.text.x =element_text(size = 8, angle = 45),
      axis.text.y =element_text(size = 8)
    ) +
    ylim(-100, 100) +
    guides(shape = "none")
  
  # Return all plots
  return(list(
    main_plot = main_plot,
    own_plot = own_plot,
    partner_plot = partner_plot,
    gender_gap_plot = gender_gap_plot
  ))
}

# Main analysis function
run_rank_based_analysis <- function(data) {
  # 1. Prepare data and calculate ranks
  cat("Preparing data and calculating ranks...\n")
  ranked_data <- prepare_and_rank_data(data)
  
  # 2. Fit rank-rank models
  cat("Fitting rank-rank models...\n")
  models <- fit_rank_models(ranked_data)
  
  # 3. Predict across parental income distribution
  cat("Predicting across parental income distribution...\n")
  predictions <- predict_mobility_ranks(models)
  
  # 4. Convert ranks back to absolute values
  cat("Converting ranks to absolute values...\n")
  abs_predictions <- convert_ranks_to_absolute(ranked_data, predictions)
  
  # 5. Calculate mobility distances
  cat("Calculating mobility distances...\n")
  distances <- calculate_distances(abs_predictions)
  
  # 6. Reshape for plotting
  cat("Preparing data for plotting...\n")
  plot_data <- reshape_for_plotting(distances)
  
  # 7. Create plots
  cat("Creating plots...\n")
  plots <- plot_mobility_distances(plot_data)
  
  # 8. Return results
  return(list(
    models = models,
    predictions = predictions,
    abs_predictions = abs_predictions,
    distances = distances,
    plot_data = plot_data,
    plots = plots
  ))
}

# Example usage with your data
results <- run_rank_based_analysis(
  gen_data(cohorts = list("1970-1985" = 1970:1985))$offspring_perm)
# 
# # View plots
# print(results$plots$main_plot)
# print(results$plots$partner_plot)
# print(results$plots$gender_gap_plot)
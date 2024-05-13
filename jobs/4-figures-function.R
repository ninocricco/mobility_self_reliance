#------------------------------------------------------------------------------
# PROJECT: MOBILITY SELF-RELIANCE
# FILE: FUNCTION TO GENERATE MAIN FIGURES
# AUTHOR: NINO CRICCO
# CONTACT: ncricco@g.harvard.edu
#------------------------------------------------------------------------------

source("jobs/3-tables-function.R")

#------------------------------------------------------------------------------
# FIGURE 1: CHANGES IN MOBILITY SELF-RELIANCE ACROSS COHORTS, BY GENDER
#------------------------------------------------------------------------------

generate_fig1 <- function(data = data, object = "fig"){
  
  fig1_values <- generate_change_table(data) %>%
    pivot_wider(names_from = model_name, values_from = value) %>%
    transmute(Gender, key, 
              mob_sr_ratio = own_parent * family_own/family_parent)
  
  fig1 <- fig1_values %>%
    # Change this line so that it's not hard coded- use regex to extract
    transmute(cohort = gsub("cohort_(\\d{4})_(\\d{4})", "\\1-\\2", key), 
      Gender, mob_sr_ratio) %>%
    ggplot(aes(x = cohort, y = mob_sr_ratio)) +
    geom_col(position = "identity", fill = "gray", color = "black") +
    theme_minimal() +
    labs(y = "Mobility Self-Reliance Ratio", x = "Birth Cohort", 
         title = "Changes in Mobility Self-Reliance by Cohort") +
    theme(plot.title = element_text(hjust = .5)) +
    facet_wrap(~Gender) +
    ylim(0,1)
  
  if(object == "values"){
    return(fig1_values)
  }
  else{
    return(fig1)
  }
  
}

#------------------------------------------------------------------------------
# FIG 2: All Estimated Parameters by Gender and cohort
#------------------------------------------------------------------------------

generate_fig2 <- function(data = data, object = "fig"){
  
  fig2 <- estimate_parameters(data) %>%
    filter(term != "(Intercept)") %>%
    filter(model_name %in% c("own_parent", "family_own", 
                             "family_parent", "residual_family_own")) %>%
    mutate(Gender = factor(ifelse(female == 1, "Women", "Men"), 
                           levels = c("Women", "Men")), 
           model_name = factor(case_when(
             model_name == "own_parent" ~ "Earnings Persistence",
             model_name == "family_own" ~ "Earnings-Income Correlation",
             model_name == "family_parent" ~ "Income Persistence",
             model_name == "residual_family_own" ~ "Residual Transmission"),
             levels = c("Income Persistence", "Earnings Persistence", 
                        "Earnings-Income Correlation", "Residual Transmission"))) %>%
    ggplot(aes(x = model_name, y = estimate, shape = cohort, color = Gender)) +
    geom_point(size = 2, position = position_dodge(width = .6)) +                  
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),     
                  width = 0.2, position = position_dodge(width = .6)) +
    theme_minimal() +
    labs(title = "Linear Parameter Estimates by Gender and Cohort",
         y = "Estimate of the Rank-Rank Slope", x = "Model") +
    scale_color_manual(values = c("goldenrod", "navyblue")) +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 20, hjust = 1),
          plot.title = element_text(hjust = 0.5)) +
    ylim(0, 1)
  
  return(fig2)
  
}

#------------------------------------------------------------------------------
# FIGURE 3: CHANGING PATHWAYS TO FAMILY INCOME PERSISTENCE
#------------------------------------------------------------------------------

generate_fig3 <- function(data = data, object = "fig"){
  
  fig3_values <- generate_change_table(data) %>%
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
    mutate(share_type = factor(case_when(
             share_type == "residual_family_own" ~ "Residual Transmission",
             share_type == "share_family_own" ~ "Earnings-Income Correlation",
             share_type == "share_own_parent" ~ "Earnings Persistence"), 
             levels = c("Residual Transmission",
                        "Earnings Persistence", "Earnings-Income Correlation"))) %>%
    ggplot(aes(x = key, y = value, fill = share_type)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(
      title = "Changing Pathways towards Income Persistence",
      y = "Rank-Rank Slope, Family Labor Income", x = "") +
    scale_fill_manual(values = c("gray59", "grey38", "gray20"))+
    facet_wrap(~Gender) +
    theme_minimal() +
    theme(legend.title = element_blank(),
          plot.title = element_text(hjust = .5),
          #legend.position = c(.9, .9),
          #legend.justification = c("right", "top"),
          legend.position = "bottom")  +
    ylim(0, .75)
  
  if(object == "values"){
    return(fig3_values)
  }
  else{
    return(fig3)
  }
  
}

generate_fig4 <- function(data = data, gender = 1){
  
  fig4 <-generate_decomp_marstat(data) %>%
    gather(key, value, -c(female, model_name, ever_married)) %>% 
    filter(female == gender) %>%
    filter(key %in% c(
      "between_change_pct", "within_change_pct", "prop_change_pct")) %>% 
    mutate(
      param = case_when(model_name == "own_parent" ~ 
                          "Earnings Persistence",
                        model_name == "family_own" ~ 
                          "Earnings-Income Correlation",
                        model_name == "residual_family_own" ~ 
                          "Residual Transmission"),
      key = case_when(key == "between_change_pct" ~ "Between-Group",
                      key == "within_change_pct" ~ "Within-Group",
                      key == "prop_change_pct" ~ "Share")
    ) %>%
    ggplot(aes(x = ever_married, y = value, fill = key)) + 
    geom_bar(stat = "identity", position = "dodge") + 
    facet_grid(rows = vars(female), cols = vars(param)) +
    scale_fill_manual(values = c("gray59", "grey38", "gray20"))+
    theme_minimal() +
    labs(title = paste0(
      "Decomposing Changes in Parameters by Marital Status, ", 
      ifelse(gender == 1, "Women", "Men")),
         x = "", y = "% Explained") +
    theme(#legend.position = c(.4, .9),
          #legend.justification = c("right", "top"),
          legend.position = "bottom",
          axis.text.x = element_text(angle = 20, hjust = 1),
          plot.title = element_text(hjust = .5), 
          legend.title = element_blank())
  
  return(fig4)
  
}

generate_figures <- function(data, caption = ""){
  
  return(grid.arrange(generate_fig1(data = data), generate_fig2(data = data),
                      generate_fig3(data = data), generate_fig4(data = data),
                      nrow = 2, top = caption))
  
}

# Spline figures
generate_fig5 <- function(data, object = "fig"){
  
  # Creating a nested dataframe by cohort and gender
  nested_data <- data %>%
    group_by(cohort, female) %>%
    nest()
  
  # Defining formulas for each model
  formulas <- list(
    family_own_spline = offspring_hdsp_income_pct_rank ~ splines::ns(
      offspring_gender_own_income_pct_rank, knots = c(25, 5, 75)),
    own_parent_spline = offspring_gender_own_income_pct_rank ~ splines::ns(
      parent_hdsp_income_pct_rank, knots = c(25, 5, 75))
  )
  
  # Create a function that creates tidy outputs from model results
  fit_models <- function(data_to_fit, formula) {
    lm(formula, data = data_to_fit)
  }
  
  # Apply initial models
  initial_models <- map(formulas, ~ nested_data %>%
                          mutate(fitted_models = map(
                            data, fit_models, .x))) %>%
    imap(., ~ mutate(.x, model_name = .y))
  
  
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
  
  # Apply processing function to model list
  combined_results <- map_df(initial_models, ~ {
    # Group by cohort and female to handle unique combinations within each tibble
    grouped_df <- group_by(.x, cohort, female)
    
    # Use group_map to apply process_models to each subgroup
    processed_models <- group_map(grouped_df, process_models, .keep = TRUE) %>% 
      bind_rows()
    
    return(processed_models)
  })
  
  
  fig5 <- combined_results %>%
    filter(str_starts(term, regex("splines::ns", ignore_case = TRUE))) %>%
    mutate(pred_spline = str_sub(term, -1),
           Predictor_Quintile_Range = factor(
             case_when(pred_spline == 1 ~ "0-25th %",
                       pred_spline == 2 ~ "25th-50th %",
                       pred_spline == 3 ~ "50th-75th %",
                       pred_spline == 4 ~ "> 75th %",),
             levels = c("0-25th %", "25th-50th %","50th-75th %", "> 75th %")),
           model_name = case_when(
             str_detect(
               term, "ns\\(parent_hdsp_income") ~ 
               "Earnings Persistence",
             str_detect(
               term, "ns\\(offspring_gender_own") ~
               "Earnings-Income Correlation"),
           Gender = ifelse(female == 1, "Women", "Men")) %>%
    select(Predictor_Quintile_Range, everything(), -c(pred_spline)) %>%
    ggplot(aes(x = Predictor_Quintile_Range, y = estimate, shape = Gender,
               color = Gender)) +
    geom_point(size = 2, position = position_dodge(width = 0.2)) +                  
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),     
                  width = 0.2, position = position_dodge(width = 0.2)) +
    facet_grid(rows = vars(model_name), cols = vars(cohort)) +
    theme_minimal() +
    scale_color_manual(values = c("navyblue", "goldenrod")) +
    labs(x = "Predictor Rank Knot", y = "", 
         title = "Non-linearities in Parameters: Spline Models") +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = .5))
  
  if(object == "values"){
    return(combined_results)
  }
  else{
    return(fig5)
  }
  
}


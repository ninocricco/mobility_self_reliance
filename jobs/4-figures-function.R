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

generate_fig1 <- function(data = data, bs_cis = F, 
                          n_bootstrap = 10000, conf = .95) {
  
  # Get unique cohort values
  cohort_values <- sort(unique(data$cohort))
  
  # Define color palette
  color_palette <- c("steelblue2", "orange1",
                     "navyblue", "orange4")
  
  if(bs_cis) {
    df_percentiles <- estimate_ci_share_bs(data, n_bootstrap = n_bootstrap, conf = conf)
    
    fig1_values <- estimate_share(data) %>%
      left_join(df_percentiles %>% 
                  as.data.frame() %>% 
                  mutate(quantile = rownames(.)) %>% 
                  gather(key, value, -quantile) %>%
                  separate(key, into = c("Gender", "key"), sep = "_"), 
                by = c("Gender", "key")) %>%
      pivot_wider(names_from = quantile, values_from = value)
      
  } else {
    fig1_values <- estimate_share(data)
  }
  
  # Create plot data with consistent group formatting
  plot_data <- fig1_values %>%
    transmute(
      cohort = gsub("cohort_(\\d{4})_(\\d{4})", "\\1-\\2", key),
      Gender = factor(Gender, levels = c("Men", "Women")),
      share_earnings = share_earnings,
      group = factor(paste(Gender, cohort),
                     levels = c(paste("Men", cohort_values[1]),
                                paste("Women", cohort_values[1]), 
                                paste("Men", cohort_values[2]),
                                paste("Women", cohort_values[2]))),
      min = if(bs_cis) min else 0,
      max = if(bs_cis) max else 0)
  
  # Create base plot
  fig1 <- plot_data %>%
    ggplot(aes(x = group, y = share_earnings, fill = group)) +
    geom_col(position = "identity", color = "black") +
    geom_errorbar(
      aes(ymin = min, ymax = max),
      width = 0.2,
      position = position_identity()
    ) +
    geom_text(aes(label = round(share_earnings, 2)), 
              vjust = -0.5, 
              hjust = -0.05,
              size = 3.5) +
    theme_bw() +
    labs(
      title = "Figure 1: Family Income Persistence, % Explained by Earnings",
      y = "Percent", 
      x = "Birth Cohort",
      fill = "Gender & Cohort",
      caption = "Data from the Panel Study of Income Dynamics. \n Error bras show 95 % bootstrapped confidence intervals."
    ) +
    scale_fill_manual(values = color_palette) +
    theme(
      legend.position = "none",
      legend.justification = c(0, 1),
      legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white"),
      plot.title = element_text(hjust = 0.5),
      legend.title = element_text(""),
      legend.margin = margin(6, 6, 6, 6)
    ) +
    ylim(0, 1)
  
  
  return(fig1)
}

#------------------------------------------------------------------------------
# FIG 2: All Estimated Parameters by Gender and cohort
#------------------------------------------------------------------------------

generate_fig2a <- function(data = data, object = "fig", by_marstat = F){
  
  # Define new labels for x-axis
  labels <- c(
    "family_parent" = "Parent - Child Income",
    "own_parent" = "Parent - Child Earnings",
    "family_own" = "Child Earnings - Child Income",
    "family_own_residuals" = "Parent - Child Income | Earnings"
  )
  
  # Get unique cohort values
  cohort_values <- sort(unique(data$cohort))
  
  # Define color palette
  color_palette <- c("steelblue2", "orange1",
                     "navyblue", "orange4") 
  
  if(by_marstat == F){
    est_param <- estimate_parameters(data)
  } else {
    est_param <- estimate_parameters(data, by_marstat = T)
  }
  
  est_param <- est_param %>%
    filter(term != "(Intercept)", model_name != "spouse_own") %>%
    mutate(
      Gender = factor(ifelse(female == 1, "Women", "Men"), 
                      levels = c("Women", "Men")),
      group = factor(paste(Gender, cohort),
                     levels = c(paste("Men", cohort_values[1]),
                                paste("Women", cohort_values[1]), 
                                paste("Men", cohort_values[2]),
                                paste("Women", cohort_values[2]))),
      model_name = factor(model_name, 
                          levels = c("family_parent",
                                     "own_parent",
                                     "family_own",
                                     "family_own_residuals")))
  
  if(by_marstat == F){
    fig2 <- est_param %>%
      ggplot(aes(x = model_name, y = estimate, color = group)) +
      geom_point(size = 2, position = position_dodge(width = .6)) +                  
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high),     
                    width = 0.2, position = position_dodge(width = .6)) +
      theme_bw() +
      labs(title = "Figure 2, Panel A",
      subtitle = "Linear Parameter Estimates by Gender and Cohort",
           y = "Rank-Rank Slope", 
           x = "", 
           color = "Gender & Cohort",
           caption = "") +
      scale_color_manual(values = color_palette) +
      scale_x_discrete(labels = labels) + 
      ylim(0, 1) +
      theme(
        legend.position = c(0.02, 0.98),  # Position in top left
        legend.justification = c(0, 1),    # Align to top left
        legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_text(""),
        legend.margin = margin(6, 6, 6, 6),
        axis.text.x = element_text(angle = 20, hjust = 1)
      ) +
      guides(
        color = guide_legend(override.aes = list(size = 2))
      )
    
  } else {
    pd <- position_dodge(width = 0.6)
    fig2 <- est_param %>%
      mutate(ever_married = ifelse(ever_married == 1, "Ever Married", "Never Married")) %>%
      ggplot(aes(x = model_name, y = estimate, color = group)) +
      geom_point(
        aes(y = estimate),
        size = 2, 
        position = pd
      ) +                  
      geom_errorbar(
        aes(y = estimate, ymin = conf.low, ymax = conf.high),
        width = 0.2, 
        position = pd
      ) +
      theme_bw() +
      facet_wrap(~ever_married) +
      labs(title = "Figure 2, Panel A",
           subtitle = "Linear Parameter Estimates by Gender, Cohort, and Marital Status",
           y = "Rank-Rank Slope", 
           x = "", 
           color = "Gender & Cohort",
           caption = "") +
      scale_color_manual(values = color_palette) +
      scale_x_discrete(labels = labels) + 
      theme(
        legend.position = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = "white"),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_text(""),
        legend.margin = margin(6, 6, 6, 6),
        axis.text.x = element_text(angle = 20, hjust = 1)
      ) +
      guides(
        color = guide_legend(override.aes = list(size = 2))
      )
  }
  
  return(fig2)
}

generate_fig2b  <- function(test_results) {
  # Create simpler labels for the models
  model_labels <- c(
    "family_parent" = "Parent - Child Income",
    "own_parent" = "Parent - Child Earnings",
    "family_own" = "Child Earnings - Child Income",
    "spouse_own" = "Child Earnings - Spouse Earnings",
    "family_own_residuals" = "Parent - Child Income | Earnings"
  )
  
  # Create combined labels for plotting
  plot_data <- test_results %>%
    filter(model %!in% c("spouse_own")) %>%
    filter(str_detect(term, ":")) %>%
    mutate(
      # Create more descriptive model names
      model = factor(model, 
                     levels = names(model_labels),
                     labels = model_labels)
    )
  
  # Create the plot
  ggplot(plot_data, 
         aes(x = model, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                   height = 0.2) +
    facet_wrap(~test_type, scales = "free_x", nrow = 1) +
    theme_bw() +
    labs(title = "Figure 2, Panel B",
         subtitle = "Within-Gender Cohort Changes and Within-Cohort Gender Differences",
         caption = "Data from the Panel Study of Income Dynamics. \nError bars show 95 % confidence intervals.",
         x = "",
         y = "") +
    scale_color_manual(values = color_palette) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(hjust = 0),
      strip.text = element_text(size = 10),
      axis.text.x = element_text(angle = 20, hjust = 1),
      strip.background = element_rect(fill = "white")
    )
}

#------------------------------------------------------------------------------
# FIGURE 3: VISUALIZING RELATIONSHIPS
#------------------------------------------------------------------------------

generate_fig3 <- function(data, 
                          x_var = "parent_hdsp_income_gender_pct_rank",
                          y_var = "offspring_own_income_gender_pct_rank",
                          method = "lm",
                          x_label = NULL,
                          y_label = NULL, 
                          title = NULL) {
  
  # Format variable names for title if not provided
  if (is.null(x_label)) {
    x_label <- stringr::str_to_title(gsub("_", " ", x_var))
  }
  if (is.null(y_label)) {
    y_label <- stringr::str_to_title(gsub("_", " ", y_var))
  }
  if (is.null(title)) {
    title <- paste(y_label, "vs", x_label, method)
  }
  
  # Get unique cohort values
  cohort_values <- sort(unique(data$cohort))
  
  # Create interaction variable for colors using actual cohort values
  plot_data <- data %>%
    mutate(
      Gender = factor(ifelse(female == 1, "Women", "Men"), 
                      levels = c("Women", "Men")),
      group = factor(paste(Gender, cohort),
                     levels = c(paste("Men", cohort_values[1]),
                                paste("Women", cohort_values[1]), 
                                paste("Men", cohort_values[2]),
                                paste("Women", cohort_values[2])))
    )
  
  # Define color palette
  color_palette <- c("steelblue2", "orange1",
                     "navyblue", "orange4") 
  
  # Create the plot
  fig3 <- plot_data %>% 
    ggplot(aes(x = .data[[x_var]], 
               y = .data[[y_var]], 
               color = group,
               linetype = cohort)) +
    #geom_jitter(alpha = 0.2, size = 0.8) +
    geom_smooth(method = method, 
                aes(fill = group),
                alpha = 0.2) +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      color = "Gender & Cohort", 
      caption = "Data from the Panel Study of Income Dynamics."
    ) +
    theme_bw() +
    scale_color_manual(values = color_palette) +
    scale_fill_manual(values = color_palette) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    #facet_wrap(~Gender) +
    theme(
      legend.position = c(0.02, 0.98),  # Position in top left
      legend.justification = c(0, 1),    # Align to top left
      legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
      legend.background = element_rect(fill = "white"), 
      legend.key = element_rect(fill = "white"),  
      plot.title = element_text(hjust = 0.5),
      legend.title = element_text(""),
      legend.margin = margin(6, 6, 6, 6)
    ) +
    guides(
      color = guide_legend(override.aes = list(alpha = 1, size = 2)),
      fill = "none",
      linetype = "none"  # Remove linetype from legend
    )
  
  return(fig3)
}

#------------------------------------------------------------------------------
# FIGURE 4: DECOMPOSING FAMILY INCOME PERSISTENCE
#------------------------------------------------------------------------------

generate_fig4 <- function(data = data, object = "fig") {
  
  # Define color palette - using similar tones but distinct for stacked bars
  color_palette <- c("orange1", "steelblue2", "navy")
  
  fig4_values <- generate_change_table(data) %>%
    filter(key != "change") %>%
    pivot_wider(names_from = model_name, values_from = value) %>%
    mutate(through_income = own_parent * family_own, 
           sum_through_income = own_parent + family_own) %>%
    select(Gender, key, own_parent, family_own, 
           through_income, sum_through_income, family_own_residuals) %>%
    gather(parameter, value, -c(
      Gender, key, sum_through_income, own_parent, family_own)) %>%
    mutate(
      share_own_parent = case_when(
        parameter == "family_own_residuals" ~ 1,
        parameter == "through_income" ~ own_parent/sum_through_income),
      share_family_own = case_when(
        parameter == "family_own_residuals" ~ 1,
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
  
  fig4 <- fig4_values %>%
    mutate(
      share_type = factor(case_when(
        share_type == "family_own_residuals" ~ "Parent - Child Income | Earnings",
        share_type == "share_family_own" ~ "Child Earnings - Child Income",
        share_type == "share_own_parent" ~ "Parent- Child Earnigns"), 
        levels = c("Parent - Child Income | Earnings",
                   "Parent- Child Earnigns", 
                   "Child Earnings - Child Income")),
      Gender = factor(Gender, levels = c("Men", "Women"))
    ) %>%
    ggplot(aes(x = key, y = value, fill = share_type)) +
    geom_bar(stat = "identity", color = "black", linewidth = 0.2) +
    facet_wrap(~Gender) +
    theme_bw() +
    labs(
      title = "Figure 4: Decomposing Intergenerational Income Persistence by Gender and Cohort",
      y = "Rank-Rank Slope, Family Income", 
      x = "",
      fill = "",
      caption = "Data from the Panel Study of Income Dynamics."
    ) +
    scale_fill_manual(values = color_palette) +
    theme(
      legend.position = c(0.02, 0.98),
      legend.justification = c(0, 1),
      legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white"),
      plot.title = element_text(hjust = 0.5),
      legend.title = element_text(""),
      legend.margin = margin(6, 6, 6, 6),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major.x = element_blank()
    ) +
    ylim(0, 0.75)
  
  if(object == "values") {
    return(fig4_values)
  } else {
    return(fig4)
  }
}

#------------------------------------------------------------------------------
# FIGURE 5: POTENTIAL MECHANISMS
#------------------------------------------------------------------------------

generate_fig5 <- function(data = data, method = method) {
  # Get unique cohort values
  cohort_values <- sort(unique(data$cohort))
  
  # Define color palette
  color_palette <- c("steelblue2", "orange1",
                     "navyblue", "orange4") 
  
  # Data preparation
  plot_data <- data %>%
    mutate(
      married_norm = married/num_observedinadulthood,
      ft_norm = ft/num_observedinadulthood,
      Gender = factor(ifelse(female == 1, "Women", "Men"), 
                      levels = c("Women", "Men")),
      group = factor(paste(Gender, cohort),
                     levels = c(paste("Men", cohort_values[1]),
                                paste("Women", cohort_values[1]), 
                                paste("Men", cohort_values[2]),
                                paste("Women", cohort_values[2])))
    )
  
  # Common theme elements
  plot_theme <- theme_bw() +
    theme(
      legend.position = c(0.3, 0.3),
      legend.justification = c(0, 1),
      legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
      legend.background = element_rect(fill = "white"),
      legend.key = element_rect(fill = "white"),
      plot.title = element_text(hjust = 0.5, size = 10),
      legend.title = element_text(""),
      legend.margin = margin(6, 6, 6, 6)
    )
  
  # Common scale elements
  plot_scales <- list(
    scale_color_manual(values = color_palette),
    scale_fill_manual(values = color_palette),
    scale_linetype_manual(values = c("solid", "dashed"))
  )
  
  # Common guide elements
  plot_guides <- guides(
    color = guide_legend(override.aes = list(alpha = 1, size = 2)),
    fill = "none",
    linetype = "none"
  )
  
  # Create the three plots
  fig5_a <- ggplot(plot_data, 
                   aes(x = parent_hdsp_income_gender_pct_rank, 
                       y = married_norm, 
                       color = group,
                       linetype = cohort)) +
    geom_smooth(method = method, aes(fill = group), alpha = 0.2) +
    labs(x = "Parent Income Rank",
         y = "% Years Partnered",
         title = "A: Partnership",
         color = "Gender & Cohort") +
    plot_theme +
    plot_scales +
    plot_guides
  
  fig5_b <- ggplot(plot_data, 
                   aes(x = parent_hdsp_income_gender_pct_rank, 
                       y = ft_norm, 
                       color = group,
                       linetype = cohort)) +
    geom_smooth(method = method, aes(fill = group), alpha = 0.2) +
    labs(x = "Parent Income Rank",
         y = "% Years Working Full-Time",
         title = "B: Labor Supply",
         color = "Gender & Cohort") +
    plot_theme +
    plot_scales +
    plot_guides +
    theme(legend.position = "") 
  
  # Arrange plots
  fig5 <- grid.arrange(
    fig5_a, fig5_b, 
    nrow = 1,
    top = textGrob(
      "Figure A1: Partnership and Labor Supply by Gender, Cohort, and Parent Income",
      gp = gpar(fontsize = 12)
    ),
    bottom = textGrob(
      "Data from the Panel Study of Income Dynamics. \nEstimates using Generalized Additive Models.",
      gp = gpar(fontsize = 10),
      hjust = 1,  # Right align
      x = 1       # Position at rightmost edge
    )
  )
  
  return(fig5)
}




#------------------------------------------------------------------------------
generate_figa3 <- function(data, 
                          x_var = "offspring_spouse_income_gender_pct_rank",
                          y_var = "ft_norm",
                          method = "lm",
                          x_label = NULL,
                          y_label = NULL, 
                          title = NULL) {
  
  # Format variable names for title if not provided
  if (is.null(x_label)) {
    x_label <- stringr::str_to_title(gsub("_", " ", x_var))
  }
  if (is.null(y_label)) {
    y_label <- stringr::str_to_title(gsub("_", " ", y_var))
  }
  if (is.null(title)) {
    title <- paste(y_label, "vs", x_label, method)
  }
  
  # Get unique cohort values
  cohort_values <- sort(unique(data$cohort))
  
  # Create interaction variable for colors using actual cohort values
  plot_data <- data %>%
    mutate(
      Gender = factor(ifelse(female == 1, "Women", "Men"), 
                      levels = c("Women", "Men")),
      ft_norm = ft/num_observedinadulthood,
      group = factor(paste(Gender, cohort),
                     levels = c(paste("Men", cohort_values[1]),
                                paste("Women", cohort_values[1]), 
                                paste("Men", cohort_values[2]),
                                paste("Women", cohort_values[2])))
    )
  
  # Define color palette
  color_palette <- c("orange1", "orange4") 
  
  # Create the plot
  figa3 <- plot_data %>% 
    filter(female == 1) %>%
    filter(offspring_spouse_income_gender_pct_rank > 75) %>%
    ggplot(aes(x = .data[[x_var]], 
               y = .data[[y_var]], 
               color = group,
               linetype = cohort)) +
    #geom_jitter(alpha = 0.2, size = 0.8) +
    geom_smooth(method = method, 
                aes(fill = group),
                alpha = 0.2) +
    labs(
      title = title,
      x = x_label,
      y = y_label,
      color = "Gender", 
      caption = "Data from the Panel Study of Income Dynamics. \n LOESS-smoothed estimates."
    ) +
    theme_bw() +
    scale_color_manual(values = color_palette) +
    scale_fill_manual(values = color_palette) +
    scale_linetype_manual(values = c("solid", "dashed")) +
    #facet_wrap(~Gender) +
    theme(
      legend.position = c(0.7, 0.98),  # Position in top left
      legend.justification = c(0, 1),    # Align to top left
      legend.box.background = element_rect(color = "black", fill = "white", linewidth = 0.5),
      legend.background = element_rect(fill = "white"), 
      legend.key = element_rect(fill = "white"),  
      plot.title = element_text(hjust = 0.5),
      legend.title = element_text(""),
      legend.margin = margin(6, 6, 6, 6)
    ) +
    guides(
      color = guide_legend(override.aes = list(alpha = 1, size = 2)),
      fill = "none",
      linetype = "none"  # Remove linetype from legend
    )
  
  return(figa3)
}

generate_fig5 <- function(data = data, object = "fig", method = "lm"){
  figmar <- data %>%
    mutate(ft_norm = ft/num_observedinadulthood) %>%
    ggplot(aes(x = offspring_spouse_income_pct_rank, y = ft_norm, 
               color = female, linetype = cohort)) +
    geom_jitter(alpha = .2) +
    geom_smooth(method = method) +
    theme_bw() +
    facet_wrap(~ever_married) +
    theme(legend.position = "bottom")
  return(figmar)
}

generate_fig5 <- function(data = data, object = "fig"){
  
  fig5_values <- generate_marstat_params(data) %>%
    select(female, cohort, ever_married, ends_with("mean"), -own_family_resid_mean) %>%
    gather(variable, mean, -c(female, cohort, ever_married)) %>%
    mutate(variable = case_when(variable == "offspring_gender_own_income_pct_rank_mean" ~ "Own Earnings Rank",
                                variable == "offspring_hdsp_income_pct_rank_mean" ~ "Family Income Rank",
                                variable == "parent_hdsp_income_pct_rank_mean" ~ "Parent Income Rank"))
  
  fig5 <- fig5_values %>%
    mutate(ever_married = factor(ifelse(ever_married == 1, "Ever Married", "Never Married")),
           female = ifelse(female == 1, "Men", "Women")) %>%
    ggplot(aes(x = cohort, y = mean, fill = ever_married)) +
    geom_col(position = "dodge", color = "black") +
    theme_bw() +
    labs(y = "Means", x = "Birth Cohort", 
         title = "Changes in Average Earnings and Income Ranks by Marital Status") +
    theme(plot.title = element_text(hjust = .5)) +
    facet_grid(rows = vars(female), cols = vars(variable)) +
    theme(legend.title = element_blank(),
          plot.title = element_text(hjust = .5),
          #legend.position = c(.9, .9),
          #legend.justification = c("right", "top"),
          legend.position = "bottom") 
  
  if(object == "values"){
    return(fig5_values)
  }
  else{
    return(fig5)
  }
  
    
}

generate_fig6 <- function(data = data, gender = 1){
  
  fig6 <-generate_decomp_marstat(data) %>%
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
    theme_bw() +
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
  
  return(fig6)
  
}

generate_figures <- function(data, caption = ""){
  
  return(grid.arrange(generate_fig1(data = data), generate_fig2(data = data),
                      generate_fig3(data = data), generate_fig6(data = data),
                      nrow = 2, top = caption))
  
}

# Spline figures
generate_fig7 <- function(data, object = "fig"){
  
  # Creating a nested dataframe by cohort and gender
  nested_data <- data %>%
    group_by(cohort, female) %>%
    nest()
  
  # Defining formulas for each model
  formulas <- list(
    family_own_spline = offspring_hdsp_income_pct_rank ~ splines::ns(
      offspring_gender_own_income_pct_rank, knots = c(25, 5, 75)),
    own_parent_spline = offspring_gender_own_income_pct_rank ~ splines::ns(
      parent_hdsp_income_pct_rank, knots = c(25, 5, 75)), 
    family_parent_spline = offspring_hdsp_income_pct_rank ~ splines::ns(
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
  
  
  fig7 <- combined_results %>%
    filter(str_starts(term, regex("splines::ns", ignore_case = TRUE))) %>%
    mutate(pred_spline = str_sub(term, -1),
           Predictor_Quintile_Range = factor(
             case_when(pred_spline == 1 ~ "0-25th %",
                       pred_spline == 2 ~ "25th-50th %",
                       pred_spline == 3 ~ "50th-75th %",
                       pred_spline == 4 ~ "> 75th %",),
             levels = c("0-25th %", "25th-50th %","50th-75th %", "> 75th %")),
           model_name = case_when(model_name == "family_own_spline" ~ "Earnings-Income Correlation",
                                  model_name == "family_parent_spline" ~ "Income Persistence",
                                  model_name == "own_parent_spline" ~ "Earnings Persistence"), 
           model_name = factor(model_name, levels = c("Income Persistence", 
                                                      "Earnings Persistence", 
                                                      "Earnings-Income Correlation")),
           Gender = ifelse(female == 1, "Women", "Men")) %>%
    select(Predictor_Quintile_Range, everything(), -c(pred_spline)) %>%
    ggplot(aes(x = Predictor_Quintile_Range, y = estimate, shape = Gender,
               color = Gender)) +
    geom_point(size = 2, position = position_dodge(width = 0.2)) +                  
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),     
                  width = 0.2, position = position_dodge(width = 0.2)) +
    facet_grid(rows = vars(model_name), cols = vars(cohort)) +
    theme_bw() +
    scale_color_manual(values = c("navyblue", "goldenrod")) +
    labs(x = "Predictor Rank Knot", y = "", 
         title = "Non-linearities in Parameters: Spline Models") +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = .5))
  
  if(object == "values"){
    return(combined_results)
  }
  else{
    return(fig7)
  }
  
}

generate_fig0 <- function(data) {
  
  data <- gen_data()$offspring_perm %>%
    group_by(cohort) %>% 
    mutate(#own_income_rank_in_family_dist = ecdf(hdsp_income)(own_income) * 100#, 
      own_income_rank_in_family_dist = ecdf(hdsp_income)(spouse_income) * 100
    )
  
  fig0 <- data %>%
    mutate(upward_rank = ifelse(offspring_hdsp_income_pct_rank > parent_hdsp_income_pct_rank, 1, 0), 
           upward_rank_own = ifelse(own_income_rank_in_family_dist > parent_hdsp_income_pct_rank, 1, 0)) %>%
    group_by(parent_hdsp_income_quintile, female, cohort) %>%
    summarise("finc_mean" = mean(upward_rank),
              "own_mean" = mean(upward_rank_own),
              "finc_se" = sd(upward_rank) / sqrt(n()),
              "own_se" = sd(upward_rank_own) / sqrt(n())
    ) %>%
    mutate(finc_min = finc_mean - finc_se,
           finc_max = finc_mean + finc_se,
           own_min = own_mean - own_se, 
           own_max = own_mean + own_se) %>%
    gather(measure, value, -c(parent_hdsp_income_quintile, female, cohort)) %>%
    mutate(female = factor(ifelse(female == 1, "Women", "Men"), levels = c("Women", "Men"))) %>%
    separate_wider_delim(measure, "_", names = c("measure", "estimate")) %>%
    mutate(measure = ifelse(measure == "finc", "Family Income", "Own Earnings")) %>%
    pivot_wider(names_from = estimate, values_from = value) %>%
    ggplot(aes(x = parent_hdsp_income_quintile, y = mean, 
               linetype = cohort, shape = female)) +
    geom_point() +
    #geom_errorbar(aes(ymin = min, ymax = max)) +
    geom_line() +
    theme_bw() +
    labs(x = "Parent Family Income Quintile", y = "% Experiencing Upward Rank Mobility, Family Income",
         title = "",
         caption = "Data from the Panel Study of Income Dynamics.") +
    theme(legend.position = c(0.8, 0.95),
          legend.justification = c(0, 1)) +
    facet_grid(cols = vars(measure)) +
    guides(shape = guide_legend(title = NULL, ncol = 1, order = 1), 
           linetype = guide_legend(title = NULL, ncol = 1, order = 1))
  return(fig0)
  
}


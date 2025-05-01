pers <- gen_data()$offspring_perm %>%
  filter(cohort == "1970-1985") %>%
  transmute(
      parent_inc      = parent_hdsp_income,
      own_avg         = own_income,
      partner_avg     = spouse_income,
      Y_own  = own_avg - parent_inc,
      Y_part = partner_avg,                 # partner contribution
      Y_tot  = Y_own + Y_part,          # = family_avg - parent_inc
      parent_rank = percent_rank(parent_inc) * 100,
      gender        = if_else(female == 0, "Men", "Women"), 
      parent_ptile = as.numeric(as.character(
        factor(cut(parent_rank,
                   breaks = seq(0, 100, by = 10),
                   include.lowest = TRUE,
                   right = FALSE),
               labels = seq(10, 100, by = 10))))
    )

pers %>%
  #filter(Y_own < 0) %>%
  group_by(parent_ptile , gender) %>%
  summarise(across(c(Y_tot, Y_own),
                   list(
                     q10   = ~ quantile(.x, 0.10, na.rm = TRUE),
                     q25   = ~ quantile(.x, 0.25, na.rm = TRUE),
                     q50 = ~ quantile(.x, 0.50, na.rm = TRUE),
                     q75   = ~ quantile(.x, 0.75, na.rm = TRUE),
                     q90   = ~ quantile(.x, 0.90, na.rm = TRUE)),
                   .names = "{.col}-{.fn}"),
            .groups = "drop") %>%
  gather(key, value, -c(parent_ptile, gender)) %>%
  separate_wider_delim(key, delim = "-", names = c("key", "quantile")) %>%
  pivot_wider(names_from = key, values_from = value) %>%
  mutate(Y_part = Y_tot - Y_own) %>%
  gather(key, value, -c(parent_ptile, gender, quantile)) %>%
  left_join(., pers %>% #filter(Y_own < 0) %>%
  group_by(parent_ptile , gender) %>%
  summarise(across(c(Y_tot, Y_own), mean,
                   .names = "{.col}"),
            .groups = "drop"
            ) %>%
    mutate(Y_part = Y_tot - Y_own) %>%
  gather(key, mean, -c(parent_ptile, gender)), 
  by = c("parent_ptile", "gender", "key")) %>%
  filter(quantile %in% c("q10", "q90")) %>%
  mutate(key = case_when(key == "Y_own" ~ "B) Distance, Personal Earnings",
                         key == "Y_tot" ~ "A) Distance, Family Income", 
                         key == "Y_part" ~ "C) Partner Contribution (B-A)"), 
         key = factor(key, levels = c("A) Distance, Family Income",
                                      "B) Distance, Personal Earnings",
                                      "C) Partner Contribution (B-A)")), 
         value = value/1000, 
         mean = mean/1000) %>%
  ggplot() +
  geom_line(aes(x = parent_ptile, y = mean, color = gender), size = .8) +
  geom_line(aes(x = parent_ptile, y = value, linetype = quantile, color = gender), alpha = .8, size = .8) +
  facet_grid(cols = vars(key)) +
  labs(linetype = "",
       x = "Parent Income Percentile",
       y = "Dollars (Thousands)", 
       title = "Distribution of Absolute Differences in Parent and Child Incomes",
       caption = "Data from the Panel Study of Income Dynamics, SRC Sample. \n Sample includes reference persons and spouses born 1970-1985. \n Childhood income is measured at ages 10-18. Adult observations are at ages 30-45. \n Solid lines indicate conditional means. Dashed and dotted lines indicate 10th and 90th conditional quantiles, respectively. \n Estimates binned at every 10th percentile of parental income."
  ) +
  theme_minimal() +  
  scale_linetype_manual(values = c("dashed", "dotted", "solid")) +
  scale_color_manual(values = c("dodgerblue", "orange")) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_blank(),
    plot.title = element_text(size = 14, hjust = .5, face = "bold"),
    strip.text = element_text(size = 10),
    legend.text=element_text(size = 10),
    plot.caption = element_text(face = "italic"),
    axis.text.x =element_text(size = 8, angle = 45),
    axis.text.y =element_text(size = 8)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  guides(linetype = "none")







# Prepare the data for ridge plots across parental income percentile
ridge_data <- pers %>%
  # Create parental income groups for cleaner visualization
  mutate(
    parent_ptile_group = cut(parent_rank, 
                             breaks = c(0, 25, 50, 75, 90, 100),
                             labels = c("0-25th", "25-50th", 
                                        "50-75th", "75-90th",
                                        "90-100th"),
                             include.lowest = TRUE),
    parent_ptile_group = fct_rev(parent_ptile_group)
  ) %>%
  # Select relevant variables
  select(gender, parent_ptile_group, 
         Y_own, Y_part) %>%
  # Only include those with gaps
  #filter(Y_own > 0) %>%
  # Create separate datasets for mobility gaps and spouse incomes
  pivot_longer(
    cols = c(Y_own, Y_part),
    names_to = "measure",
    values_to = "income"
  ) %>%
  # Log transform income
  mutate(
    # Ensure clear labeling
    measure = factor(measure, 
                     levels = c("Y_own", "Y_part"),
                     labels = c("Distance, Personal Earnings", "Partner Earnings"))
  )

# 1. summaries per strip ------------------------------------------
ridge_stats <- ridge_data %>%                               # <- your long file
  filter(measure == "Distance, Personal Earnings",
         income < 0) %>%
  mutate(income = log(-income)) %>%                         # same transform
  group_by(parent_ptile_group, gender) %>%
  summarise(
    mean = mean(income, na.rm = TRUE),
    q10  = quantile(income, 0.10, na.rm = TRUE),
    q90  = quantile(income, 0.90, na.rm = TRUE),
    .groups = "drop"
  ) %>%                                   # pivot to a long format for ggplot
  tidyr::pivot_longer(-c(parent_ptile_group, gender),
                      names_to = "stat", values_to = "x")

# Alternative approach with discrete parental income groups
ggplot(ridge_data %>%
         filter(measure == "Distance, Personal Earnings", 
                income < 0) %>%
         mutate(income = log(income * -1)),
       aes(x = income, y = parent_ptile_group, fill = gender)) +
  # Create ridge density plot
  geom_density_ridges(
    alpha = 0.7,
    scale = 0.9,
    rel_min_height = 0.01
  ) +
  labs(
    title = "Distribution of Distance from Threshold for Upward Mobility via Personal Earnings",
    x = "Log (Parent - Earnings | Earnings < Parent)",
    y = "Parental Income Percentile",
    fill = "Gender"
  ) +
  # Reverse y-axis to have lowest percentiles at the bottom
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = c("dodgerblue", "orange")) +
  # Improve theme
  theme_minimal() +
  coord_flip() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

ridge_stats %>%
  mutate(x = exp(x)) %>%
  pivot_wider(names_from = c(gender, stat), values_from = x) %>%
  transmute(parent_ptile_group, 
            mean = Men_mean/Women_mean)

# Alternative approach with discrete parental income groups
ggplot(ridge_data, aes(x = income, y = parent_ptile_group, fill = measure)) +
  # Create ridge density plot
  geom_density_ridges(
    alpha = 0.7,
    scale = 0.9,
    rel_min_height = 0.01
  ) +
  # Facet by gender
  facet_wrap(~ gender, ncol = 1) +
  # Add labels
  labs(
    title = "Distribution of Mobility Deficit and Partner Earnings by Parental Income",
    x = "Log Income",
    y = "Parental Income Percentile Group",
    fill = "Measure"
  ) +
  # Custom color palette
  scale_fill_manual(values = c("Distance, Personal Earnings Threshold" = "#E69F00", "Partner Earnings" = "#56B4E9")) +
  # Reverse y-axis to have lowest percentiles at the bottom
  scale_y_discrete(limits = rev) +
  # Improve theme
  theme_minimal() +
  coord_flip() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )



# Alternative approach with discrete parental income groups
ridge_data %>%
  select(gender, cohort, parent_ptile_group, measure, log_income) %>%
  pivot_wider(names_from = measure, values_from = log_income) %>%
  unnest() %>%
  rename(mobdef = "Mobility Deficit", partner = "Partner Earnings") %>%
  mutate(difference = mobdef-partner) %>%
  ggplot(aes(x = difference, y = parent_ptile_group, fill = gender)) +
  # Create ridge density plot
  geom_density_ridges(
    alpha = 0.7,
    scale = 0.9,
    rel_min_height = 0.01
  ) +
  # Add labels
  labs(
    title = "Distribution of Difference in Mobility Deficit and Partner Earnings by Parental Income",
    x = "",
    y = "Parental Income Percentile Group",
    fill = "Gender"
  ) +
  # Custom color palette
  scale_fill_manual(values = c("Women" = "#E69F00", "Men" = "#56B4E9")) +
  # Reverse y-axis to have lowest percentiles at the bottom
  scale_y_discrete(limits = rev) +
  # Improve theme
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  coord_flip() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

# Deirdre 
# Presence of a partner at the child generation- way to incorporate partner in the parent generation
# Attention to the role of partners for men is a good one
# Returns to education- paper in Demography by Arthur Sakamoto showing that returns to education,
# their partner's earnings are important for those returns
# Chetty on mobility by race- trying to understand to what extent is it really men's earnings 
# diriving things, when women's earnigns are more related to partnership


# Alternative approach with discrete parental income groups
ggplot(data %>%
         mutate(diff = own_income  - parent_hdsp_income,
                parent_ptile_group = cut(parent_ptile, 
                                         breaks = c(0, 10, 25, 50, 75, 90, 100),
                                         labels = c("0-10th", "10-25th", "25-50th", 
                                                    "50-75th", "75-90th", "90-100th"),
                                         include.lowest = TRUE),
                parent_ptile_group = fct_rev(parent_ptile_group)
         ) %>%
         filter(cohort == "1970-1985"),
       aes(x = diff, y = parent_ptile_group, fill = gender)) +
  # Create ridge density plot
  geom_density_ridges(
    alpha = 0.7,
    scale = 0.9,
    rel_min_height = 0.01
  ) +
  # Facet by gender
  #facet_wrap(~ gender, ncol = 1) +
  # Add labels
  labs(
    title = "Distribution of Mobility Deficit and Partner Earnings by Parental Income",
    x = "Log Income",
    y = "Parental Income Percentile Group",
    fill = "Measure"
  ) +
  # Custom color palette
  #scale_fill_manual(values = c("Mobility Deficit" = "#E69F00", "Partner Earnings" = "#56B4E9")) +
  # Reverse y-axis to have lowest percentiles at the bottom
  scale_y_discrete(limits = rev) +
  # Improve theme
  theme_minimal() +
  coord_flip() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5))

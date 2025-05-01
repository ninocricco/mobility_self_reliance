data <- gen_data(cohorts = list("1970-1985" = 1970:1985))$offspring_perm %>%
  mutate(parent_rank = percent_rank(parent_hdsp_income) * 100) %>%
  mutate(parent_ptile = as.numeric(as.character(
    factor(cut(parent_rank,
               breaks = seq(0, 100, by = 10),
               include.lowest = TRUE,
               right = FALSE),
           labels = seq(10, 100, by = 10)))),
    gender = ifelse(female == 1, "Women", "Men"))

# Calculate the partner contribution decomposition
mobility_decomposition <- data %>%
  # Create necessary indicators
  mutate(
    # Indicator for having a mobility gap (own income < parent income)
    has_gap = own_income < parent_hdsp_income,
    
    # Indicator for having a partner
    has_partner = married/num_observedinadulthood,
    
    # Indicator for closing the gap with family income
    closes_gap = hdsp_income >= parent_hdsp_income & has_gap,
    
    # Indicator for partner contribution to mobility
    partner_contribution = abs.fam.mob == 1 & abs.own.mob == 0
  ) %>%
  # Group by parental income percentile (and any other grouping variables)
  group_by(gender, cohort, parent_ptile) %>%
  summarise(
    # Sample size in this cell (for diagnostics)
    n = n(),
    
    # Observed partner contribution to mobility
    observed_partner_share = mean(partner_contribution),
    
    # Verify calculation method
    observed_partner_share_alt = mean(abs.fam.mob) - mean(abs.own.mob),
    
    # Component 1: Partnership rate
    partnership_rate = mean(has_partner),
    
    # Component 2: Gap prevalence (proportion with mobility gap)
    gap_prevalence = mean(has_gap),
    
    # Count partnered people with gaps (for diagnostics)
    n_partner_with_gap = sum(has_partner & has_gap),
    
    # Component 3: Gap closing rate
    # Using sum() instead of mean() to properly calculate conditional probability
    gap_closing_rate = if_else(
      sum(has_partner & has_gap) > 0,
      sum(has_partner & has_gap & closes_gap) / sum(has_partner & has_gap),
      NA_real_  # Handle cases with zero denominator
    ),
    
    # For comparison: Unconditional gap closing rate
    uncond_closing_rate = mean(closes_gap & has_partner)
  ) %>%
  # Calculate decomposition product
  mutate(
    # The decomposed partner contribution
    decomposed_share = partnership_rate * gap_prevalence * gap_closing_rate,
    cf_eq_partnership = case_when(gender == "Women" ~ 1 * gap_prevalence * gap_closing_rate,
                                  gender == "Men" ~ 1 * gap_prevalence * gap_closing_rate),
    
    # Calculate ratio to check accuracy of decomposition
    decomp_ratio = if_else(
      observed_partner_share > 0,
      decomposed_share / observed_partner_share,
      NA_real_
    ),
    
    # Add a calibrated version if needed
    calibrated_closing_rate = if_else(
      partnership_rate * gap_prevalence > 0,
      observed_partner_share / (partnership_rate * gap_prevalence),
      NA_real_
    ),
    
    # Add normalized decomposition
    normalized_decomp = partnership_rate * gap_prevalence * calibrated_closing_rate
  )

# Check the output
summary(mobility_decomposition)

# Calculate the partner contribution decomposition
mobility_decomposition_full <- data %>%
  # Create necessary indicators
  mutate(
    # Indicator for having a mobility gap (own income < parent income)
    has_gap = own_income < parent_hdsp_income,
    
    # Indicator for having a partner
    has_partner = married/num_observedinadulthood,
    
    # Indicator for closing the gap with family income
    closes_gap = hdsp_income >= parent_hdsp_income & has_gap,
    
    # Indicator for partner contribution to mobility
    partner_contribution = abs.fam.mob == 1 & abs.own.mob == 0
  ) %>%
  # Group by parental income percentile (and any other grouping variables)
  group_by(gender, cohort) %>%
  summarise(
    # Sample size in this cell (for diagnostics)
    n = n(),
    
    # Observed partner contribution to mobility
    observed_partner_share = mean(partner_contribution),
    
    # Verify calculation method
    observed_partner_share_alt = mean(abs.fam.mob) - mean(abs.own.mob),
    
    # Component 1: Partnership rate
    partnership_rate = mean(has_partner),
    
    # Component 2: Gap prevalence (proportion with mobility gap)
    gap_prevalence = mean(has_gap),
    
    # Count partnered people with gaps (for diagnostics)
    n_partner_with_gap = sum(has_partner & has_gap),
    
    # Component 3: Gap closing rate
    # Using sum() instead of mean() to properly calculate conditional probability
    gap_closing_rate = if_else(
      sum(has_partner & has_gap) > 0,
      sum(has_partner & has_gap & closes_gap) / sum(has_partner & has_gap),
      NA_real_  # Handle cases with zero denominator
    ),
    
    # For comparison: Unconditional gap closing rate
    uncond_closing_rate = mean(closes_gap & has_partner)
  ) %>%
  # Calculate decomposition product
  mutate(
    # The decomposed partner contribution
    decomposed_share = partnership_rate * gap_prevalence * gap_closing_rate,
    cf_eq_partnership = case_when(gender == "Women" ~ 1 * gap_prevalence * gap_closing_rate,
                                  gender == "Men" ~ 1 * gap_prevalence * gap_closing_rate),
    
    # Calculate ratio to check accuracy of decomposition
    decomp_ratio = if_else(
      observed_partner_share > 0,
      decomposed_share / observed_partner_share,
      NA_real_
    ),
    
    # Add a calibrated version if needed
    calibrated_closing_rate = if_else(
      partnership_rate * gap_prevalence > 0,
      observed_partner_share / (partnership_rate * gap_prevalence),
      NA_real_
    ),
    
    # Add normalized decomposition
    normalized_decomp = partnership_rate * gap_prevalence * calibrated_closing_rate
  )

# Visualization of decomposition
mobility_decomposition %>%
  select(gender, parent_ptile, decomposed_share, partnership_rate,
         gap_prevalence, gap_closing_rate) %>%
  gather(key, value, -c(gender, parent_ptile, cohort)) %>%
  mutate(key = case_when(key == "decomposed_share" ~ "Partner Contributions", 
                         key == "gap_prevalence" ~ "Upward Mobility Deficit",
                         key == "partnership_rate" ~ "Proportion Partnered",
                         key == "gap_closing_rate" ~ "Partner Earnings Exceed Deficit"),
         key = factor(key, levels = c("Partner Contributions", "Upward Mobility Deficit",
                                      "Proportion Partnered",  "Partner Earnings Exceed Deficit"))) %>%
  ggplot(aes(x = parent_ptile, linetype = gender, y = value)) +
  geom_line() +
  facet_grid(~key)+
  labs(fill = "",
       x = "Parent Income Percentile",
       y = "Percent", 
       title = "Decomposing Partner Contribution by Childhood Income Percentile and Gender",
       caption = "Data from the Panel Study of Income Dynamics, SRC Sample. Sample includes reference persons and spouses born 1970-1985. \n Childhood income is measured at ages 10-18. Adult observations are at ages 30-45. \n Point estimates are binned every 5th percentile."
  ) +
  theme_minimal() +  
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_blank(),
    plot.title = element_text(size = 14, hjust = .5, face = "bold"),
    strip.text = element_text(size = 12),
    legend.text=element_text(size = 12),
    plot.caption = element_text(face = "italic"),
    axis.text.x =element_text(size = 8, angle = 45),
    axis.text.y =element_text(size = 8)
  ) 

mobility_decomposition %>%
  select(gender, parent_ptile, decomposed_share, cf_eq_partnership) %>%
  gather(key, value, -c(gender, parent_ptile, cohort)) %>%
  mutate(key = case_when(key == "decomposed_share" ~ "Partner Contributions", 
                         key == "cf_eq_partnership" ~ "Full Partnership")) %>%
  ggplot(aes(x = parent_ptile, linetype = key, y = value)) +
  geom_line() +
  facet_grid(~gender)+
  labs(fill = "",
       x = "Parent Income Percentile",
       y = "Percent", 
       title = "Counterfactual Partner Contribution by Childhood Income Percentile and Gender",
       caption = "Data from the Panel Study of Income Dynamics, SRC Sample. Sample includes reference persons and spouses born 1970-1985. \n Childhood income is measured at ages 10-18. Adult observations are at ages 30-45. \n Point estimates are binned every 5th percentile."
  ) +
  theme_minimal() +  
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_blank(),
    plot.title = element_text(size = 14, hjust = .5, face = "bold"),
    strip.text = element_text(size = 12),
    legend.text=element_text(size = 12),
    plot.caption = element_text(face = "italic"),
    axis.text.x =element_text(size = 8, angle = 45),
    axis.text.y =element_text(size = 8)
  )

ev_decomposition <- data %>%
  group_by(gender, cohort, parent_ptile) %>%
  mutate(partnership_intensity = married/num_observedinadulthood) %>%
  summarise(
    # Expected value of incomes
    E_parent = mean(parent_hdsp_income),
    E_own = mean(own_income),
    E_gap = mean(pmax(0, parent_hdsp_income - own_income)),
    
    # Partnership rate
    P_rate = mean(partnership_intensity),
    
    # Gap probability
    P_gap = mean(parent_hdsp_income > own_income),
    # Expected partner contribution conditional on having a gap and a partner
    E_partner_contrib_if_gap_and_partner = mean(
      spouse_income[parent_hdsp_income > own_income & partnership_intensity > 0], 
      na.rm = TRUE
    ),
    
    sufficiency_ratio = E_partner_contrib_if_gap_and_partner / E_gap,
    
    # Probability that partner income exceeds the gap
    P_partner_closes_gap = mean(
      spouse_income >= (parent_hdsp_income - own_income)
      [parent_hdsp_income > own_income & partnership_intensity > 0],
      na.rm = TRUE
    ),
    
    # Expected partner contribution to mobility
    expected_partner_mobility = P_rate * P_gap * P_partner_closes_gap,
    
    # Observed partner contribution to mobility
    observed_partner_mobility = mean(hdsp_income >= parent_hdsp_income & own_income < parent_hdsp_income)
  )



# Check where the decomposition fits well vs. poorly
fit_quality <- mobility_decomposition %>%
  filter(!is.na(decomp_ratio)) %>%
  mutate(
    fit_quality = case_when(
      between(decomp_ratio, 0.9, 1.1) ~ "Good fit (within 10%)",
      between(decomp_ratio, 0.8, 1.2) ~ "Moderate fit (within 20%)",
      TRUE ~ "Poor fit (> 20% difference)"
    )
  ) %>%
  group_by(gender, cohort, fit_quality) %>%
  summarise(
    n_percentiles = n(),
    avg_cell_size = mean(n),
    min_cell_size = min(n),
    max_cell_size = max(n)
  )

# Print fit quality summary
print(fit_quality)

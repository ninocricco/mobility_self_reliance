data <- gen_data()$offspring_perm %>%
  mutate(parent_ptile = as.numeric(as.character(
    factor(cut(parent_hdsp_income_pct_rank,
               breaks = seq(0, 100, by = 5),
               include.lowest = TRUE,
               right = FALSE),
           labels = seq(5, 100, by = 5)))),
         gender = ifelse(female == 1, "Women", "Men"))

mob_data_percentile <- data %>%
  group_by(gender, cohort, parent_ptile) %>%
  summarise(abs.fam.mob = mean(abs.fam.mob), 
            abs.own.mob = mean(abs.own.mob), 
            rel.fam.mob = mean(rel.fam.mob), 
            rel.own.mob = mean(rel.own.mob)) %>%
  mutate(abs.fam.mob = abs.fam.mob - abs.own.mob,
         rel.fam.mob = rel.fam.mob - rel.own.mob) %>%
  pivot_longer(
    cols = c(abs.fam.mob, abs.own.mob, 
             rel.fam.mob, rel.own.mob),
    names_to = "mobility_type",
    values_to = "mean"
  ) %>%
  separate(
    mobility_type,
    into = c("mobtype", "inctype"),
    sep = "\\.",
    extra = "merge"
  )

mob_data_all <- data %>%
  group_by(gender, cohort) %>%
  summarise(abs.fam.mob = mean(abs.fam.mob), 
            abs.own.mob = mean(abs.own.mob), 
            rel.fam.mob = mean(rel.fam.mob), 
            rel.own.mob = mean(rel.own.mob), 
            partner_share_abs = abs.fam.mob-abs.own.mob, 
            partner_share_rel = rel.fam.mob-rel.own.mob)

mob_fig <- mob_data_percentile %>%
  #filter(cohort == "1950-1969") %>%
  filter(cohort == "1970-1985") %>%
  mutate(inctype = ifelse(inctype == "fam.mob", "Family Income", "Individual Income"), 
         mobtype = ifelse(mobtype == "abs", "Absolute Income", "Income Rank")) %>%
  ggplot() +
  geom_area(aes(x = parent_ptile, y = mean, 
                fill = inctype)) +
  facet_grid(rows = vars(mobtype), cols = vars(gender)) +
  labs(color = "",
       shape = "Income Type",
       x = "Parent Income Percentile",
       y = "Percent", 
       title = "% Children with Adult Income > Childhood Income by Gender",
       caption = "Data from the Panel Study of Income Dynamics, SRC Sample. Sample includes reference persons and spouses born 1970-1985. \n Childhood income is measured at ages 10-18. Adult income is measured at ages 30-45. Estimates are binned every 5th percentile. \n For the rank specification, both individual and family income ranks are defined in reference to the family income distribuiton."
       ) +
  theme_minimal() +  
  scale_fill_manual(values = c("orange2", "steelblue4")) +
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

dist_percentile_abs <- data %>%
  group_by(gender, cohort, parent_ptile) %>%
  summarise(parent = mean(parent_hdsp_income), 
            own = mean(own_income), 
            spouse = mean(spouse_income),
            hdsp = mean(hdsp_income)) %>%
  mutate(dist_own = own-parent, 
         dist_spouse = hdsp-parent) %>% 
  gather(dist_type, value, -c(parent_ptile, gender, cohort)) %>%
  filter(dist_type %in% c("dist_own", "dist_spouse"), cohort == "1970-1985") %>%
  mutate(dist_type = ifelse(dist_type == "dist_own", "Individual Income", "Family Income"), 
         value = value/1000) %>%
  ggplot(aes(x = parent_ptile, y = value, 
             color = dist_type, fill = dist_type, shape = dist_type)) +
  geom_point() +
  geom_smooth(method = "gam") +
  facet_grid(cols = vars(gender)) +
  labs(color = "",
       x = "Parent Income Percentile",
       y = "Average Adult - Childhood Income (Thousands)", 
       title = "Average Distance Between Childhood and Adult Income by Gender and Income Type",
       caption = "Data from the Panel Study of Income Dynamics, SRC Sample. Sample includes reference persons and spouses born 1970-1985. \n Parent income is measured at ages 10-18. Adult income is measured at ages 30-45. \n Point estimates are binned every 5th percentile. Fitted line from Generalized Additive Model."
  ) +
  scale_fill_manual(values = c("orange2", "steelblue4"), name = "") +
  scale_color_manual(values = c("orange2", "steelblue4"), name = "") +
  scale_shape_manual(values = c(21, 22), name = "") +
  geom_hline(yintercept = 0, linetype = "dashed") +
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

dist_percentile_abs_median <- data %>%
  mutate(dist_own = own_income-parent_hdsp_income, 
         dist_spouse = hdsp_income-parent_hdsp_income) %>% 
  group_by(gender, cohort, parent_ptile) %>%
  summarise(dist_own = quantile(dist_own, probs = .5), 
            dist_spouse = quantile(dist_spouse, probs = .5)) %>%
  gather(dist_type, value, -c(parent_ptile, gender, cohort)) %>%
  filter(dist_type %in% c("dist_own", "dist_spouse"), cohort == "1950-1969") %>%
  mutate(dist_type = ifelse(dist_type == "dist_own", "Individual Income", "Family Income"), 
         value = value/1000) %>%
  ggplot(aes(x = parent_ptile, y = value, 
             color = dist_type, fill = dist_type, shape = dist_type)) +
  geom_point() +
  geom_smooth(method = "gam") +
  facet_grid(cols = vars(gender)) +
  labs(color = "",
       x = "Parent Income Percentile",
       y = "Average Adult - Childhood Income (Thousands)", 
       title = "Median Distance Between Childhood and Adult Income by Gender and Income Type",
       caption = "Data from the Panel Study of Income Dynamics, SRC Sample. Sample includes reference persons and spouses born 1970-1985. \n Parent income is measured at ages 10-18. Adult income is measured at ages 30-45. \n Point estimates are binned every 5th percentile. Fitted line from Generalized Additive Model."
  ) +
  scale_fill_manual(values = c("orange2", "steelblue4"), name = "") +
  scale_color_manual(values = c("orange2", "steelblue4"), name = "") +
  scale_shape_manual(values = c(21, 22), name = "") +
  geom_hline(yintercept = 0, linetype = "dashed") +
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

table <- mob_data_percentile %>%
  filter(cohort == "1970-1985") %>%
  mutate(mobtype = ifelse(mobtype == "abs", "Absolute", "Relative")) %>%
  pivot_wider(names_from = inctype, values_from = mean) %>% 
  mutate(spouse.mob = fam.mob, fam.mob = own.mob + spouse.mob) %>%
  pivot_longer(c(own.mob, spouse.mob, fam.mob), names_to = "inctype") %>%
  mutate(inctype = factor(
    inctype, levels = c("own.mob", "spouse.mob", "fam.mob"),
    labels = c("Individual", "Spouse", "Total"))) %>% 
  filter(parent_ptile %in% c(5, 20, 40, 60, 80, 95)) %>% 
  pivot_wider(names_from = parent_ptile, values_from = value) %>%
  arrange(gender, mobtype, inctype) %>%
  ungroup()

table %>%
  # Select relevant columns and rename for clarity
  select(inctype, everything(), -c(cohort, gender, mobtype)) %>%
  # Filter to keep only Total Family Income Mobility rows for cleaner comparison
  # Create the LaTeX table
  kbl(format = "latex", booktabs = TRUE, digits = 3,
      col.names = c("Income Type", 
                    "5", "20", "40", "60", "80", "95"),
      caption = "Contribution to Share of Children with Income > Parental Income",
      label = "tab:mobility") %>%
  # Add column grouping
  add_header_above(c(" " = 3, "Percentile" = 6)) %>%
  pack_rows("Men, Absolute", 1, 3) %>%
  pack_rows("Men, Relative", 4, 6) %>%
  pack_rows("Women, Absolute", 7, 9) %>%
  pack_rows("Women, Relative", 10, 12)

# Prepare the data for ridge plots across parental income percentile
ridge_data <- data %>%
  # Create parental income groups for cleaner visualization
  mutate(
    parent_ptile_group = cut(parent_ptile, 
                             breaks = c(0, 10, 25, 50, 75, 90, 100),
                             labels = c("0-10th", "10-25th", "25-50th", 
                                        "50-75th", "75-90th", "90-100th"),
                             include.lowest = TRUE),
    parent_ptile_group = fct_rev(parent_ptile_group),
    
    # Calculate mobility gap correctly
    mobility_gap = if_else(own_income < parent_hdsp_income, 
                           parent_hdsp_income - own_income, 
                           as.numeric(NA)),
  ) %>%
  # Select relevant variables
  select(gender, cohort, parent_ptile, parent_ptile_group, 
         mobility_gap, spouse_income) %>%
  # Only include those with gaps
  filter(!is.na(mobility_gap)) %>%
  # Create separate datasets for mobility gaps and spouse incomes
  pivot_longer(
    cols = c(mobility_gap, spouse_income),
    names_to = "measure",
    values_to = "income"
  ) %>%
  # Log transform income
  mutate(
    log_income = log1p(income + 5),
    # Ensure clear labeling
    measure = factor(measure, 
                     levels = c("mobility_gap", "spouse_income"),
                     labels = c("Mobility Deficit", "Partner Earnings"))
  ) %>%
  filter(cohort == "1970-1985")

# Alternative approach with discrete parental income groups
ggplot(ridge_data, aes(x = log_income, y = parent_ptile_group, fill = measure)) +
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
  scale_fill_manual(values = c("Mobility Deficit" = "#E69F00", "Partner Earnings" = "#56B4E9")) +
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

# Deirdre 
# Presence of a partner at the child generation- way to incorporate partner in the parent generation
# Attention to the role of partners for men is a good one
# Returns to education- paper in Demography by Arthur Sakamoto showing that returns to education,
# their partner's earnings are important for those returns
# Chetty on mobility by race- trying to understand to what extent is it really men's earnings 
# diriving things, when women's earnigns are more related to partnership, and 

  

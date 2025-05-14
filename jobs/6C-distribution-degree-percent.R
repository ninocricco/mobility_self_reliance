source("jobs/60-gensample.R")

data %>% mutate(spouse_distance.obs = spouse_income-parent_hdsp_income, 
                own_distance.obs = own_income-parent_hdsp_income,
                family_distance.obs = hdsp_income-parent_hdsp_income) %>%
  mutate(parent_ptile = as.numeric(
    as.character(
      factor(cut(parent_rank, breaks = seq(0, 100, by = 5),
                 include.lowest = TRUE, right = FALSE),
             labels = seq(5, 100, by = 5))))) %>%
  select(gender, parent_ptile, parent_hdsp_income, 
         own_distance.obs, spouse_distance.obs, family_distance.obs) %>%
  gather(key, value, -c(gender, parent_ptile, parent_hdsp_income)) %>%
  mutate(dist_mag = case_when(value <  parent_hdsp_income * - .75 ~ "Downward Mobility: > 75%", 
                              value <  parent_hdsp_income * - .25 ~ "Downward Mobility: Between 25 & 75 %", 
                              value < 0 ~ "Downward Mobility: < 25 %", 
                              value < parent_hdsp_income * .25 ~ "Upward Mobility: < 25 %", 
                              value < parent_hdsp_income * .75 ~ "Upward Mobility: Between 75 and 25 %", 
                              TRUE ~ "Upward Mobility: > 75 %"
                              ), 
         dist_mag = factor(dist_mag, levels = c("Downward Mobility: > 75%", "Downward Mobility: Between 25 & 75 %", "Downward Mobility: < 25 %", 
                                                "Upward Mobility: < 25 %", "Upward Mobility: Between 75 and 25 %", "Upward Mobility: > 75 %")),
         key = factor(case_when(key == "own_distance.obs"~ "Personal Earnings", 
                         key == "spouse_distance.obs" ~ "Partner Earnings", 
                         TRUE ~ "Family Income"), levels = c("Family Income", "Personal Earnings", "Partner Earnings"))) %>%
  group_by(gender, parent_ptile, key) %>%
  count(dist_mag, name = "n") %>%       # same as group_by() + summarise(n = n())
  complete(dist_mag,                    # list every level of `group`
           fill = list(n = 0)) %>%   # turn absent groups into n = 0
  mutate(freq = n / sum(n)) %>%
  ggplot(aes(x = parent_ptile, y = freq, fill = dist_mag)) +
  geom_area() +
  facet_grid(rows = vars(gender), cols = vars(key)) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_fill_manual(values = c("orangered2", "orange", "gold2", "skyblue2", "dodgerblue2", "royalblue4")) +
  labs(x = "Parent Income Percentile",
       y = "Share", 
       title = "Degree of Mobility by Income Source and Gender Across the Distribution",
       caption = "Data from the Panel Study of Income Dynamics, SRC Sample. Sample includes reference persons and spouses born 1950-1980. \n Childhood income is measured at ages 10-18. Adult income is measured at ages 30-45. Estimates are binned every 10th percentile. ") +
  theme_minimal() +  
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank(),
    plot.title = element_text(size = 14, hjust = .5, face = "bold"),
    strip.text = element_text(size = 12),
    legend.text=element_text(size = 12),
    plot.caption = element_text(face = "italic"),
    axis.text.x =element_text(size = 8, angle = 45),
    axis.text.y =element_text(size = 8)
  )
  

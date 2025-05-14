source("jobs/60-gensample.R")

data %>%
  group_by(gender) %>%
  summarise(across(c(abs.own.mob, abs.spouse.mob, abs.combined, abs.fam.mob), 
                   ~ mean(.x, na.rm = TRUE))) %>%
  rename("Focal > Parent" = "abs.own.mob",
         "+ Non-Focal > Parent" = "abs.spouse.mob", 
         "+ Joint > Parent" = "abs.combined", 
         "Joint > Parent" = "abs.fam.mob") %>%
  mutate(Focal = "Own") %>%
  bind_rows(data %>%
              group_by(gender) %>%
              summarise(across(c(abs.spouse.mob.alt, abs.own.mob.alt, abs.combined.alt, abs.fam.mob), 
                               ~ mean(.x, na.rm = TRUE))) %>%
              rename("Focal > Parent" = "abs.spouse.mob.alt",
                     "+ Non-Focal > Parent" = "abs.own.mob.alt", 
                     "+ Joint > Parent" = "abs.combined.alt", 
                     "Joint > Parent" = "abs.fam.mob") %>%
              mutate(Focal = "Partner")) %>%
  select(gender, Focal, everything())

data %>%
  group_by(gender, parent_ptile) %>%
  summarise(across(c(abs.own.mob, abs.spouse.mob, abs.combined, abs.fam.mob), 
                   ~ mean(.x, na.rm = TRUE))) %>%
  pivot_longer(
    cols = c(abs.own.mob, abs.spouse.mob, abs.combined),
    names_to = "mobility_type",
    values_to = "mean") %>%
  mutate(inctype = factor(case_when(mobility_type == "abs.own.mob" ~ "Personal Earnings",
                             mobility_type == "abs.spouse.mob" ~ "+ Partner Earnings",
                             mobility_type == "abs.combined" ~ "+ Joint Earnings"),
                          levels = c("Personal Earnings", "+ Partner Earnings", "+ Joint Earnings"))) %>%
  ggplot() +
  geom_area(aes(x = parent_ptile, y = mean, 
                fill = inctype),
            position = position_stack(reverse = T)) +
  facet_grid(cols = vars(gender)) +
  labs(color = "",
       shape = "Income Type",
       x = "Parent Income Percentile",
       y = "Percent", 
       title = "% Children with Adult Income > Childhood Income by Gender",
       caption = "Data from the Panel Study of Income Dynamics, SRC Sample. Sample includes reference persons and spouses born 1950-1980. \n Childhood income is measured at ages 10-18. Adult income is measured at ages 30-45. Estimates are binned every 10th percentile. "
  ) +
  theme_minimal() +  
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_fill_manual(values = c("+ Joint Earnings" = "orange1", 
                               "+ Partner Earnings" = "firebrick", 
                               "Personal Earnings" = "steelblue4")) +
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

  

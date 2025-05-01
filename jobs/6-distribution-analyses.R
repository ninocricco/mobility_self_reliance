data <- gen_data(cohorts = list("1970-1985" = 1970:1985))$offspring_perm %>%
  mutate(parent_rank = percent_rank(parent_hdsp_income) * 100) %>%
  mutate(parent_ptile = as.numeric(as.character(
    factor(cut(parent_rank,
               breaks = seq(0, 100, by = 10),
               include.lowest = TRUE,
               right = FALSE),
           labels = seq(10, 100, by = 10)))),
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
  group_by(gender) %>%
  summarise(abs.fam.mob = mean(abs.fam.mob), 
            abs.own.mob = mean(abs.own.mob), 
            rel.fam.mob = mean(rel.fam.mob), 
            rel.own.mob = mean(rel.own.mob), 
            partner_share_abs = abs.fam.mob-abs.own.mob, 
            partner_share_rel = rel.fam.mob-rel.own.mob)

mob_fig <-
  mob_data_percentile %>%
  mutate(inctype = ifelse(inctype == "fam.mob", "Family Income", "Individual Income"), 
         mobtype = ifelse(mobtype == "abs", "Absolute Income", "Income Rank")) %>%
  filter(mobtype == "Absolute Income") %>%
  ggplot() +
  geom_area(aes(x = parent_ptile, y = mean, 
                fill = inctype)) +
  facet_grid(rows = vars(mobtype), cols = vars(gender)) +
  labs(color = "",
       shape = "Income Type",
       x = "Parent Income Percentile",
       y = "Percent", 
       title = "% Children with Adult Income > Childhood Income by Gender",
       caption = "Data from the Panel Study of Income Dynamics, SRC Sample. Sample includes reference persons and spouses born 1969-1979. \n Childhood income is measured at ages 10-18. Adult income is measured at ages 30-45. Estimates are binned every 5th percentile. \n For the rank specification, both individual and family income ranks are defined in reference to the family income distribuiton."
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

mob_fig

table <- mob_data_percentile %>%
  mutate(mobtype = ifelse(mobtype == "abs", "Absolute", "Relative")) %>%
  pivot_wider(names_from = inctype, values_from = mean) %>% 
  mutate(spouse.mob = fam.mob, fam.mob = own.mob + spouse.mob) %>%
  pivot_longer(c(own.mob, spouse.mob, fam.mob), names_to = "inctype") %>%
  mutate(inctype = factor(
    inctype, levels = c("own.mob", "spouse.mob", "fam.mob"),
    labels = c("Individual", "Spouse", "Total"))) %>% 
  pivot_wider(names_from = parent_ptile, values_from = value) %>%
  arrange(gender, mobtype, inctype) %>%
  ungroup()

table


  

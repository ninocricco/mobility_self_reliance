source("jobs/60-gensample.R")

# --------------------------------------------------------------------
# 1.  Build the child–level building blocks --------------------------
# --------------------------------------------------------------------
df <- data %>%                     # <- your child-level data.frame / tibble
  mutate(
    own_loses   = abs.own.mob == 0,                 # set S : lose on own
    m_i         = partner_rate,                     # share of years partnered
    e_i         = partner_rate * partner_lf_rate,   # share of years partnered *and* partner working
    e_i2        = partner_rate * partner_ft_rate,   # share of years partnered *and* partner working
    j_i         = ifelse(own_loses & abs.fam.mob == 1, 1, 0)  # joint beats parent *and* own loses
  )

# --------------------------------------------------------------------
# 2.  Compute the three factors, overall -----------------------------
# --------------------------------------------------------------------
S <- df %>% filter(own_loses)                       # the conditioning set |S|

N_S <- nrow(S)                                      # |S|

F1 <- sum(S$m_i)   / N_S                            # average partnership exposure
F2 <- sum(S$e_i)   / sum(S$m_i)                     # partner-LFP exposure given partnered
F3 <- sum(S$j_i)   / sum(S$e_i)                     # success per unit of partner-work exposure
F22 <- sum(S$e_i2)   / sum(S$m_i)                     # partner-LFP exposure given partnered
F32 <- sum(S$j_i)   / sum(S$e_i2)                     # success per unit of partner-work exposure

conditional_mobility <- F1 * F2 * F3                # check: equals mean(S$j_i)
conditional_mobility_v2 <- F1 * F22 * F32                # check: equals mean(S$j_i)

# --------------------------------------------------------------------
# 3.  (Optional) Do it by parent-income percentile -------------------
#    assuming you have a variable parent_pct taking 1…100
# --------------------------------------------------------------------
results_by_pct <- df %>%
  filter(own_loses) %>%
  group_by(gender, parent_ptile) %>%
  summarise(
    F1  = sum(m_i)         / n(),                   # within-percentile |S_b|
    F2  = sum(e_i)         / sum(m_i),
    F3  = sum(j_i)         / sum(e_i),
    F22 = sum(e_i2)   / sum(m_i),                     # partner-LFP exposure given partnered
    F32 = sum(j_i)   / sum(e_i2),                     # success per unit of partner-work exposure
    mob = F1 * F2 * F3,
    test = mean(j_i)
  ) %>%
  bind_cols(data %>% filter(abs.own.mob == 0) %>% 
              mutate(test2 = abs.spouse.mob + abs.combined) %>%
              group_by(gender, parent_ptile) %>% 
              summarise(test2 = mean(test2)) %>%
              ungroup() %>%
              select(test2))

results_by_pct %>%
  select(-test) %>%
  gather(key, value, -c(gender, parent_ptile)) %>%
  mutate(key2 = case_when(key == "F1" ~ "Partnership Rate (PR)", 
                         key == "F2" ~ "Partner LFP Rate | Partnered (PLFR)",
                         key == "F3" ~ "PE > (Parent - Personal) | PR, PLFR", 
                         key == "F22" ~ "Partner LFP Rate | Partnered (PLFR)",
                         key == "F32" ~ "PE > (Parent - Personal) | PR, PLFR", 
                         key == "mob" ~ "Partner Earnings (PE) > (Parent - Personal)"), 
         ft = ifelse(key %in% c("F1", "F2", "F3", "mob"), "In Labor Force", "Working Full-Time"), 
         key2 = factor(key2, levels = c("Partner Earnings (PE) > (Parent - Personal)", 
                                        "Partnership Rate (PR)",
                                        "Partner LFP Rate | Partnered (PLFR)",
                                        "PE > (Parent - Personal) | PR, PLFR"))) %>%
  filter(ft == "In Labor Force") %>%
  ggplot() +
  geom_point(aes(x = parent_ptile, y = value, shape = gender, color = gender)) +
  geom_line(aes(x = parent_ptile, y = value, color = gender)) +
  scale_color_manual(values = c("black", "black")) +
  facet_grid(cols = vars(key2)) + 
  labs(color = "",
       shape = "Income Type",
       x = "Parent Income Percentile",
       y = "Percent", 
       title = "Decomposition of Share, Partner Earnings Close Gap if Personal Earnings < Parent Income",
       caption = "Data from the Panel Study of Income Dynamics, SRC Sample. Sample includes reference persons and spouses born 1950-1980. \n Childhood income is measured at ages 10-18. Adult income is measured at ages 30-45. Estimates are binned every 10th percentile. "
  ) +
  theme_minimal() +  
  scale_x_continuous(breaks = seq(20, 100, 20)) +
  geom_hline(yintercept = 1, linetype = "dotted", color = "red") +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_blank(),
    plot.title = element_text(size = 14, hjust = .5, face = "bold"),
    strip.text = element_text(size = 8),
    legend.text=element_text(size = 8),
    plot.caption = element_text(face = "italic", size = 8),
    axis.text.x =element_text(size = 8, angle = 45),
    axis.text.y =element_text(size = 8)
  ) +
  guides(color = "none")

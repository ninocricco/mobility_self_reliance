test <- gen_data(
  offspring_age_range = list("offspring_ages" = 25:50), 
  cohorts = list("1950-1959" = 1950:1959,
                 "1960-1969" = 1960:1969, 
                 "1970-1979" = 1970:1979))$offspring_panel %>%
  filter(complete.cases(parent_hdsp_income))

test_perm <- gen_data(
  offspring_age_range = list("offspring_ages" = 25:50), 
  cohorts = list("1950-1959" = 1950:1959,
                 "1960-1969" = 1960:1969, 
                 "1970-1979" = 1970:1979))$offspring_perm %>%
  select(person, cohort, parent_hdsp_income_quintile)

test_lifetime <- test %>% 
  select(person, age, year, female, cohort,
         own_income, spouse_income, hdsp_income) %>%
  filter(hdsp_income > 0) %>%
  arrange(person, age) %>%
  group_by(person) %>%
  summarise(across(c(own_income, spouse_income, hdsp_income),
                ~ sum(.), .names = "{.col}_lifetime")) 

test_cumulative <- test %>% 
  select(person, age, year, female, cohort, 
         married, not_in_lf,
         own_income, spouse_income, hdsp_income) %>%
  arrange(person, age) %>%
  group_by(person) %>%
  mutate(across(c(own_income, spouse_income, hdsp_income),
                ~ cumsum(.), .names = "{.col}_cum")) %>%
  mutate(share_cum_own = own_income_cum/hdsp_income_cum, 
         share_cum_spouse = spouse_income_cum/hdsp_income_cum, 
         share_cum_other = 1 - (share_cum_own + share_cum_spouse)) %>%
  ungroup() %>%
  left_join(., test_perm %>% distinct(), by = c("person", "cohort")) %>%
  left_join(., test_lifetime, by = c("person")) %>%
  mutate(across(c(own_income_cum, spouse_income_cum, hdsp_income_cum),
                ~ .x/hdsp_income_lifetime, .names = "{.col}_lifetime")) %>%
  mutate(across(c(own_income, spouse_income, hdsp_income, 
                  share_cum_own, share_cum_spouse, share_cum_other, 
                  own_income_cum, spouse_income_cum, hdsp_income_cum, 
                  own_income_cum_lifetime, spouse_income_cum_lifetime, hdsp_income_cum_lifetime),
                ~ replace_na(.x, 0)))

test_cumulative %>%
  ungroup() %>%
  group_by(age, cohort, female, parent_hdsp_income_quintile) %>%
  summarise(across(c(own_income_cum, spouse_income_cum), 
                ~ mean(.)/1000)) %>%
  filter(complete.cases(parent_hdsp_income_quintile)) %>%
  gather(key, value, -c(age, female, cohort, parent_hdsp_income_quintile)) %>%
  mutate(female = ifelse(female == 1, "Women", "Men"), 
         parent_hdsp_income_quintile = case_when(parent_hdsp_income_quintile == 1 ~ "<25th", 
                                                 parent_hdsp_income_quintile == 2 ~ "25th-50th",
                                                 parent_hdsp_income_quintile == 3 ~ "50th-75th",
                                                 parent_hdsp_income_quintile == 4 ~ ">=75th"),
         parent_hdsp_income_quintile = factor(parent_hdsp_income_quintile, 
                                              levels = c("<25th", "25th-50th", "50th-75th", ">=75th")),
         key = ifelse(key == "own_income_cum", "Personal Earnings", "Partner Earnings")) %>%
  mutate(value = ifelse(cohort == "1970-1979" & age > 45, NA, value)) %>%
  filter(cohort == "1950-1959") %>%
  ggplot() +
  geom_area(aes(x = age, y = value, 
                fill = key)) +
  facet_grid(cols = vars(parent_hdsp_income_quintile), 
             rows = vars(female)) +
  labs(color = "",
       x = "Age",
       y = "Cumulative Earnings, $ (Thousands)", 
       title = "Cumulative Labor Income by Source, by Gender and Social Origin",
       caption = "Data from the Panel Study of Income Dynamics, SRC Sample. \n Sample includes reference persons and spouses born 1950-1959. \n Origin quartile is measured with average household income at ages 10-18. "
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

test_cumulative %>%
  ungroup() %>%
  group_by(age, cohort, female, parent_hdsp_income_quintile) %>%
  summarise(across(c(share_cum_own), 
                   ~ mean(.))) %>%
  filter(complete.cases(parent_hdsp_income_quintile)) %>%
  mutate(female = ifelse(female == 1, "Women", "Men"), 
         parent_hdsp_income_quintile = case_when(parent_hdsp_income_quintile == 1 ~ "<25th", 
                                                 parent_hdsp_income_quintile == 2 ~ "25th-50th",
                                                 parent_hdsp_income_quintile == 3 ~ "50th-75th",
                                                 parent_hdsp_income_quintile == 4 ~ ">=75th"),
         parent_hdsp_income_quintile = factor(parent_hdsp_income_quintile, 
                                              levels = c("<25th", "25th-50th", "50th-75th", ">=75th"))) %>%
  filter(cohort == "1950-1959") %>%
  filter(female == "Women") %>%
  ggplot(aes(x = age, y = share_cum_own)) +
  geom_point() +
  geom_smooth(method = "gam") +
  facet_grid(cols = vars(parent_hdsp_income_quintile)) +
  labs(color = "",
       x = "Age",
       y = "Percent", 
       title = "Share of Women's Cumulative Income by Source, by Social Origin",
       caption = "Data from the Panel Study of Income Dynamics, SRC Sample. \n Sample includes reference persons and spouses born 1950-1959. \n Origin quartile is measured with average household income at ages 10-18. "
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

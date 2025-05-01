source("jobs/1-generate-analysis-data.R")

df_cum <- gen_data(
  offspring_age_range = list("offspring_ages" = 25:50), 
  cohorts = list("1950-1959" = 1950:1959,
                 "1960-1969" = 1960:1969, 
                 "1970-1979" = 1970:1979))$offspring_panel %>%
  left_join(., gen_data(
    offspring_age_range = list("offspring_ages" = 25:50), 
    cohorts = list("1950-1959" = 1950:1959,
                   "1960-1969" = 1960:1969, 
                   "1970-1979" = 1970:1979))$offspring_perm %>%
      select(person, cohort, parent_hdsp_income_pct_rank) %>%
      distinct(), 
    by = c("person", "cohort")
      ) %>%
  filter(complete.cases(parent_hdsp_income_pct_rank)) %>%
  group_by(person) %>%
  mutate(
    first_age = age[which.min(year)],               
    first_year = min(year)#,                        
    #age = first_age + (year - first_year)
  ) %>%
  ungroup() %>%
  mutate(own_lf = ifelse(not_in_lf == 1, 0, 1), 
         partner_lf = ifelse(not_in_lf_spouse == 1, 0, 1),
         parent_ptile = as.numeric(as.character(
           factor(cut(parent_hdsp_income_pct_rank,
                      breaks = seq(0, 100, by = 10),
                      include.lowest = TRUE,
                      right = FALSE),
                  labels = seq(10, 100, by = 10))))) %>%
  rename(partnered = married, partner_income = spouse_income) %>%
  select(person, age, year, female, cohort, parent_ptile,
         parent_hdsp_income_pct_rank, partnered,
         own_lf, partner_lf,
         own_income, partner_income, hdsp_income) %>%
  arrange(person, age) %>%
  group_by(person) %>%
  mutate(count = row_number()) %>%
  ungroup() %>%
  group_by(person, female, cohort, parent_hdsp_income_pct_rank) %>%
  mutate(
    cum_own_earnings = cumsum(own_income)/count,
    cum_partner_earnings = cumsum(partner_income)/count,
    cum_total_income = cum_own_earnings + cum_partner_earnings,
    cum_years_partnered = cumsum(partnered)/count,
    cum_years_partner_LF = cumsum(partner_lf)/count,
    cum_years_partner_LF = ifelse(cum_years_partner_LF > cum_years_partnered, 
                                  cum_years_partnered, cum_years_partner_LF),
    partner_LF_share = if_else(cum_years_partnered > 0,
                               cum_years_partner_LF / cum_years_partnered, 0),
    partner_earnings_intensity = if_else(cum_years_partner_LF > 0,
                                         cum_partner_earnings / cum_years_partner_LF, 0),
  ) %>%
  ungroup()

df_cum %>%
  group_by(parent_ptile, age, female) %>%
  summarise(own = mean(own_income),
            partner = mean(partner_income),
            age = factor(age)) %>%
  filter(age %in% c(25, 35, 45, 50)) %>%
  gather(source, value, -c(parent_ptile, age, female)) %>%
  pivot_wider(names_from = c(female, source), values_from = value)
  ggplot(aes(x = parent_ptile, y = value, linetype = female, color = age)) +
  geom_line() +
  facet_grid(rows = vars(source))

library(splines)

# OWN EARNINGS MODEL (flexible)
own_mod_25 <- lm(cum_own_earnings ~ 
                bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                   degree = 3)*female,
              data=df_cum %>% filter(age == 25))

own_mod_35 <- lm(cum_own_earnings ~ 
                   bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                      degree = 3)*female,
                 data=df_cum %>% filter(age == 35))

own_mod_45 <- lm(cum_own_earnings ~ 
                   bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                      degree = 3)*female,
                 data=df_cum %>% filter(age == 45))

own_mod_50 <- lm(cum_own_earnings ~ 
                   bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                      degree = 3)*female,
                 data=df_cum %>% filter(age == 50))


# PARTNER EARNINGS MODEL (flexible)
partner_mod_25 <- lm(cum_partner_earnings ~ 
                       bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                          degree = 3)*female,
                     data=df_cum %>% filter(age == 25))

partner_mod_35 <- lm(cum_partner_earnings ~ 
                   bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                      degree = 3)*female,
                 data=df_cum %>% filter(age == 35))

partner_mod_45 <- lm(cum_partner_earnings ~ 
                   bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                      degree = 3)*female,
                 data=df_cum %>% filter(age == 45))

partner_mod_50 <- lm(cum_partner_earnings ~ 
                   bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                      degree = 3)*female,
                 data=df_cum %>% filter(age == 50))

# YEARS PARTNERED MODEL (flexible)
years_partnered_mod_25 <- lm(cum_years_partnered ~ bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                                                  degree = 3)*female, data=df_cum %>% filter(age == 25))

years_partnered_mod_30 <- lm(cum_years_partnered ~ bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                                                   degree = 3)*female, data=df_cum %>% filter(age == 30))

years_partnered_mod_45 <- lm(cum_years_partnered ~ bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                                                   degree = 3)*female, data=df_cum %>% filter(age == 45))

years_partnered_mod_50 <- lm(cum_years_partnered ~ bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                                                       degree = 3)*female, data=df_cum %>% filter(age == 50))

partner_lf_mod_25 <- lm(partner_LF_share ~ bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                                           degree = 3)*female,
                     data= df_cum %>% filter(cum_years_partnered > 0, age == 25))

partner_lf_mod_30 <- lm(partner_LF_share ~ bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                                              degree = 3)*female,
                        data= df_cum %>% filter(cum_years_partnered > 0, age == 30))

partner_lf_mod_45 <- lm(partner_LF_share ~ bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                                              degree = 3)*female,
                        data= df_cum %>% filter(cum_years_partnered > 0, age == 45))

partner_lf_mod_50 <- lm(partner_LF_share ~ bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                                              degree = 3)*female,
                        data= df_cum %>% filter(cum_years_partnered > 0, age == 50))

partner_earnings_mod_25 <- lm(partner_earnings_intensity ~ bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                                                           degree = 3)*female,
                           data=df_cum %>% filter(cum_years_partnered>0, age == 25))

partner_earnings_mod_35 <- lm(partner_earnings_intensity ~ bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                                                              degree = 3)*female,
                              data=df_cum %>% filter(cum_years_partnered>0, age == 35))

partner_earnings_mod_45 <- lm(partner_earnings_intensity ~ bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                                                              degree = 3)*female,
                              data=df_cum %>% filter(cum_years_partnered>0, age == 45))

partner_earnings_mod_50 <- lm(partner_earnings_intensity ~ bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                                                              degree = 3)*female,
                              data=df_cum %>% filter(cum_years_partnered>0, age == 50))

partner_earnings_lfmod_25 <- lm(partner_earnings_intensity ~ bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                                                              degree = 3)*female + partner_LF_share,
                              data=df_cum %>% filter(cum_years_partnered>0, age == 25))

partner_earnings_lfmod_35 <- lm(partner_earnings_intensity ~ bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                                                              degree = 3)*female + partner_LF_share,
                              data=df_cum %>% filter(cum_years_partnered>0, age == 35))

partner_earnings_lfmod_45 <- lm(partner_earnings_intensity ~ bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                                                              degree = 3)*female + partner_LF_share,
                              data=df_cum %>% filter(cum_years_partnered>0, age == 45))

partner_earnings_lfmod_50 <- lm(partner_earnings_intensity ~ bs(parent_hdsp_income_pct_rank, knots = c(25, 75), 
                                                              degree = 3)*female + partner_LF_share,
                              data=df_cum %>% filter(cum_years_partnered>0, age == 50))



parent_income_grid <- seq(0, 100, length=100)

pred_df <- data.frame(parent_hdsp_income_pct_rank = c(seq(1, 100, 1), seq(1,100,1)),
                      female = factor(c(rep(0, 100), rep(1, 100))), 
                      partner_LF_share = 1)

pred_df$pred_own_25 <- predict(own_mod_25, newdata=pred_df)
pred_df$pred_own_35 <- predict(own_mod_35, newdata=pred_df)
pred_df$pred_own_45 <- predict(own_mod_45, newdata=pred_df)
pred_df$pred_own_50 <- predict(own_mod_50, newdata=pred_df)

pred_df$pred_partner_25 <- predict(partner_mod_25, newdata=pred_df)
pred_df$pred_partner_35 <- predict(partner_mod_35, newdata=pred_df)
pred_df$pred_partner_45 <- predict(partner_mod_45, newdata=pred_df)
pred_df$pred_partner_50 <- predict(partner_mod_50, newdata=pred_df)

pred_df$pred_partstat_25  <- predict(years_partnered_mod_25, newdata=pred_df)
pred_df$pred_partstat_30  <- predict(years_partnered_mod_30, newdata=pred_df)
pred_df$pred_partstat_45  <- predict(years_partnered_mod_45, newdata=pred_df)
pred_df$pred_partstat_50  <- predict(years_partnered_mod_50, newdata=pred_df)

pred_df$pred_partstat_lf_mod_25  <- predict(partner_lf_mod_25, newdata=pred_df)
pred_df$pred_partstat_lf_mod_30  <- predict(partner_lf_mod_30, newdata=pred_df)
pred_df$pred_partstat_lf_mod_45  <- predict(partner_lf_mod_45, newdata=pred_df)
pred_df$pred_partstat_lf_mod_50  <- predict(partner_lf_mod_50, newdata=pred_df)

pred_df$pred_partner_earnings_mod_25  <- predict(partner_earnings_mod_25, newdata=pred_df)
pred_df$pred_partner_earnings_mod_35 <- predict(partner_earnings_mod_35, newdata=pred_df)
pred_df$pred_partner_earnings_mod_45  <- predict(partner_earnings_mod_45, newdata=pred_df)
pred_df$pred_partner_earnings_mod_50  <- predict(partner_earnings_mod_50, newdata=pred_df)

pred_df$pred_partner_earnings_lfmod_25  <- predict(partner_earnings_lfmod_25, newdata=pred_df)
pred_df$pred_partner_earnings_lfmod_35 <- predict(partner_earnings_lfmod_35, newdata=pred_df)
pred_df$pred_partner_earnings_lfmod_45  <- predict(partner_earnings_lfmod_45, newdata=pred_df)
pred_df$pred_partner_earnings_lfmod_50  <- predict(partner_earnings_lfmod_50, newdata=pred_df)



pred_df %>%
  select(parent_hdsp_income_pct_rank, female, 
         starts_with("pred_partner")) %>%
  gather(Age, value, -c(parent_hdsp_income_pct_rank, female)) %>%
  mutate(key = str_sub(Age, 1, -4),
         key = case_when(key == "pred_partner" ~ "Partner Earnings, Observed", 
                         key == "pred_partner_earnings_mod" ~ "Partner Earnings, Partnered",
                         key == "pred_partner_earnings_lfmod" ~ "Partner Earnings, Partnered, Working"),
         Age = as.numeric(str_sub(Age, -2, -1)),
         female = ifelse(female == 1, "Women", "Men")) %>%
  left_join(., pred_df %>%
              select(parent_hdsp_income_pct_rank, female, 
                     starts_with("pred_own")) %>%
              gather(Age, value_own, -c(parent_hdsp_income_pct_rank, female)) %>%
              mutate(Age = as.numeric(str_sub(Age, -2, -1)),
                     female = ifelse(female == 1, "Women", "Men")), 
            by = c("parent_hdsp_income_pct_rank", "female", "Age")) %>%
  bind_rows(., pred_df %>%
              select(parent_hdsp_income_pct_rank, female, 
                     starts_with("pred_own")) %>%
              gather(Age, value, -c(parent_hdsp_income_pct_rank, female)) %>%
              mutate(key = str_sub(Age, 1, -4),
                     key = case_when(key == "pred_own" ~ "Personal Earnings"),
                     Age = as.numeric(str_sub(Age, -2, -1)),
                     female = ifelse(female == 1, "Women", "Men"))) %>%
  mutate(colorline = ifelse(key == "Personal Earnings", "blue", "orange")) %>%
  arrange(desc(Age)) %>%
  ggplot(aes(x=parent_hdsp_income_pct_rank, y=value / 1000, 
             linetype = key, color = colorline)) +
  geom_line() +
  facet_grid(cols = vars(Age), rows = vars(female)) + 
  scale_linetype_manual(values = c("solid","dashed", "dotted", "solid")) +
  scale_color_manual(values = c("steelblue4", "orange2")) +
  geom_vline(xintercept=c(25, 75), linetype="dashed", color="gray") +
  theme_minimal() +
  labs(
    x = "Parent Income Percentile",
    y = "Cumulative Earnings/Cumulative Years Observed",
    title =
      "Predicted Cumulative Partner Earnings by Age, Gender, and Social Origins",
    caption =
      "Panel Study of Income Dynamics, SRC sample.\n Vertical dashed lines indicate knots (25%, 50%, 75% quantiles)",
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box      = "vertical",
    legend.title = element_blank(),
    plot.title      = element_text(size = 14, hjust = .5, face = "bold"),
    strip.text      = element_text(size = 12),
    legend.text     = element_text(size = 12),
    axis.text.x     = element_text(size = 8, angle = 45),
    axis.text.y     = element_text(size = 8)
  ) +
  guides(color = "none",
         linetype = guide_legend(nrow = 2))

pred_df %>%
  select(parent_hdsp_income_pct_rank, female, starts_with("pred_own")) %>%
  gather(Age, value, -c(parent_hdsp_income_pct_rank, female)) %>%
  mutate(Age = as.numeric(str_sub(Age, -2, -1)),
         female = ifelse(female == 1, "Women", "Men")) %>%
  arrange(desc(Age)) %>%
  ggplot(aes(x=parent_hdsp_income_pct_rank, y=value/1000, 
                    fill = Age, color = Age)) +
  #geom_col(position = "identity") +
  facet_grid(cols = vars(female)) + 
  #scale_fill_continuous(type = "viridis") +
  scale_color_continuous(type = "viridis") +
  geom_vline(xintercept=c(25, 75), linetype="dashed", color="gray") +
  theme_minimal() +
  labs(
    x = "Parent Income Percentile",
    y = "Cumulative Partner Earnings ($ thousands)",
    title =
      "Predicted Cumulative Partner Earnings by Age, Gender, and Social Origins",
    caption =
      "Panel Study of Income Dynamics, SRC sample.\n Vertical dashed lines indicate knots (25%, 50%, 75% quantiles)",
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box      = "vertical",
    plot.title      = element_text(size = 14, hjust = .5, face = "bold"),
    strip.text      = element_text(size = 12),
    legend.text     = element_text(size = 12),
    axis.text.x     = element_text(size = 8, angle = 45),
    axis.text.y     = element_text(size = 8)
  ) +
  guides(color = "none") +
  ylim(0, 2700)



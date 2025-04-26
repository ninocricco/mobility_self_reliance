########################################################################
##  Packages
########################################################################
library(lme4)          # mixed-effects engine
library(dplyr)         # wrangling
library(broom.mixed)   # tidy() for lmer objects
library(ggplot2)       # plots

########################################################################
##  Data: 1950-59 cohort, ages 25-50
########################################################################
df <- gen_data(
  offspring_age_range = list("offspring_ages" = 25:50),
  cohorts             = list("1950-1959"      = 1950:1959)
)$offspring_panel %>% 
  filter(complete.cases(parent_hdsp_income)) %>% 
  mutate(
    log_age    = log(age - 24),                 # age 25 → 0
    log_parent = log(parent_hdsp_income + 1),
    partnered  = as.integer(married),
    not_lf         = as.integer(not_in_lf),          # 1 = NOT in LF
    not_lf_spouse = ifelse(is.na(not_in_lf_spouse), 0, not_in_lf_spouse) %>%
      as.integer(), # all missing cases are non-partnered
    own_inc    = own_income,
    part_inc   = spouse_income
  )

########################################################################
##  Helper: fit ONE flow model and add σ²-adjusted predictions
########################################################################
fit_baseline <- function(data, dep) {
  
  ## 1 ── create log_flow column for this dep variable
  data$log_flow <- log(data[[dep]] + 1)
  
  ## 2 ── fit the mixed model
  mod <- lmer(
    log_flow ~ log_age + 
      log_parent +
      log_age:log_parent +
      (1 + log_age | person),
    data    = data,
    REML    = FALSE,
    control = lmerControl(optimizer = "bobyqa",
                          optCtrl   = list(maxfun = 2e5))
  )
  
  data[[paste0(dep, "_pred_baseline")]] <- exp(predict(mod, newdata = data)) - 1
  return(list("pred_data" = data, 
              "model" = mod))
}

fit_flow <- function(data, dep) {
  
  ## 1 ── create log_flow column for this dep variable
  data$log_flow <- log(data[[dep]] + 1)
  
  ## 2 ── fit the mixed model
  mod <- lmer(
    log_flow ~ log_age + partnered +
      not_lf + not_lf_spouse +
      log_parent +
      log_age:log_parent +
      (1 + log_age | person),
    data    = data,
    REML    = FALSE,
    control = lmerControl(optimizer = "bobyqa",
                          optCtrl   = list(maxfun = 2e5))
  )
  
  data[[paste0(dep, "_pred_adj")]] <- exp(predict(mod, newdata = data)) - 1
  return(list("pred_data" = data, 
              "model" = mod))
}

########################################################################
##  1.  Women
########################################################################
df_women <- df %>% filter(female == 1)

df_women <- fit_flow(df_women, "own_inc")$pred_data   # own earnings
df_women <- fit_flow(df_women, "part_inc")$pred_data  # partner earnings
df_women <- fit_baseline(df_women, "own_inc")$pred_data   # own earnings
df_women <- fit_baseline(df_women, "part_inc")$pred_data  # partner earnings


df_women <- df_women %>% 
  arrange(person, age) %>% 
  group_by(person) %>% 
  mutate(across(c(own_inc_pred_baseline, part_inc_pred_baseline, 
                  own_inc_pred_adj, part_inc_pred_adj), 
                ~ cumsum(.), .names = "cum_{.col}")) %>%
  mutate(
    cum_tot_pred_baseline  = cum_own_inc_pred_baseline + cum_own_inc_pred_baseline,
    cum_tot_pred_adj  = cum_own_inc_pred_adj + cum_own_inc_pred_adj
  ) %>% 
  ungroup() %>% 
  mutate(sex = "Women")

########################################################################
##  2.  Men
########################################################################
df_men <- df %>% filter(female == 0)

df_men <- fit_flow(df_men, "own_inc")$pred_data   # own earnings
df_men <- fit_flow(df_men, "part_inc")$pred_data  # partner earnings
df_men <- fit_baseline(df_men, "own_inc")$pred_data   # own earnings
df_men <- fit_baseline(df_men, "part_inc")$pred_data  # partner earnings


df_men <- df_men %>% 
  arrange(person, age) %>% 
  group_by(person) %>% 
  mutate(across(c(own_inc_pred_baseline, part_inc_pred_baseline, 
                  own_inc_pred_adj, part_inc_pred_adj), 
                ~ cumsum(.), .names = "cum_{.col}")) %>%
  mutate(
    cum_tot_pred_baseline  = cum_own_inc_pred_baseline + cum_own_inc_pred_baseline,
    cum_tot_pred_adj  = cum_own_inc_pred_adj + cum_own_inc_pred_adj
  ) %>% 
  ungroup() %>% 
  mutate(sex = "Men")

########################################################################
##  3.  Combine & add parent-quintile info (unchanged)
########################################################################
df_pred <- bind_rows(df_men, df_women) %>% 
  left_join(., test_perm %>% distinct(), by = c("person", "cohort")
  )

########################################################################
##  4.  Prepare prediction & observed data frames (unchanged)
########################################################################
pred_plot_data <- df_pred %>% 
  group_by(sex, age, parent_hdsp_income_quintile) %>% 
  summarise(
    mean_cum_own.baseline  = mean(cum_own_inc_pred_baseline,  na.rm = TRUE),
    mean_cum_part.baseline = mean(cum_part_inc_pred_baseline, na.rm = TRUE),
    mean_cum_own.adj  = mean(cum_own_inc_pred_adj,  na.rm = TRUE),
    mean_cum_part.adj = mean(cum_part_inc_pred_adj, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  tidyr::pivot_longer(starts_with("mean_cum"),
                      names_to  = "source",
                      values_to = "Predicted") %>% 
  separate(
    col   = source,
    into  = c("source", "model"),
    sep   = "\\.",     
    remove = T) %>%
  mutate(source = recode(source,
                         mean_cum_own  = "Own earnings",
                         mean_cum_part = "Partner earnings"), 
         model = recode(model, 
                        baseline = "Baseline", 
                        adj = "Adjusted"))

pred_obs <- pred_plot_data %>% 
  left_join(
    test_cumulative %>% 
      ungroup() %>% 
      filter(cohort == "1950-1959",
             complete.cases(parent_hdsp_income_quintile)) %>% 
      group_by(age, female, parent_hdsp_income_quintile) %>% 
      summarise(across(c(own_income_cum, spouse_income_cum), mean),
                .groups = "drop") %>% 
      rename(sex = female) %>% 
      tidyr::pivot_longer(cols = c(own_income_cum, spouse_income_cum),
                          names_to  = "source",
                          values_to = "Observed") %>% 
      mutate(source = ifelse(source == "own_income_cum",
                             "Own earnings", "Partner earnings"),
             sex = ifelse(sex == 0, "Men", "Women")),
    by = c("sex", "age", "parent_hdsp_income_quintile", "source")
  ) %>%
  pivot_wider(names_from = model, values_from = Predicted)

########################################################################
##  5.  Plot
########################################################################
pred_obs %>% 
  mutate(parent_hdsp_income_quintile = case_when(
    parent_hdsp_income_quintile == 1 ~ "<25th",
    parent_hdsp_income_quintile == 2 ~ "25th-50th",
    parent_hdsp_income_quintile == 3 ~ "50th-75th",
    parent_hdsp_income_quintile == 4 ~ ">=75th"),
    parent_hdsp_income_quintile =
      factor(parent_hdsp_income_quintile,
             levels = c("<25th","25th-50th","50th-75th",">=75th"))) %>% 
  tidyr::pivot_longer(cols = c(Observed, Baseline, Adjusted),
                      names_to  = "key",
                      values_to = "value") %>% 
  ggplot(aes(x = age, y = value/1000,
             colour = source, linetype = key)) +
  geom_line() +
  facet_grid(cols = vars(parent_hdsp_income_quintile),
             rows = vars(sex)) +
  labs(
    x = "Age",
    y = "Cumulative earnings ($ thousands)",
    colour = "",
    linetype = "",
    title =
      "Model predictions, cumulative income by source, gender and social origins",
    caption =
      "Panel Study of Income Dynamics, SRC sample.\n"  ,
  ) +
  scale_colour_manual(values = c("steelblue4", "orange2")) +
  scale_linetype_manual(values = c("dotted", "twodash", "solid")) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.box      = "vertical",
    plot.title      = element_text(size = 14, hjust = .5, face = "bold"),
    strip.text      = element_text(size = 12),
    legend.text     = element_text(size = 12),
    axis.text.x     = element_text(size = 8, angle = 45),
    axis.text.y     = element_text(size = 8)
  )

men_own_baseline <- fit_baseline(df_men, "own_inc")$model
women_own_baseline <- fit_baseline(df_women, "own_inc")$model
men_own_adj <- fit_flow(df_men, "own_inc")$model
women_own_adj <- fit_flow(df_women, "own_inc")$model

men_part_baseline <- fit_baseline(df_men, "part_inc")$model
women_part_baseline <- fit_baseline(df_women, "part_inc")$model
men_part_adj <- fit_flow(df_men, "part_inc")$model
women_part_adj <- fit_flow(df_women, "part_inc")$model

fe_table_own <- tidy(
  men_own_adj, effects = "fixed", conf.int = T) %>% 
  mutate(sex = "Men", model = "own_income") %>%
  bind_rows(., tidy(women_own_adj, effects = "fixed", conf.int = T) %>% 
  mutate(sex = "Women", model = "own_income")) %>%
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ exp(.) - 1))

fe_table_part <- tidy(
  men_part_adj, effects = "fixed", conf.int = T) %>% 
  mutate(sex = "Men", model = "partner_income") %>%
  bind_rows(., tidy(women_part_adj, effects = "fixed", conf.int = T) %>% 
              mutate(sex = "Women", model = "partner_income")) %>%
  mutate(across(c(estimate, conf.low, conf.high), 
                ~ exp(.) - 1))


library(dplyr)
library(tidyr)
library(purrr)
library(lme4)
library(ggplot2)

# ─────────────────────────────────────────────────────────────────────────────
# 1. prepare data splits
df_men   <- df %>% filter(female == 0)
df_women <- df %>% filter(female == 1)

# ─────────────────────────────────────────────────────────────────────────────
# 2. helper to fit baseline vs adjusted for a given flow
fit_model <- function(data, dep, model_type) {
  if(model_type == "baseline") {
    fit_baseline(data, dep)$model
  } else {
    fit_flow(data, dep)$model
  }
}

# ─────────────────────────────────────────────────────────────────────────────
# 3. build a tibble of all 8 models
models_tbl <- crossing(
  sex        = c("Men","Women"),
  source     = c("own","part"),
  model_type = c("baseline","adj")) %>%
  mutate(data = map(sex, ~ if(.x=="Men") df_men else df_women),
         dep = if_else(source=="own","own_inc","part_inc"),
         model = pmap(list(data, dep, model_type), fit_model)) %>%
  select(-data)

# ─────────────────────────────────────────────────────────────────────────────
# 4. extract variance components
get_vars <- function(mod) {
  vc <- as.data.frame(VarCorr(mod)$person) %>%
    rename(intercept = "(Intercept)") %>%
    mutate(cov = lag(log_age), 
           intercept = lag(intercept)) %>%
    filter(rownames(.) == "log_age") %>%
    mutate(residual = sigma(mod) ^2) %>%
    rename(slope = log_age)
  
  return(vc)
}

var_tbl <- models_tbl %>%
  mutate(vars = map(model, get_vars)) %>%
  unnest(vars)

# ─────────────────────────────────────────────────────────────────────────────
# 5. compute % variance explained  
pct_var <- var_tbl %>%
  select(-c(model, cov)) %>%
  pivot_wider(names_from = model_type,
              values_from = c(intercept,slope,residual)) %>%
  mutate(pct_exp_int = 1-(intercept_adj/intercept_baseline),
         pct_exp_slope = 1-(slope_adj/slope_baseline),
         pct_exp_resid = 1-(residual_adj/residual_baseline)) %>%
  select(sex, source,
         starts_with("pct")) 

# ─────────────────────────────────────────────────────────────────────────────
# 7. extract BLUPs and plot heterogeneity vs parent income
blups_tbl <- models_tbl %>%
  mutate(
    blups = map(model, ~ ranef(.x)$person %>% 
                  as_tibble(rownames="person") %>%
                  rename(intercept = `(Intercept)`,
                         slope     = log_age))
  ) %>%
  select(sex, source, model_type, blups) %>%
  unnest(blups) %>%
  left_join(df %>% distinct(person, parent_hdsp_income),
            by = c("person")) %>%
  mutate(parent_percentile = ntile(parent_hdsp_income, 100)) %>%
  pivot_longer(c(intercept,slope),
               names_to = "component",
               values_to = "blup") %>%
  mutate(component = ifelse(component == "intercept", "Intercept", "Slope"), 
         source = ifelse(source == "own", "Own Earnings", "Partner Earnings"),
         model_type = ifelse(model_type == "adj", "Adjusted", "Baseline"))

re_var_men <- blups_tbl %>% filter(sex == "Men") %>%
  ggplot( 
  aes(x = parent_percentile, 
      y = blup, colour = model_type)) +
  geom_point(alpha=.2) +
  facet_grid(rows = vars(component), cols = vars(source)) +
  labs(x = "Parent Income",
       y = "Deviation from Population Mean",
       colour = "Model",
       title = "Random effect variance by model, source, and social origins", 
       subtitle = "Men") +
  theme_minimal() +
  scale_color_manual(values = c("dodgerblue2", "darkorange3")) +
  theme(
    legend.position = "bottom",
    legend.box      = "vertical",
    plot.title      = element_text(size = 14, hjust = .5, face = "bold"),
    plot.subtitle      = element_text(size = 12, hjust = .5),
    strip.text      = element_text(size = 12),
    legend.text     = element_text(size = 12),
    axis.text.x     = element_text(size = 8, angle = 45),
    axis.text.y     = element_text(size = 8)
  )


re_var_women <- blups_tbl %>% filter(sex == "Women") %>%
  ggplot( 
    aes(x = parent_percentile, 
        y = blup, colour = model_type)) +
  geom_point(alpha=.2) +
  geom_smooth(method="lm", se=FALSE) +
  facet_grid(rows = vars(component), cols = vars(source)) +
  labs(x = "Parent Income",
       y = "Deviation from Population Mean",
       colour = "Model",
       title = "Random effect variance by model, source, and social origins", 
       subtitle = "Women") +
  theme_minimal() +
  scale_color_manual(values = c("dodgerblue2", "darkorange3")) +
  theme(
    legend.position = "bottom",
    legend.box      = "vertical",
    plot.title      = element_text(size = 14, hjust = .5, face = "bold"),
    plot.subtitle      = element_text(size = 12, hjust = .5),
    strip.text      = element_text(size = 12),
    legend.text     = element_text(size = 12),
    axis.text.x     = element_text(size = 8, angle = 45),
    axis.text.y     = element_text(size = 8)
  )

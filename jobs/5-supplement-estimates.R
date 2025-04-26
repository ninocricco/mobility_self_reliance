#------------------------------------------------------------------------------
# PROJECT: MOBILITY SELF-RELIANCE
# FILE: SUPPLEMENTARY ESTIMATES
# AUTHOR: NINO CRICCO
# CONTACT: ncricco@g.harvard.edu
#------------------------------------------------------------------------------

data <- data$offspring_perm

table_supplement <- 
  generate_supplement_estimates(gen_data()$offspring_perm) %>%
  mutate(model = "Main Specificattion") %>%
  bind_rows(
    generate_supplement_estimates(
      gen_data()$offspring_perm, var_specs = list(
        offspring_own = "offspring_own_income_pct_rank",
        offspring_hdsp = "offspring_hdsp_income_pct_rank",
        parent_hdsp = "parent_hdsp_income_pct_rank"
      )) %>% mutate(model = "Pooled Distributions")) %>%
  bind_rows(
    generate_supplement_estimates(
      gen_data()$offspring_perm, var_specs = list(
        offspring_own = "offspring_own_income_gender_pct_rank",
        offspring_hdsp = "offspring_family_income_gender_pct_rank",
        parent_hdsp = "parent_family_income_gender_pct_rank"
      )) %>% mutate(model = "Family Incomes, All Sources")) %>%
  bind_rows(
    generate_supplement_estimates(
      gen_data(offspring_age_range = list("offspring_ages" = 25:35))$offspring_perm) %>% 
      mutate(model = "Children Age 25-35")) %>%
  bind_rows(
    generate_supplement_estimates(
      gen_data(offspring_age_range = list("offspring_ages" = 35:45))$offspring_perm) %>% 
      mutate(model = "Children Age 35-45")) %>%
  bind_rows(
    generate_supplement_estimates(
      gen_data(parental_age_range = list("parent_ages" = 5:18))$offspring_perm) %>% 
      mutate(model = "Parent Age 5-18, Main Spec")) %>%
    bind_rows(
        generate_supplement_estimates(
          gen_data(familyid_vector = c(1:3000, 5001:6872))$offspring_perm) %>% 
          mutate(model = "Including SEO sample")) %>% 
  bind_rows(
    generate_supplement_estimates(
      gen_data(familyid_vector = c(1:6872))$offspring_perm) %>% 
      mutate(model = "Including SEO and Immigrant sample")) %>%
  bind_rows(
  generate_change_table(data = gen_data(familyid_vector = c(1:3000, 5001:6872))$offspring_perm,
                        by_race = T) %>% 
    pivot_wider(names_from = model_name, values_from = value) %>%
    transmute(Gender, key, family_parent, 
              share_earnings = (own_parent * family_own)/family_parent, 
              own_parent, family_own, race_ever) %>%
    gather(param, value, -c(Gender, key, race_ever)) %>%
    pivot_wider(names_from = c(Gender, param), values_from = value) %>%
    select(key, starts_with("Men"), starts_with("Women"), race_ever) %>%
    rename(model = race_ever)
  )

table_supplement %>%
  select(model, key, starts_with("Men"), everything()) %>%
  # Round all numeric columns to 3 decimal places
  mutate(across(where(is.numeric), ~round(., 3))) %>%
  # Rename columns to be more readable
  rename(
    "Model" = model,
    "Cohort" = key,
    "Parent - Child Family Income" = Men_family_parent,
    "% Share, Earnings" = Men_share_earnings,
    "Parent - Child Earnings" = Men_own_parent,
    "Child Earnings - Child Family Income" = Men_family_own,
    "Parent - Child Family Income " = Women_family_parent,
    "% Share, Earnings " = Women_share_earnings,
    "Parent - Child Earnings " = Women_own_parent,
    "Child Earnings - Child Family Income " = Women_family_own
  ) %>%
  kbl(format = "latex", 
      booktabs = TRUE,
      caption = "Table A1: Supplementary Estimates, All Parameters by Gender and Year",
      align = c('l', 'l', rep('r', 8))) %>%
  kable_styling(latex_options = c("scale_down", "hold_position")) %>%
  add_header_above(c(" " = 2, 
                     "Men" = 4, 
                     "Women" = 4))

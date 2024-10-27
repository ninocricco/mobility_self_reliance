#------------------------------------------------------------------------------
# PROJECT: MOBILITY SELF-RELIANCE
# FILE: T-TESTS
# AUTHOR: NINO CRICCO
# CONTACT: ncricco@g.harvard.edu
#------------------------------------------------------------------------------
add_residuals <- function(data, by_marstat = FALSE) {
  # Group the data according to the by_marstat parameter
  grouped_data <- if(by_marstat) {
    data %>%
      group_by(cohort, female, ever_married)
  } else {
    data %>%
      group_by(cohort, female)
  }
  
  # Add residuals within each group
  result <- grouped_data %>%
    group_modify(~ {
      # Fit model for this specific group
      model <- lm(offspring_hdsp_income_pct_rank ~ 
                    offspring_gender_own_income_pct_rank, 
                  data = .x)
      
      # Add residuals to the group's data
      .x$residuals <- resid(model)
      
      return(.x)
    }) %>%
    ungroup()  # Remove grouping for easier further manipulation
  
  return(result)
}

data <- add_residuals(gen_data())

# Own earnings rank-tests 
summary(lm(offspring_gender_own_income_pct_rank ~ parent_hdsp_income_pct_rank * cohort,
           data = data %>% filter(female == 0)))

summary(lm(offspring_own_income_pct_rank ~ parent_hdsp_income_pct_rank * cohort,
           data = data %>% filter(female == 0)))

summary(lm(offspring_gender_own_income_pct_rank ~ parent_hdsp_income_pct_rank * cohort,
           data = data %>% filter(female == 1)))

summary(lm(offspring_own_income_pct_rank ~ parent_hdsp_income_pct_rank * cohort,
           data = data %>% filter(female == 1)))

summary(lm(offspring_gender_own_income_pct_rank ~ parent_hdsp_income_pct_rank * female,
           data = data %>% filter(cohort == "1950-1969")))

summary(lm(offspring_gender_own_income_pct_rank ~ parent_hdsp_income_pct_rank * female,
           data = data %>% filter(cohort == "1970-1985")))

summary(lm(offspring_own_income_pct_rank ~ parent_hdsp_income_pct_rank * female,
           data = data %>% filter(cohort == "1950-1969")))

summary(lm(offspring_own_income_pct_rank ~ parent_hdsp_income_pct_rank * female,
           data = data %>% filter(cohort == "1970-1985")))

summary(lm(offspring_gender_own_income_pct_rank ~ parent_hdsp_income_pct_rank * female * cohort,
           data = data))

summary(lm(offspring_own_income_pct_rank ~ parent_hdsp_income_pct_rank * female * cohort,
           data = data))


summary(lm(offspring_own_income_pct_rank ~ parent_hdsp_income_pct_rank,
           data = data %>% filter(cohort == "1950-1969")))

# Earnings- Income rank t-tests
# Own earnings rank-tests 
summary(lm(offspring_hdsp_income_pct_rank ~ 
             offspring_gender_own_income_pct_rank * cohort,
           data = data %>% filter(female == 0)))

summary(lm(offspring_hdsp_income_pct_rank ~ 
             offspring_gender_own_income_pct_rank * cohort,
           data = data %>% filter(female == 1)))

summary(lm(offspring_hdsp_income_pct_rank ~ 
             offspring_gender_own_income_pct_rank * female,
           data = data %>% filter(cohort == "1950-1969")))

summary(lm(offspring_hdsp_income_pct_rank ~ 
          offspring_gender_own_income_pct_rank * female,
           data = data %>% filter(cohort == "1970-1985")))

summary(lm(offspring_hdsp_income_pct_rank ~ 
             offspring_gender_own_income_pct_rank * female * cohort,
           data = data))

summary(lm(offspring_hdsp_income_pct_rank ~ 
             offspring_own_income_pct_rank * cohort,
           data = data %>% filter(female == 0)))

summary(lm(offspring_hdsp_income_pct_rank ~ 
             offspring_own_income_pct_rank * cohort,
           data = data %>% filter(female == 1)))

summary(lm(offspring_hdsp_income_pct_rank ~ 
             offspring_own_income_pct_rank * female,
           data = data %>% filter(cohort == "1950-1969")))

summary(lm(offspring_hdsp_income_pct_rank ~ 
             offspring_own_income_pct_rank * female,
           data = data %>% filter(cohort == "1970-1985")))

# Residuals t-test
summary(lm(residuals ~ parent_hdsp_income_pct_rank * cohort,
           data = data %>% filter(female == 0)))

summary(lm(residuals ~ parent_hdsp_income_pct_rank * cohort,
           data = data %>% filter(female == 1)))

summary(lm(residuals ~ parent_hdsp_income_pct_rank * female,
           data = data %>% filter(cohort == "1950-1969")))

summary(lm(residuals ~ parent_hdsp_income_pct_rank * female,
           data = data %>% filter(cohort == "1970-1985")))

summary(lm(residuals ~ parent_hdsp_income_pct_rank * female * cohort,
           data = data))

# Residuals t-test w/pooled earnings


#------------------------------------------------------------------------------
# PROJECT: MOBILITY SELF-RELIANCE
# FILE: GENERATING ANALYSIS DATA: CREATING FEATURES AND RESHAPING
# AUTHOR: NINO CRICCO
# CONTACT: ncricco@g.harvard.edu
#------------------------------------------------------------------------------

# Read in dependencies 
source("jobs/0-raw.R")
source("jobs/0-functions.R")

# Function to convert vectors to named lists for age ranges
create_age_list <- function(ages, prefix) {
  list(setNames(list(ages), prefix))
}

# Create a function that outputs formatted analysis data
# Can change inputs for sample specification: default arguments
# correspond to specs for the main analytic sample in the paper
gen_data <- function(parental_age_range = list("parent_ages" = 10:18),
                     offspring_age_range = list("offspring_ages" = 30:45),
                     cohorts = list("1950-1969" = 1950:1969, "1970-1985" = 1970:1985),
                     familyid_vector = 1:3000,
                     minobs_childhood = 1,
                     minobs_adulthood = 1,
                     outcome = "observed") {
  
  # Handle different input types for age ranges
  if(is.numeric(parental_age_range)) {
    parental_age_range <- create_age_list(parental_age_range, "parent_ages")
  }
  if(is.numeric(offspring_age_range)) {
    offspring_age_range <- create_age_list(offspring_age_range, "offspring_ages")
  }
  if(is.numeric(familyid_vector)) {
    familyid_vector <- as.vector(familyid_vector)
  }
  
  # Extract age ranges
  parent_ages <- unlist(parental_age_range)
  offspring_ages <- unlist(offspring_age_range)
  cohort_filter <- unlist(cohorts)
  cohort_labels <- rep(names(cohorts), sapply(cohorts, length))
  
  filtered_data <- unrestricted %>%
    filter(familyid %in% familyid_vector) %>%
    mutate(age = na_codes(age, 999, 0),
           head_age = na_codes(head_age, 999, 0),
           birthyear = year - age) %>%
    # Use dynamic cohort filter
    filter(birthyear %in% cohort_filter) %>%
    mutate(female = factor(ifelse(sex == 2, 1, 0)),
           period = case_when(
             age >= parent_ages[1] & age <= parent_ages[length(parent_ages)] ~ "parent",
             age >= offspring_ages[1] & age <= offspring_ages[length(offspring_ages)] ~ "offspring"),
           cohort = factor(cohort_labels[match(birthyear, cohort_filter)])) %>%
    # Rest of the function remains the same
    left_join(cpi, by = "year") %>%
    mutate(across(c(family_income, head_income, spouse_income),
                  ~ . * to_multiply)) %>%
    filter(relhead %in% c(1, 2, 3, 9, 10, 20, 22, 30, 33, 35))
  
  # Creating measure of long-run parental income for all individuals and
  # income measures (family, head, spouse, head + spouse income)
  unrestricted_summarized_parent <- filtered_data %>%
    mutate(hdsp_income = head_income + spouse_income) %>%
    # Subset to only observations where respondents are labeled as
    # offspring and in the appropriate observation window 
    filter(period == "parent", relhead %in% c(3, 30, 33, 35)) %>%
    group_by(person, period, cohort, female) %>%
    # Computing mean of parental income measures across observed years
    summarise(
      across(c(family_income, head_income, spouse_income, 
               hdsp_income, head_age, age), 
             ~ mean(.x, na.rm = TRUE), 
             .names = "parent_{.col}"),
      # Indicator for the number of times offspring is observed in parent hh
      num_observedinchildhood = n()) %>%
    group_by(cohort, female)
  
  # Creating offspring data
  unrestricted_offspring <- filtered_data %>%
    filter(period == "offspring", relhead %in% c(1, 2, 10, 20, 22)) %>%
    mutate(hdsp_income = head_income + spouse_income,
           own_income = case_when(
             relhead %in% c(1, 10) ~ head_income,
             relhead %in% c(2, 20, 22) ~ spouse_income),
           spouse_income = hdsp_income-own_income,
           wrkhrs = case_when(
             relhead %in% c(1, 10) ~ head_wrkhrs,
             relhead %in% c(2, 20, 22) ~ spouse_wrkhrs),
           ed = case_when(
             relhead %in% c(1, 10) ~ head_education,
             relhead %in% c(2, 20, 22) ~ spouse_education),
           lfstatus = case_when(
             wrkhrs > 1500 ~ "fulltime",
             wrkhrs > 0 ~ "parttime",
             TRUE ~ "notinlf"), 
           zero_earnings = ifelse(own_income == 0, 1, 0),
           not_in_lf = ifelse(lfstatus == "notinlf", 1, 0), 
           ft = ifelse(lfstatus  == "fulltime", 1, 0),
           own_income_share = own_income/hdsp_income,
           married = ifelse(marstat == 1, 1, 0)) %>%
    left_join(., unrestricted_summarized_parent %>% select(-period),
              by = c("person", "cohort", "female")) %>%
    # Calculate whether they are ever married as we observe them
    group_by(person, female, period, cohort) %>%
    mutate(ever_married = any(marstat == 1), 
           ever_married = ifelse(ever_married == T, 1, 0)) %>%
    # Ungroup to perform further operations
    ungroup()
  
  unrestricted_offspring_summarized <- unrestricted_offspring %>%
    group_by(person, cohort, female) %>%
    summarize(across(c(hdsp_income, own_income, 
                       head_income, spouse_income,
                       family_income, ever_married, 
                        age, own_income_share),
                     ~ mean(.x, na.rm = TRUE)),
              num_observedinadulthood = n(),
              across(c(married, zero_earnings, not_in_lf, ft), 
                     ~ sum(.x))) %>%
    left_join(., unrestricted_offspring %>% 
                group_by(person, cohort, female) %>%
                count(lfstatus) %>% filter(n == max(n)), 
              by = c("person", "cohort", "female")) %>%
    left_join(., unrestricted_summarized_parent,
              by = c("person", "cohort", "female")) %>%
    filter(complete.cases(parent_hdsp_income)) %>%
    # Creating relative measures by cohort pooling men and women
    ungroup() %>% 
    group_by(cohort) %>%
    mutate(across(c(hdsp_income, own_income,
                    head_income, spouse_income, family_income), 
                  ~ rank(., ties.method = "average") / length(.) * 100, 
                  .names = "offspring_{.col}_pct_rank"), 
           across(c(hdsp_income, own_income, 
                    head_income, spouse_income, family_income), 
                  ~ ntile(., 4), 
                  .names = "offspring_{.col}_quintile"), 
           across(c(parent_hdsp_income, parent_family_income, 
                    parent_head_income, parent_spouse_income),
                  ~ rank(., ties.method = "average") / length(.) * 100, 
                  .names = "{.col}_pct_rank"),
           across(c(parent_hdsp_income, parent_family_income, 
                    parent_head_income, parent_spouse_income),
                  ~ ntile(., 4), 
                  .names = "{.col}_quintile")#, 
           #own_income_rank_in_family_dist = ecdf(hdsp_income)(own_income) * 100,
           #dist_own_rank_fam_rank = offspring_hdsp_income_pct_rank-own_income_rank_in_family_dist
           ) %>%
    # Creating relative measures by gender and cohort
    ungroup() %>% group_by(cohort, female) %>%
    mutate(across(c(hdsp_income, own_income, head_income, spouse_income, family_income), 
                  ~ rank(., ties.method = "average") / length(.) * 100, 
                  .names = "offspring_{.col}_gender_pct_rank"),
           across(c(parent_hdsp_income, parent_family_income, 
                    parent_head_income, parent_spouse_income),
                  ~ rank(., ties.method = "average") / length(.) * 100, 
                  .names = "{.col}_gender_pct_rank"), 
           own_income_gender_quintile = ntile(own_income, 4)#,
           #own_income_gender_rank_in_family_dist = ecdf(hdsp_income)(own_income) * 100,
           #dist_own_gender_rank_fam_rank = offspring_hdsp_income_gender_pct_rank-own_income_gender_rank_in_family_dist
           ) %>%
    mutate(own_income_rank_scaled_size = factor(round(
      min_rank(own_income) / n(), digits = 2))) %>%
    ungroup() %>%
    # Creating measure to capture share absolute mobility:
    # % income strictly > than parents
    # comparing joint head+spouse, own to father, mother
    mutate(fam.mob = ifelse(hdsp_income > parent_hdsp_income, 1, 0),
           own.mob = ifelse(own_income > parent_hdsp_income, 1, 0),
           dad.mob = ifelse(own_income > parent_head_income, 1, 0),
           mom.mob = ifelse(own_income > parent_spouse_income, 1, 0)) %>%
    filter(complete.cases(period), 
           num_observedinchildhood >= minobs_childhood,
           num_observedinadulthood >= minobs_adulthood)
  
  return(list("offspring_panel" = unrestricted_offspring,
              "offspring_perm" = unrestricted_offspring_summarized, 
              "parent_perm" = unrestricted_summarized_parent))
  
}


#------------------------------------------------------------------------------
# PROJECT: MOBILITY SELF-RELIANCE
# FILE: GENERATING ANALYSIS DATA: CREATING FEATURES AND RESHAPING
# AUTHOR: NINO CRICCO
# CONTACT: ncricco@g.harvard.edu
#------------------------------------------------------------------------------

# Read in dependencies 
source("jobs/0-raw.R")
source("jobs/0-functions.R")

# Create a function that outputs formatted analysis data
# Can change inputs for sample specification: default arguments
# correspond to specs for the main analytic sample in the paper
gen_data <- function(parental_age_range = 10:18, offspring_age_range = 30:45,
                     cohorts = list("1950-1969" = 1950:1969, "1970-1985" = 1970:1985),
                     familyid_vector = 1:3000, minobs_childhood = 1,
                     minobs_adulthood = 1, outcome = "observed") {
  
  # Create a dynamic cohort filter and label
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
             age >= parental_age_range[1] & age <= parental_age_range[length(parental_age_range)] ~ "parent",
             age >= offspring_age_range[1] & age <= offspring_age_range[length(offspring_age_range)] ~ "offspring"), 
           cohort = factor(cohort_labels[match(birthyear, cohort_filter)])) %>%
    
    # Adjust incomes to 2019 dollars using the CPI
    left_join(cpi, by = "year") %>%
    mutate(across(c(family_income, head_income, spouse_income), 
                  ~ . * to_multiply)) %>%
    # Selecting only individuals observed as heads, spouses, or offspring
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
    group_by(cohort) %>%
    # Ranking long-run parental income by cohort
    mutate(across(c(parent_family_income, parent_head_income,
                    parent_spouse_income, parent_hdsp_income), 
                  ~ rank(., ties.method = "random") / length(.) * 100, 
                  .names = "{.col}_pct_rank")) %>%
    mutate(across(c(parent_family_income, parent_head_income,
                    parent_spouse_income, parent_hdsp_income), 
                  ~ ntile(., 5), 
                  .names = "{.col}_quintile"))
  
  # Creating offspring data
  unrestricted_offspring <- filtered_data %>%
    filter(period == "offspring", relhead %in% c(1, 2, 10, 20, 22)) %>%
    mutate(hdsp_income = head_income + spouse_income,
           own_income = case_when(
             relhead %in% c(1, 10) ~ head_income,
             relhead %in% c(2, 20, 22) ~ spouse_income),
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
    summarize(across(c(hdsp_income, own_income, head_income,
                       spouse_income, ever_married, age),
                     ~ mean(.x, na.rm = TRUE)),
              num_observedinadulthood = n()) %>%
    left_join(., unrestricted_offspring %>% 
                group_by(person, cohort, female) %>%
                count(lfstatus) %>% filter(n == max(n)), 
              by = c("person", "cohort", "female")) %>%
    left_join(., unrestricted_summarized_parent,
              by = c("person", "cohort", "female")) %>%
    # Creating relative measures by cohort pooling men and women
    ungroup() %>% 
    group_by(cohort) %>%
    mutate(across(c(hdsp_income, own_income,
                    head_income, spouse_income), 
                  ~ rank(., ties.method = "random") / length(.) * 100, 
                  .names = "offspring_{.col}_pct_rank")) %>%
    mutate(across(c(hdsp_income, own_income, 
                    head_income, spouse_income), 
                  ~ ntile(., 5), 
                  .names = "offspring_{.col}_quintile")) %>%
    # Creating relative measures by gender and cohort
    ungroup() %>% group_by(cohort, female) %>%
    mutate(across(c(hdsp_income, own_income, head_income, spouse_income), 
                  ~ rank(., ties.method = "random") / length(.) * 100, 
                  .names = "offspring_gender_{.col}_pct_rank"),
           own_income_gender_quintile = ntile(own_income, 5)) %>%
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
  
  men_cutoffs <- unrestricted_offspring_summarized %>%
    filter(female == 0) %>%
    group_by(cohort) %>%
    summarise(own_income_cutoffs = quantile(
      own_income, probs = seq(0, 1, length.out = 101),
      na.rm = TRUE, names = FALSE, type = 5)) %>%
    mutate(percentile = factor(seq(0, 1, length.out = 101))) %>%
    ungroup()
  
  unrestricted_offspring_summarized <- unrestricted_offspring_summarized %>%
    left_join(men_cutoffs, by = c(
      "cohort", "own_income_rank_scaled_size" = "percentile")) %>%
    mutate(
      own_income_sim = if_else(
        cohort == unique(cohort)[2] & female == 1, 
        own_income_cutoffs, own_income), 
      hdsp_income_sim = own_income_sim + spouse_income
      ) %>%
    group_by(cohort) %>%
    mutate(across(c(hdsp_income_sim), 
                  ~ rank(., ties.method = "random") / length(.) * 100, 
                  .names = "offspring_hdsp_income_sim_pct_rank"))
  
  if(outcome == "simulated"){
    unrestricted_offspring_summarized <- unrestricted_offspring_summarized %>%
      mutate(offspring_hdsp_income_pct_rank = case_when(
        cohort == unique(cohort_labels)[2] ~
          offspring_hdsp_income_sim_pct_rank,
        TRUE ~ offspring_hdsp_income_pct_rank
      ))
    }
  
  return(unrestricted_offspring_summarized)
  
}

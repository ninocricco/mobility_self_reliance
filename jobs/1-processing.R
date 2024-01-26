#********************************************************
# PROJECT: MOBILITY SELF-RELIANCE
# FILE: RESHAPING DATA, FEATURE ENGINEERING
# AUTHOR: NINO CRICCO
# CONTACT: ncricco@g.harvard.edu
#********************************************************

source("0-raw.R")

mydata <- unrestricted %>%
  filter(birth_year >= 1960 & birth_year <= 1984) %>% 
  mutate(female = factor(ifelse(sex == 2, 1, 0)),
         period = case_when(age >= 10 & age <= 20 ~ "parent",
                            age >= 25 & age <= 42 ~ "offspring"), 
         cohort = case_when(birth_year <= 1972 ~ "cohort_1",  
                            birth_year <= 1984 ~ "cohort_2")) %>%
  filter(!is.na(period)) %>%
  group_by(person) %>%
  filter(any(period == "parent")) %>%
  mutate(num_observedInChildhood = n_distinct(person)) %>%
  group_by() %>%
  # Adjust incomes to 2016 dollars by the CPI
  left_join(cpi,
            by = "year") %>%
  mutate(across(c(familyIncome, headIncome, spouseIncome), 
                ~ . * to_multiply),
         # Creating measures of log income
         across(c(familyIncome, headIncome, spouseIncome), 
                ~ log(.), 
                .names = "log_{.col}"), 
         across(starts_with("log_"), ~ ifelse(.x == -Inf, 0, .x))) %>%
  filter(relhead %in% c(10, 20, 22, 30, 33, 35))

# Creating a table of parental income for all individuals and 
# income measures (family, "head", spouse income)
unrestricted_summarized_parent <- mydata %>%
  mutate(hdspIncome = headIncome + spouseIncome) %>%
  filter(period == "parent", relhead %in% c(30, 33, 35)) %>%
  group_by(person, period, cohort, female, birth_year) %>%
  summarize(Parent_familyIncome = mean(familyIncome, na.rm = T),
            Parent_headIncome = mean(headIncome, na.rm = T),
            Parent_spouseIncome = mean(spouseIncome, na.rm = T), 
            Parent_hdspIncome = mean(hdspIncome, na.rm = T),
            num_observedInChildhood = n()) %>%
  group_by(cohort) %>%
  mutate(across(c(Parent_familyIncome, Parent_headIncome, Parent_spouseIncome, Parent_hdspIncome), 
                ~ rank(., ties.method = "random") / length(.) * 100, 
                .names = "{.col}_pct_rank")) %>%
  mutate(across(c(Parent_familyIncome, Parent_headIncome, Parent_spouseIncome, Parent_hdspIncome), 
                ~ ntile(., 5), 
                .names = "{.col}_quintile"))
 
unrestricted_offspring <- mydata %>%
  filter(period == "offspring", relhead %in% c(10, 20, 22)) %>%
  mutate(hdspIncome = headIncome + spouseIncome,
         ownIncome = case_when(relhead == 10 ~ headIncome,
                               relhead %in% c(20, 22) ~ spouseIncome),
         married = ifelse(marstat == 1, 1, 0)) %>%
  left_join(., unrestricted_summarized_parent %>% select(-period),
            by = c("person", "cohort", "female", "birth_year")) %>%
  mutate(fam.mob = ifelse(hdspIncome > Parent_hdspIncome, 1, 0),
         own.mob = ifelse(ownIncome > Parent_hdspIncome, 1, 0),
         dad.mob = ifelse(ownIncome > Parent_headIncome, 1, 0),
         mom.mob = ifelse(ownIncome > Parent_spouseIncome, 1, 0)) %>%
  # Calculate whether they are ever married as we observe them
  group_by(person, female, period, cohort) %>%
  mutate(ever_married = any(marstat == 1), 
         ever_married = ifelse(ever_married == T, 1, 0)) %>%
  # Ungroup to perform further operations
  ungroup()

unrestricted_offspring_summarized <- unrestricted_offspring %>%
  group_by(person, cohort, female, birth_year) %>%
  summarize(hdspIncome = mean(hdspIncome, na.rm = T), 
            ownIncome = mean(ownIncome, na.rm = T), 
            headIncome = mean(headIncome, na.rm = T),
            spouseIncome = mean(spouseIncome, na.rm = T), 
            ever_married = mean(ever_married, na.rm = T)) %>%
  left_join(., unrestricted_summarized_parent,
            by = c("person", "cohort", "female", "birth_year")) %>%
  ungroup() %>% group_by(cohort) %>%
  mutate(across(c(hdspIncome, ownIncome, headIncome, spouseIncome), 
                ~ rank(., ties.method = "random") / length(.) * 100, 
                .names = "Offspring_{.col}_pct_rank")) %>%
  mutate(across(c(hdspIncome, ownIncome, headIncome, spouseIncome), 
                ~ ntile(., 5), 
                .names = "Offspring_{.col}_quintile")) %>%
  mutate(across(ends_with('quintile'), 
                ~case_when(
                  . == 1 ~ "20th",
                  . == 2 ~ "40th",
                  . == 3 ~ "60th",
                  . == 4 ~ "80th",
                  . == 5 ~ "80th+",
                  TRUE ~ NA_character_ # for missing or NA values
                ), 
                .names = "{.col}_label")) %>%
  mutate(across(ends_with('label'), 
                ~factor(., levels = c("20th", "40th","60th", "80th","80th+")))) %>%
  mutate(fam.mob = ifelse(hdspIncome > Parent_hdspIncome, 1, 0),
         own.mob = ifelse(ownIncome > Parent_hdspIncome, 1, 0),
         dad.mob = ifelse(ownIncome > Parent_headIncome, 1, 0),
         mom.mob = ifelse(ownIncome > Parent_spouseIncome, 1, 0)) %>%
  ungroup() %>% group_by(cohort, female) %>%
  mutate(across(c(hdspIncome, ownIncome, headIncome, spouseIncome), 
                ~ rank(., ties.method = "random") / length(.) * 100, 
                .names = "Offspring_GenderRef_{.col}_pct_rank")) %>%
  filter(complete.cases(period))


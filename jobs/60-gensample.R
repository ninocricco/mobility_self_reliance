library(tidyverse)
library(splines)

source("jobs/1-generate-analysis-data.R")

data <- gen_data(cohorts = list("1950-1980" = 1950:1980))$offspring_perm %>%
  filter(complete.cases(parent_hdsp_income)) %>%
  mutate(parent_rank = percent_rank(parent_hdsp_income) * 100) %>%
  mutate(
    parent_ptile = as.numeric(
      as.character(
        factor(cut(parent_rank, breaks = seq(0, 100, by = 10),
                   include.lowest = TRUE, right = FALSE),
               labels = seq(10, 100, by = 10)))),
    gender = ifelse(female == 1, "Women", "Men"),
    abs.spouse.mob = ifelse(abs.own.mob == 0 & spouse_income > parent_hdsp_income, 1, 0), 
    abs.spouse.mob.alt = ifelse(spouse_income > parent_hdsp_income, 1, 0),
    abs.own.mob.alt = ifelse(abs.spouse.mob.alt == 0 & own_income > parent_hdsp_income, 1, 0), 
    abs.combined = ifelse(abs.own.mob == 0 & abs.spouse.mob == 0 & abs.fam.mob == 1, 1, 0),
    abs.combined.alt = ifelse(abs.own.mob.alt == 0 & abs.spouse.mob.alt == 0 & abs.fam.mob == 1, 1, 0),
    ever_partner_lf = ifelse(ever_married == 1 & in_lf_spouse == 1, 1, 0),
    partner_rate = married/num_observedinadulthood,
    partner_lf_rate = in_lf_spouse/num_observedinadulthood,
    partner_ft_rate = ft_spouse/num_observedinadulthood
  ) %>%
  filter(parent_rank < 100) %>%
  mutate(test = spouse_income/(partner_rate * partner_lf_rate),
         censor = ifelse(parent_rank < 10 & complete.cases(test) &
                           test > 1000000, 1, 0)) %>%
  filter(censor == 0) 

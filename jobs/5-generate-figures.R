#------------------------------------------------------------------------------
# PROJECT: MOBILITY SELF-RELIANCE
# FILE: GENERATING FIGURES
# AUTHOR: NINO CRICCO
# CONTACT: ncricco@g.harvard.edu
#------------------------------------------------------------------------------

source("jobs/4-figures-function.R")

generate_figures(gen_data(), caption = "Main Specification")

generate_figures(gen_data(offspring_age_range = 30:35),
                 caption = "Offspring Age Window 30-35")

generate_figures(gen_data(parental_age_range = 14:18),
                 caption = "Childhood Age Window 14-18")

generate_figures(gen_data(minobs_childhood = 3,
                          minobs_adulthood = 3), 
                 caption = "Minimum Observations = 3")

generate_figures(gen_data(cohorts = list(
  "1950-1960" = 1950:1960, "1970-1980" = 1970:1980)),
  caption = "Cohorts 1950-1960, 1970-1980")

generate_figures(gen_data(refdist = "pooled"), 
                 caption = "Own Earnings Rank Pooled")

generate_figures(gen_data(familyid_vector = c(1:3000, 5001:6872)), 
                 caption = "Including SEO sample")

generate_figures(gen_data(familyid_vector = c(1:3000, 
                                              3001:3511,
                                              5001:6872)), 
                 caption = "Including SEO + 97-99 Immigrant sample")

generate_figures(gen_data(outcome = "simulated"), 
                 caption = "Simulating Gender Convergence")


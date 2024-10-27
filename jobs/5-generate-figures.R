#------------------------------------------------------------------------------
# PROJECT: MOBILITY SELF-RELIANCE
# FILE: GENERATING FIGURES
# AUTHOR: NINO CRICCO
# CONTACT: ncricco@g.harvard.edu
#------------------------------------------------------------------------------

source("jobs/4-figures-function.R")

generate_fig0(gen_data())

generate_fig1(gen_data())

generate_fig2(gen_data())
generate_fig3(gen_data())
generate_fig4(gen_data())

generate_figures(gen_data(), caption = "Main Specification")

generate_desc_stats_table(gen_data())
generate_params_sims_table(gen_data())

generate_figures(gen_data(offspring_age_range = 30:35),
                 caption = "Offspring Age Window 30-35")

generate_figures(gen_data(offspring_age_range = 40:45),
                 caption = "Offspring Age Window 30-35")


generate_figures(gen_data(parental_age_range = 14:18),
                 caption = "Childhood Age Window 14-18")

generate_figures(gen_data(minobs_childhood = 3,
                          minobs_adulthood = 3), 
                 caption = "Minimum Observations = 3")

generate_figures(gen_data(refdist = "pooled", cohorts = list(
  "1946-1964" = 1946:1960, "1965-1980" = 1965:1980)),
  caption = "Cohorts Baby Boomers, Generation X")

generate_figures(gen_data(refdist = "pooled"), 
                 caption = "Own Earnings Rank Pooled")

generate_fig1(gen_data(refdist = "pooled"))

generate_figures(gen_data(familyid_vector = c(1:3000, 5001:6872)), 
                 caption = "Including SEO sample")

generate_figures(gen_data(familyid_vector = c(1:3000, 
                                              3001:3511,
                                              5001:6872)), 
                 caption = "Including SEO + 97-99 Immigrant sample")

generate_figures(gen_data(outcome = "simulated"), 
                 caption = "Simulating Gender Convergence")

generate_fig1(gen_data())

# Do NLSY comparison that includes other income sources that aren't just wage/salary
generate_figures(gen_data(cohorts = list(
  "NLSY79" = 1957:1964, "NLSY97" = 1980:1984), 
  offspring_age_range = 27:32, parental_age_range = 13:17, minobs_childhood = 2),
  caption = "NLSY Comparison")

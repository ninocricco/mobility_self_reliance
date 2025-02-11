library(ggplot2)
library(ggalluvial)
library(dplyr)
library(patchwork)
library(paletteer)

data <- gen_data()$offspring_perm

# Function to create alluvial plot for a specific group
create_alluvial_plot <- function(data, title) {
  # Create labels for the quintiles
  quintile_labels <- c(">75%", "50-75%", "25-50%", "0-25%")
  
  alluvial_data <- data %>%
    count(parent_family_income_quintile, own_income_gender_quintile, offspring_family_income_quintile) %>%
    group_by(parent_family_income_quintile) %>%
    mutate(freq = n/sum(n)) %>%
    ungroup()
  
  ggplot(alluvial_data,
         aes(y = freq, 
             axis1 = factor(parent_family_income_quintile, 
                            levels = 4:1,
                            labels = quintile_labels),
             axis2 = factor(own_income_gender_quintile, 
                            levels = 4:1,
                            labels = quintile_labels),
             axis3 = factor(offspring_family_income_quintile, 
                            levels = 4:1,
                            labels = quintile_labels))) +
    geom_alluvium(aes(fill = factor(parent_family_income_quintile,
                                    levels = 4:1,
                                    labels = quintile_labels)), 
                  width = 1/3,
                  alpha = 0.8) +
    geom_stratum(width = 1/3, 
                 fill = "white", 
                 color = "grey") +
    geom_label(stat = "stratum", 
               aes(label = after_stat(stratum)),
               size = 3) +
    scale_x_continuous(breaks = 1:3,
                       labels = c("Parent Income", "Offspring Earnings", "Offspring Family Income")) +
    scale_fill_manual(values = c("orange2", "gold", "steelblue1", "royalblue4")) +
    #scale_fill_manual(values = c("orange2", "white", "white", "white")) +
    labs(y = "Proportion",
         fill = "Parent Income Percentile",
         title = title) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(hjust = 0.5)
    )
}

# Create plots for each group
p1 <- create_alluvial_plot(
  data %>% filter(female == 1, cohort == "1950-1969"),
  "Women, Born 1950-1969"
)

p2 <- create_alluvial_plot(
  data %>% filter(female == 1, cohort == "1970-1985"),
  "Women, Born 1970-1985"
)

p3 <- create_alluvial_plot(
  data %>% filter(female == 0, cohort == "1950-1969"),
  "Men, Born 1950-1969"
)

p4 <- create_alluvial_plot(
  data %>% filter(female == 0, cohort == "1970-1985"),
  "Men, Born 1970-1985"
)

# Combine all plots
alluv <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title = "Quartile Transitions by Gender and Cohort",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )

# Combine all plots
alluv_c1 <- (p3 + p1) +
  plot_annotation(
    title = "Quartile Transitions by Gender",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )

ggsave("alluv/alluv_c1_p5.pdf", alluv_c1, units = "in", height = 7, width = 12)

alluv_c2 <- (p4 + p2) +
  plot_annotation(
    title = "Quartile Transitions by Gender",
    theme = theme(plot.title = element_text(hjust = 0.5))
  )

ggsave("alluv/alluv_c2_p5.pdf", alluv_c2, units = "in", height = 7, width = 12)




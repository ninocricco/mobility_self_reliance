#------------------------------------------------------------------------------
# Animate figures for presentation output
#------------------------------------------------------------------------------

data <- gen_data()

fig1_values <- generate_change_table(data) %>%
  pivot_wider(names_from = model_name, values_from = value) %>%
  transmute(Gender, key, 
            mob_sr_ratio = own_parent * family_own/family_parent)

fig1_p1 <- fig1_values %>%
  mutate(mob_sr_ratio = ifelse(key == "1970-1985", NA, mob_sr_ratio)) %>%# Change this line so that it's not hard coded- use regex to extract
  transmute(cohort = ifelse(key == "1950-1969", "Baby Boomer", "Gen X"), 
            Gender, mob_sr_ratio) %>%
  ggplot(aes(x = cohort, y = mob_sr_ratio, fill = cohort)) +
  geom_col(position = "identity", fill = "gray", color = "black") +
  geom_text(aes(label = round(mob_sr_ratio, 2)), vjust = -0.5) +  # Add labels on top of the bars
  theme_bw() +
  labs(y = "IMER", x = "Birth Cohort", 
       title = "Income Mobility Earnings Ratio by Cohort") +
  theme(plot.title = element_text(hjust = .5),
        legend.position = "") +
  facet_wrap(~Gender) +
  ylim(0,1)

fig1_p2 <- fig1_values %>%
  # Change this line so that it's not hard coded- use regex to extract
  transmute(cohort = ifelse(key == "1950-1969", "Baby Boomer", "Gen X"),
            Gender, mob_sr_ratio) %>%
  ggplot(aes(x = cohort, y = mob_sr_ratio, fill = cohort)) +
  geom_col(position = "identity", fill = "gray", color = "black") +
  geom_text(aes(label = round(mob_sr_ratio, 2)), vjust = -0.5) +  # Add labels on top of the bars
  theme_bw() +
  labs(y = "IMER", x = "Birth Cohort", 
       title = "Income Mobility Earnings Ratio by Cohort") +
  theme(plot.title = element_text(hjust = .5),
        legend.position = "") +
  facet_wrap(~Gender) +
  ylim(0,1)

ggsave("f1_p1.pdf", fig1_p1, width = 6, height = 6, units = "in")
ggsave("f1_p2.pdf", fig1_p2, width = 6, height = 6, units = "in")

pooled <- generate_fig1(gen_data(refdist = "pooled"))
ggsave("pooled.pdf", pooled, width = 6, height = 6, units = "in")
simulated <- generate_fig1(gen_data(outcome = "simulated"))
ggsave("simulated.pdf", simulated, width = 6, height = 6, units = "in")

fig2_values <- estimate_parameters(data) %>%
  filter(term != "(Intercept)") %>%
  filter(model_name %in% c("own_parent", "family_own", 
                           "family_parent", "residual_family_own")) %>%
  mutate(Gender = factor(ifelse(female == 1, "Women", "Men"), 
                         levels = c("Women", "Men")), 
         model_name = factor(case_when(
           model_name == "own_parent" ~ "Earnings Persistence",
           model_name == "family_own" ~ "Earnings-Income Correlation",
           model_name == "family_parent" ~ "Income Persistence",
           model_name == "residual_family_own" ~ "Residual Transmission"),
           levels = c("Income Persistence", "Earnings Persistence", 
                      "Earnings-Income Correlation", "Residual Transmission")))

fig2_p4 <- fig2_values %>%
#  mutate(estimate = ifelse(model_name %in% c("Income Persistence", "Earnings Persistence",
 #                                            "Earnings-Income Correlation"), estimate, NA), 
  #       conf.low = ifelse(model_name %in% c("Income Persistence", "Earnings Persistence",
   #                                          "Earnings-Income Correlation"), conf.low, NA), 
    #     conf.high = ifelse(model_name %in% c("Income Persistence", "Earnings Persistence",
     #                                         "Earnings-Income Correlation"), conf.high, NA)
      #   ) %>%
  ggplot(aes(x = model_name, y = estimate, shape = cohort, color = Gender)) +
  geom_point(size = 2, position = position_dodge(width = .6)) +                  
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),     
                width = 0.2, position = position_dodge(width = .6)) +
  theme_bw() +
  labs(title = "Linear Parameter Estimates by Gender and Cohort",
       y = "Estimate of the Rank-Rank Slope", x = "Model") +
  scale_color_manual(values = c("goldenrod", "navyblue")) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1)

ggsave("f2_p4.pdf", fig2_p, width = 6, height = 6, units = "in")



fig3_values <- generate_change_table(data) %>%
  filter(key != "change") %>%
  pivot_wider(names_from = model_name, values_from = value) %>%
  mutate(through_income = own_parent *family_own, 
         sum_through_income = own_parent + family_own) %>%
  select(Gender, key, own_parent, family_own, 
         through_income, sum_through_income, residual_family_own) %>%
  gather(parameter, value, -c(
    Gender, key, sum_through_income, own_parent, family_own)) %>%
  mutate(
    share_own_parent = case_when(
      parameter == "residual_family_own" ~ 1,
      parameter == "through_income" ~ own_parent/sum_through_income),
    share_family_own = case_when(
      parameter == "residual_family_own" ~ 1,
      parameter == "through_income" ~ family_own/sum_through_income)) %>%
  select(Gender, key, parameter, value, starts_with("share")) %>% 
  pivot_longer(cols = starts_with("share"), 
               names_to = "share_type", 
               values_to = "share_value") %>%
  mutate(share_type = ifelse(
    parameter == "through_income", share_type, parameter), 
    value = ifelse(
      parameter == "through_income", value * share_value, value)) %>%
  unique()

fig3_p1 <- fig3_values %>%
  filter(Gender == "Women") %>%
  mutate(value = ifelse(key == "1950-1969", value, NA)) %>%
  mutate(share_type = factor(case_when(
    share_type == "residual_family_own" ~ "Residual Transmission",
    share_type == "share_family_own" ~ "Earnings-Income Correlation",
    share_type == "share_own_parent" ~ "Earnings Persistence"), 
    levels = c("Residual Transmission",
               "Earnings Persistence", "Earnings-Income Correlation"))) %>%
  ggplot(aes(x = key, y = value, fill = share_type)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Changing Pathways towards Income Persistence",
    y = "Rank-Rank Slope, Family Labor Income", x = "") +
  scale_fill_manual(values = c("gray59", "grey38", "gray20"))+
  facet_wrap(~Gender) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = .5),
        #legend.position = c(.9, .9),
        #legend.justification = c("right", "top"),
        legend.position = "bottom")  +
  ylim(0, .75)

fig3_p2 <- fig3_values %>%
  filter(Gender == "Women") %>%
  mutate(share_type = factor(case_when(
    share_type == "residual_family_own" ~ "Residual Transmission",
    share_type == "share_family_own" ~ "Earnings-Income Correlation",
    share_type == "share_own_parent" ~ "Earnings Persistence"), 
    levels = c("Residual Transmission",
               "Earnings Persistence", "Earnings-Income Correlation"))) %>%
  ggplot(aes(x = key, y = value, fill = share_type)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Changing Pathways towards Income Persistence",
    y = "Rank-Rank Slope, Family Labor Income", x = "") +
  scale_fill_manual(values = c("gray59", "grey38", "gray20"))+
  facet_wrap(~Gender) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = .5),
        #legend.position = c(.9, .9),
        #legend.justification = c("right", "top"),
        legend.position = "bottom")  +
  ylim(0, .75)

fig3_p3 <- fig3_values %>%
  mutate(share_type = factor(case_when(
    share_type == "residual_family_own" ~ "Residual Transmission",
    share_type == "share_family_own" ~ "Earnings-Income Correlation",
    share_type == "share_own_parent" ~ "Earnings Persistence"), 
    levels = c("Residual Transmission",
               "Earnings Persistence", "Earnings-Income Correlation"))) %>%
  ggplot(aes(x = key, y = value, fill = share_type)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Changing Pathways towards Income Persistence",
    y = "Rank-Rank Slope, Family Labor Income", x = "") +
  scale_fill_manual(values = c("gray59", "grey38", "gray20"))+
  facet_wrap(~Gender) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(hjust = .5),
        #legend.position = c(.9, .9),
        #legend.justification = c("right", "top"),
        legend.position = "bottom")  +
  ylim(0, .75)


ggsave("f2_p1.pdf", fig3_p1, width = 8, height = 6, units = "in")
ggsave("f2_p2.pdf", fig3_p2, width = 8, height = 6, units = "in")
ggsave("f2_p3.pdf", fig3_p3, width = 8, height = 6, units = "in")


fig4 <- generate_fig4(gen_data())
fig5 <- generate_fig5(gen_data())
fig7 <- generate_fig7(gen_data())
fig8 <- generate_fig2(gen_data(), by_marstat = T)
ggsave("f8.pdf", fig8, width = 10, height = 6, units = "in")


ggsave("f4.pdf", fig4, width = 6, height = 4, units = "in")
ggsave("f5.pdf", fig5, width = 6, height = 4, units = "in")
ggsave("f7.pdf", fig7, width = 8, height = 6, units = "in")



fig6_values <- generate_decomp_marstat(data) %>%
  gather(key, value, -c(female, model_name, ever_married)) %>% 
  filter(female == gender) %>%
  filter(key %in% c(
    "between_change_pct", "within_change_pct", "prop_change_pct")) %>% 
  mutate(
    param = case_when(model_name == "own_parent" ~ 
                        "Earnings Persistence",
                      model_name == "family_own" ~ 
                        "Earnings-Income Correlation",
                      model_name == "residual_family_own" ~ 
                        "Residual Transmission"),
    key = case_when(key == "between_change_pct" ~ "Between-Group",
                    key == "within_change_pct" ~ "Within-Group",
                    key == "prop_change_pct" ~ "Share")
  )

fig6_share <- fig6_values %>%
  filter(key == "Share") %>%
  group_by(female, model_name, param) %>%
  summarise(value = sum(value)) %>%
  mutate(ever_married = "Share", key = "Share")

fig6_values2 <- fig6_values %>%
  filter(key != "Share") %>%
  bind_rows(fig6_share)

fig6_values2 %>%
  group_by(female, model_name, param) %>%
  summarise(sum(value))

library(ggpattern)

fig6_values2 %>%
  ggplot(aes(y = value, x = param, fill = ever_married, color = key)) + 
  geom_bar(stat = "identity", position = "stack") + 
  scale_fill_manual(values = c("dodgerblue2", "tomato", "goldenrod2"))+
  scale_color_manual(values = c("black", "black", "black"))+
  theme_bw() +
  labs(title = paste0(
    "Decomposing Changes in Parameters by Marital Status, ", 
    ifelse(gender == 1, "Women", "Men")),
    x = "", y = "% Explained") +
  theme(#legend.position = c(.4, .9),
    #legend.justification = c("right", "top"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 20, hjust = 1),
    plot.title = element_text(hjust = .5), 
    legend.title = element_blank())


fig6 <- fig6_values2 %>%
  mutate(group = ifelse(key != "Share", 
                        paste(ever_married, key, sep = ", "),
                        key)) %>%
  ggplot(aes(y = value, x = param, fill = group, color = param)) + 
  geom_bar(stat = "identity", position = "stack") + 
  scale_fill_manual(values = c("red4", "tomato",
                               "dodgerblue4", "dodgerblue1",
                               "goldenrod2"))+
  scale_color_manual(values = c("black", "black", "black"))+
  theme_bw() +
  labs(title = paste0(
    "Decomposing Changes in Parameters by Marital Status, ", 
    ifelse(gender == 1, "Women", "Men")),
    x = "", y = "% Explained") +
  theme(#legend.position = c(.4, .9),
    #legend.justification = c("right", "top"),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 20, hjust = 1),
    plot.title = element_text(hjust = .5), 
    legend.title = element_blank()) +
  guides(color = "none",
         fill = guide_legend(nrow = 2))

ggsave("f6.pdf", fig6, width = 8, height = 6, units = "in")


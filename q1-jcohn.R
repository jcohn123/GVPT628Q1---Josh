library(tidyverse)
library(srvyr)
library(scales)
library(haven)  # to handle labelled data

# Read and clean data
data <- readRDS("Q1data.RDS")

# Convert labelled variables to numeric for recoding
data <- data |>
  mutate(across(starts_with("VCF"), haven::zap_labels))

# Define survey design
svydata <- data |>
  as_survey_design(weights = VCF0009z, nest = TRUE)

# Recode and summarize
recoded <- svydata |>
  mutate(
    partyid.recode = case_when(
      VCF0302 == 1 ~ "Republican",
      VCF0302 == 5 ~ "Democrat",
      TRUE ~ "DK/Other/DNR"
    ),
    age.group = case_when(
      VCF0101 >= 17 & VCF0101 <= 35 ~ "17-35",
      VCF0101 >= 36 & VCF0101 <= 59 ~ "36-59",
      VCF0101 >= 60 ~ "60+",
      TRUE ~ "DNR"
    ),
    Gender = case_when(
      VCF0104 == 1 ~ "Male",
      VCF0104 == 2 ~ "Female",
      TRUE ~ "DNR/Other"
    ),
    hadtochoose = case_when(
      VCF0824 == 1 ~ "Liberal",
      VCF0824 == 5 ~ "Conservative",
      TRUE ~ "Moderate/DNR"
    ),
    IncomePercentile = case_when(
      VCF0114 == 1 ~ "0–16%",
      VCF0114 == 2 ~ "17–33%",
      VCF0114 == 3 ~ "34–67%",
      VCF0114 == 4 ~ "68–95%",
      VCF0114 == 5 ~ "96–100%",
      TRUE ~ "DK/NA/DNR"
    )
  ) |>
  filter(
    partyid.recode != "DK/Other/DNR",
    hadtochoose != "Moderate/DNR"
  ) |>
  group_by(VCF0004, partyid.recode, hadtochoose) |>
  summarize(
    p = survey_mean(vartype = "ci", na.rm = TRUE)
  ) |>
  ungroup()

# Plot
ggplot(recoded, aes(x = VCF0004, y = p, color = hadtochoose)) +
  geom_ribbon(
    aes(ymin = p_low, ymax = p_upp, fill = hadtochoose, group = hadtochoose),
    alpha = 0.15, linewidth = 0
  ) +
  geom_line(linewidth = 0.75) +
  facet_wrap(~partyid.recode, ncol = 1) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    x = NULL,
    y = "Share Within Party",
    title = "Ideology Over Time by Party",
    color = "Ideology",
    fill = "Ideology"
  ) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

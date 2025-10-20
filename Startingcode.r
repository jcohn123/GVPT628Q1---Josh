data <- readRDS("Q1data.rds")



library(tidyverse)
library(srvyr)
library(haven)
library(scales)


cleandata <- data |>
  mutate(paryid.recode = case_when(
    VCF0302 %in% c(1:1) ~ "Republican" ,
    VCF0302 %in% c(5:5) ~ "Democrat" ,
    TRUE ~ "DK/Other/DNR"
         ) 
  ) |> 
  mutate(age.group = case_when(
    VCF0101 %in% c(17:35) ~ "17-35",
    VCF0101 %in% c(36:59) ~ "36:59",
    VCF0101 %in% c(60:99) ~ "60+", 
    TRUE ~ "DNR"
  )) |> 
  mutate (
    Gender = case_when(
      VCF0104 %in% c(1:1) ~ "Male",
      VCF0104 %in% c(2:2) ~ "Female", 
      TRUE ~ "DNR/Other"
    )
  ) |> 
  mutate (
    hadtochoose = case_when(
      VCF0824 %in% c(1:1) ~ "Liberal",
      VCF0824 %in% c(5:5) ~ "Conservative",
      TRUE ~ "Moderate/DNR"
    )
  ) |> 
  mutate(
    IncomePercentile = case_when(
    VCF0114 %in% c(1:1) ~ "0-16%",
    VCF0114 %in% c(2:2) ~ "17-33%",
    VCF0114 %in% c(3:3) ~ "34-67%",
    VCF0114 %in% c(4:4) ~ "68-95%",
    VCF0114 %in% c(5:5) ~ "96-100%",
    TRUE ~ "DK/NA/DNR"
      
    )
  ) 

Q1.results <- cleandata |>
  mutate(
    ideology = case_when 
  )
  )



  
cleandata.svy <- cleandata |>
  as_survey_design(
    weights = VCF0009z,
    nest = TRUE
  )








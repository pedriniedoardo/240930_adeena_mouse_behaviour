# libraries ---------------------------------------------------------------
library(tidyverse)
library(finalfit)

# read in the data --------------------------------------------------------
df <- read_csv("../data/M83HindlimbScoringRawData.csv")


# wrangling ---------------------------------------------------------------
# recode the variables
# There are three treatment groups (1051, 1381, 1461) with n=24 per group (some animals died at some timepoints).
# Each behaviour was recorded at different timepoints (baseline, 30 days post inoculation, 60dpi, 90dpi...). 
table(df$Treatment)
table(df$Stage)

# remove the treatments that are too scarce
df_filter <- df %>%
  # recode the timepoints in the tp column
  mutate(tp = str_extract_all(Stage,pattern = "\\d+d|Baseline") %>% unlist()) %>%
  mutate(tp = str_remove(tp,"d")) %>%
  # group_by(Stage,tp) %>% summarise()
  mutate(tp = case_when(tp == "Baseline" ~ "0",
                        T ~ tp)) %>%
  mutate(tp = str_pad(as.numeric(tp),side = "left",pad = 0,width = 3)) %>%
  mutate(tp_fct = paste0(tp,"d")) %>%
  mutate(tp_num = as.numeric(tp)) %>%
  # recode the treatments
  mutate(Treat = str_remove_all(Treatment," Treatment Group")) %>%
  mutate(Treat = paste0("T",Treat)) %>%
  filter(!tp_fct %in% c("103d", "110d","180d"))

# EDA ---------------------------------------------------------------------
# PCA


# focus on the distance variable






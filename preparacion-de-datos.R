library(ggplot2)
library(tidyr)
library(readr)
library(dplyr)
setwd("/home/fabian/R/proyectos/analisis-bellabeat/datasets")
read.csv("dailyActivity_merged.csv")

#mutar acitividad diairia
skim_without_charts(act_diaria)
glimpse(act_diaria)
CambioAct_diaria <- act_diaria %>%
  mutate(ActivityDate = mdy(ActivityDate))
glimpse(CambioAct_diaria)
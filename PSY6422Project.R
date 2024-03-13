#Requirements
library(tidyverse)
library(here)

#Import Datasets
ftse100pathway <- here("..", "Assessment", "FTSE100_Data_2002_2024.csv")
ftse100 <- read.csv(ftse100pathway)

resultspathway <- here("..", "Assessment", "results.csv")
results <- read.csv(resultspathway)

#Show Data
view(ftse100)
view(results)
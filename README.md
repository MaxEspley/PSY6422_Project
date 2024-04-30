# PSY6422_Project

Edmans et. al., 2007 performed an analysis on the influence of international sporting results on countries’ stock returns, finding that national team losses predicted lower stock returns. The researchers hypoth- esized that this effect was due to decreased investor mood.
https://onlinelibrary.wiley.com/doi/epdf/10.1111/j.1540-6261.2007.01262.x?saml_referrer

I have aimed to perform a similar analysis of each UK country’s national football (soccer) results and the returns of FTSE 100 stocks, beginning in 2002 after the start of the Japan World Cup.

## Replicating my project

The "psy6422_proj" folder of this repository contains three subfolders. The data subfolder contains the three datasets that I analysed. The figs subfolder contains each of the figures that I created from the data. The code subfolder holds an r script consistign of all of the code I used to run my analyses and create my figures. If you download this folder and run the script, the code will find the datasets automatically, run through the analyses, and produce the visualisations in the figs folder.

## Dependencies

The code used loads of functions, and therefore required many libraries!
These included: library(tidyverse), library(here), library(tidyr), library(ggplot2), library(dplyr), library(plotly), library(gganimate), library(gifski), library(forcats), library(ggtext), library(scales), and library(prismatic).

The R script contains the necessary code to install these libraries if needed.

# My Full Analysis
For more details regarding my analyses, please follow this link:
https://rpubs.com/MaxEspley/PSY6422_Project

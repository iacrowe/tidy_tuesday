# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2021-04-13')
tuesdata <- tidytuesdayR::tt_load(2021, week = 16)

post_offices <- tuesdata$post_offices

# load packages
library(tidyverse)



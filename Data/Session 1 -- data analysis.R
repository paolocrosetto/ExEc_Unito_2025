## needed library to manipulate data
library(tidyverse) 
library(broom)

## data import
df <- read_csv("Session_1_BDM_in_action_data.csv")

## let's run some tests!

## test #1: What is the initial WTP for the matched group, for each product? 
##          for the isolated group? are they statistically different?
##
## predictions: 


## test #2: Are there peer effects? 
##          i.e.: does the average WTP move over periods 1-4 in treatment differently than in control? 
##          is there convergence? and if so, towards high, middle, low
##          check both the mean (to get direction) and st.dev. (to get convergence)
##
## predictions: 


## test #3: does giving AB organic info impact WTP? for chocolate? for wine? for both? 
##          is the policy impact different for treatment and control?
##
## predictions: 


## test #4: Are there peer effects AFTER the policy information shock?
##          i.e.: does the average WTP move over periods 1-4 in treatment differently than in control? 
##          is there convergence? and if so, towards high, middle, low
##          check both the mean (to get direction) and st.dev. (to get convergence)
##
## predictions: 




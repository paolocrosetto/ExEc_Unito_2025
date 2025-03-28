## needed library to manipulate data
library(tidyverse) 
library(broom)

## data import
df <- read_csv("Session_1_BDM_in_action_data.csv")

## let's run some tests!

## test #1: What is the initial WTP for the matched group, for each product? 
##          for the isolated group? are they statistically different?
##
##         predictions: 
df %>% 
  filter(period == 1) %>% 
  group_by(product) %>% 
  group_modify(~tidy(t.test(.$bid~.$control)))


## test #2: Are there peer effects? 
##          i.e.: does the average WTP move over periods 1-4 in treatment differently than in control? 
##          is there convergence? and if so, towards high, middle, low
##          check both the mean (to get direction) and st.dev. (to get convergence)
##
## predictions: 
## are there peer effects (st.dev. goes DOWN)? YES
## in which direction does the avg move UP, DOWN, SAME)? 1 up; all the others down

# plot
df %>% 
  filter(period <= 4) %>% 
  #filter(control == 0) %>% 
  ggplot()+
  aes(x = period, y = bid, color = product)+
  stat_summary()+
  facet_grid(control~product, scale = "free")+
  hrbrthemes::theme_ipsum()

# numbers
df %>% 
  filter(period <= 4) %>% 
  group_by(product, control, period) %>% 
  summarise(m = mean(bid), sd = sd(bid))


## test #3: does giving AB organic info impact WTP? for chocolate? for wine? for both? 
##          is the policy impact different for treatment and control?
##
## predictions: 
## is the AB label significantly impacting WTP? yes
## is it different in treatment and control? more for treatment?
df %>% 
  filter(period %in% c(4,5)) %>% 
  group_by(product, control, period) %>% 
  summarise(m = mean(bid))

df %>% 
  filter(period %in% c(4,5)) %>% 
  group_by(product, control) %>% 
  group_modify(~tidy(t.test(.$bid~.$period)))

## test #4: Are there peer effects AFTER the policy information shock?
##          i.e.: does the average WTP move over periods 5-8 in treatment differently than in control? 
##          is there convergence? and if so, towards high, middle, low
##          check both the mean (to get direction) and st.dev. (to get convergence)
##
## predictions: 
## are there peer effects (st.dev. goes DOWN)? YES
## in which direction does the avg move UP, DOWN, SAME)? down but does not offset the jump; 

# plot
df %>% 
  filter(period > 4) %>% 
  filter(control == 0) %>% 
  ggplot()+
  aes(x = period, y = bid, color = product)+
  stat_summary()+   
  facet_wrap(~product, scale = "free")+
  hrbrthemes::theme_ipsum()

# numbers
df %>% 
  filter(period > 4) %>% 
  group_by(product, control, period) %>% 
  summarise(m = mean(bid), sd = sd(bid))  



 
library(tidyverse)
library(janitor)
library(effectsize)
library(gt)

# plot theme
theme_set(hrbrthemes::theme_ipsum_es())

#### 1. reading in the file ####

df <- read_csv("responses.csv")


#### 2. data cleaning ### #

## cleaning names (depends on package "janitor")
df <- df %>% clean_names()

## create new, better names
new_names <- c("ID", "Created", "Link", 
               paste0("D", seq(1,30)), 
               "SOEP", "investment", "lottery", "bret", 
               paste0("HL", seq(1:10)),
               "gender", "nickname")

## apply new names
names(df) <- new_names

## delete unneeded variables
df <- df %>% 
  select(-Created, -Link) %>% 
  select(nickname, everything())


#### 3. variable cleaning ####

## cleaning DOSPERT
df <- df %>% 
  mutate(across(starts_with("D"), ~as.integer(str_sub(.x, end = 2))))


## cleaning SOEP
df <- df %>% 
  mutate(SOEP = str_match(SOEP, "\\((\\d+)\\)")[,2])

## cleaning investment game
df <- df %>% 
  mutate(investment = as.integer(str_remove(investment, "%")))

## cleaning the eckel-grossman lottery selection task
df <- df %>% 
  mutate(lottery = str_sub(lottery, start = 1, end = 1), 
         lottery = as.integer(lottery))

## cleaning the holt and laury task
## recode "1" as a safe option
df <- df %>% 
  mutate(across(starts_with("HL"), ~as.integer(str_sub(.x, end = 2)))) %>% 
  mutate(across(starts_with("HL"), ~-(.x - 1)+1))


#### 4. creating the constructs ####

## DOSPERT
df <- df %>% 
  mutate(spe_ethical = (D6 + D9 + D10 + D16 + D29 + D30)/6,
         spe_financial = (D12 + D4 + D18 + D3 + D14 + D8)/6,
         spe_health = (D5 + D15 + D17 + D20 + D23 + D26)/6,
         spe_recre = (D2 + D11 + D13 + D19 + D24 + D25)/6,
         spe_social = (D1 + D7 + D21 + D22 + D27 + D28)/6,
         spe_all = (spe_ethical + spe_financial + spe_health + spe_recre + spe_social)/6)

## HL

### method 1: counting safe options
df <- df %>% 
  group_by(ID) %>%
  mutate(safeHL = sum(HL1, HL2, HL3, HL4, HL5, HL6, HL7, HL8, HL9, HL10), 
         SOEP = as.numeric(SOEP))


#### 5. just concentrate on the needed variables to have ahandy dataset ####

df <- df %>% 
  select(-starts_with("D"), -starts_with("HL"))


###################################################################################



## SOEP: 
df %>% 
  ggplot(aes(SOEP))+
  geom_histogram()

## DOSPERT
df %>% 
  select(ID, starts_with("spe")) %>% 
  gather(key, value, -ID) %>% 
  ggplot(aes(value, color = key))+
  geom_density(size = 2)+
  facet_grid(key~., scale = "free")+
  hrbrthemes::theme_ipsum_ps()



## investment game
df %>% 
  ggplot(aes(investment))+
  geom_density(size = 2)

## bomb task

df %>% 
  ggplot(aes(bret))+
  geom_density(size = 2)+
  geom_vline(xintercept = 50, color = "red")

## lottery task
df %>% 
  ggplot(aes(lottery))+
  geom_bar(position = position_dodge())+
  geom_vline(xintercept = 4.5, color = "red")

## HL
df %>% 
  ggplot(aes(safeHL))+
  geom_bar(fill = "blue")+
  geom_vline(xintercept = 5, color = "red")


### is there a gender effect?


## data in long format

dflong <- df %>% 
  ungroup() %>% 
  #filter(sample == "2024") %>% 
  filter(!is.na(SOEP)) %>% 
  select(-ID, -nickname, -contains("sampl")) %>% 
  gather(key, value, -gender)

# plot
dflong %>% 
  ggplot(aes(value, color = gender))+
  geom_density()+
  facet_wrap(~key, scale= "free")


## test
dflong %>% 
  group_by(key) %>% 
  group_modify(~broom::tidy(t.test(.$value~.$gender))) %>% 
  select(key, starts_with("estimate"), p.value) %>% 
  gt() %>% 
  fmt_number()


## correlation

panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}


panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt)
}

# do tasks correlate with one another
df %>% 
  ungroup() %>% 
  select(bret, lottery, safeHL, investment) %>% 
  pairs(diag.panel = panel.hist, upper.panel = panel.cor)

## do tasks correlate with questionnaires
df %>% 
  filter(!is.na(SOEP)) %>% 
  ungroup() %>% 
  select(bret, lottery, safeHL, investment, spe_all, SOEP) %>% 
  pairs(diag.panel = panel.hist, upper.panel = panel.cor)


#### 1. Analyzing r #####


## translate into r

## investment
r_invest <- function(choice) {
  
  if (choice == 100) { out <- 1  }
  if (choice == 0) { out <- -1  }
  if (choice != 100 & choice != 0) { 
    out <- (log(1/(2*(100-choice))) + log(100 + 2*choice))/(-log(100-choice) + log(100 + 2*choice))  
  }
  
  out
  
}


## bret
r_bret <- function(choice) {
  
  out <- choice/(100-choice)
  
  out
}


## lottery
r_eg <- function(choice) {
  if (choice == 1) { out <- -2.45 }
  if (choice == 2) { out <- runif(1, min = -2.45, max = -0.16)}
  if (choice == 3) { out <- runif(1, min = -0.16, max = 0.29)}
  if (choice == 4) { out <- runif(1, min = 0.29, max = 0.50)}
  if (choice == 5) { out <- runif(1, min = 0.50, max = 1)}
  if (choice == 6) { out <- 1}
  
  out
}




## hl
r_hl <- function(choice) {
  
  choice = 10 - choice
  
  if (choice == 10) { out <- 2.71   }
  if (choice == 9) { out <- runif(1, min = 1.95, max = 2.71) }
  if (choice == 8) { out <- runif(1, min = 1.49, max = 1.95) }
  if (choice == 7) { out <- runif(1, min = 1.15, max = 1.49) }
  if (choice == 6) { out <- runif(1, min = 0.85, max = 1.15) }
  if (choice == 5) { out <- runif(1, min = 0.59, max = 0.85) }
  if (choice == 4) { out <- runif(1, min = 0.32, max = 0.59) }
  if (choice == 3) { out <- runif(1, min = 0.03, max = 0.32) }
  if (choice == 2) { out <- runif(1, min = -0.37, max = 0.03) }
  if (choice == 1) { out <- -0.37 }
  if (choice == 0) { out <- -0.37 }
  
  out
}

dfr <- df %>% 
  mutate(rbret = r_bret(bret),
         rlottery = r_eg(lottery), 
         rinvestment = r_invest(investment),
         rhl = r_hl(safeHL)) %>% 
  select(gender = nickname, starts_with("r"))

dfr %>% 
  ungroup() %>% 
  select(-ID, -gender) %>% 
  pairs(diag.panel = panel.hist, upper.panel = panel.cor)

# bret
dfr %>% 
  ggplot(aes(rbret))+
  geom_density()+
  geom_vline(xintercept = 1, color = "red")

# investment game
dfr %>% 
  ggplot(aes(rinvestment))+
  geom_density()+
  geom_vline(xintercept = 1, color = "red")


# lottery choice
dfr %>% 
  ggplot(aes(rlottery))+
  geom_density()+
  geom_vline(xintercept = 1, color = "red")

# hl 
dfr %>% 
  ggplot(aes(rhl))+
  geom_density()+
  geom_vline(xintercept = 1, color = "red")

## all together now
dfr %>% 
  select(ID, starts_with("r")) %>% 
  pivot_longer(-ID, names_to = "task", values_to="r", names_prefix = "r") %>% 
  ggplot(aes(r, color = task))+
  geom_density() +
  scale_x_continuous(limits = c(-1,1.5))


# payment time!
df %>% 
  select(nickname, bret) %>% 
  mutate(bret = if_else(bret >= 66, 0, bret)) %>% 
  arrange(-bret) %>% 
  top_n(3)


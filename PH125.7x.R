library(Lahman)
library(broom)
data(Teams)
library(HistData)
library(reshape2)
library(lpSolve)
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>% 
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

lm(son ~ father, data = galton_heights)

galton_heights <- galton_heights %>%
  mutate(father_centered=father - mean(father))

lm(son ~ father_centered, data = galton_heights)

rss <- function(beta0, beta1, data) {
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

fit <- lm(son ~ father_centered, data = galton_heights)
summary(fit)

B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
hist(lse$beta_0)
hist(lse$beta_1)

mod <- lm(son ~ father, data = galton_heights)
summary(mod)

lse %>% summarize(cor(beta_0, beta_1))

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})

cor(lse[1,], lse[2,]) 

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth()

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_point(data = galton_heights, aes(x = father, y = son)) +
  geom_line(color = "blue", size = 1) 
  
#back to balls
library(Lahman)
data(Teams)

lahman_teams <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game = R/G, HR_per_game = HR/G, AB_per_game = AB/G)
lm(R_per_game~AB_per_game, data = lahman_teams, offset = HR_per_game)
lm(R_per_game~BB_per_game+HR_per_game, data = lahman_teams)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G,1),
         BB = BB/G,
         R = R/G) %>%
  select (HR, BB, R) %>%
  filter(HR >= 0.4 & HR <= 1.2)

dat %>% 
  group_by(HR) %>%
  do(fit = lm(R~BB, data = .))

get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

dat %>% 
  group_by(HR) %>%
  do(get_slope(.))

fit <- lm(R~BB, data = dat)
tidy(fit, conf.int=TRUE)

dat %>% 
  group_by(HR) %>%
  do(tidy(lm(R~BB, data = .), conf.int=TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y= estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

glance(fit)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 

fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         BB = BB/G,
         R = R/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G) %>%
  lm(R ~ BB + singles + doubles + triples + HR, data = .)

tidy(fit, conf.int=TRUE)

Teams %>%
  filter(yearID %in% 2002) %>%
  mutate(HR = HR/G,
         BB = BB/G,
         R = R/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G) %>%
  mutate(R_hat = predict(fit, newdata = .)) 

pa_per_game <- Batting %>% filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
  .$pa_per_game %>%
  mean

players <- Batting %>% filter(yearID %in% 1999:2001) %>%
  group_by(playerID) %>% mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G,
            triples = sum(X3B)/G,
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>% select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

players <- Salaries %>% filter(yearID == 2002) %>%
  select(playerID, salary) %>% right_join(players, by="playerID")

players <- Fielding %>% filter(yearID == 2002) %>%
  filter(!POS %in% c("OF","P")) %>%
  group_by(playerID) %>% top_n(1,G) %>%
  filter(row_number(G) == 1) %>% ungroup() %>%
  select(playerID,POS) %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS) & !is.na(salary))
  
players <- Master %>% 
  select(playerID, nameFirst, nameLast, debut) %>%
  right_join(players, by="playerID") 

teamA <- -2.77+2*0.371+4*0.519+1*0.771+1*0.927
teamB <- -2.77+1*0.371+6*0.519+2*0.771+1*1.24

players <- players %>% filter(debut <= 1997 & debut > 1988)
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 
  

our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)


my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))

#sophomore slump
playerInfo <- Fielding %>% group_by(playerID) %>%
  arrange(desc(G)) %>% slice(1) %>% 
  ungroup %>% left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

ROY <- AwardsPlayers %>% filter(awardID == "Rookie of the Year") %>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG = H/AB) %>%
  filter(POS != "P")

ROY <- ROY %>% filter(yearID == rookie_year | yearID == rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>% ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))
ROY

data("admissions")
?admissions

#Assessment: Linear Models (Verified Learners only)
library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G)


Teams_small %>%
  lm(avg_attendance ~ R, .)
Teams_small %>%
  lm(avg_attendance ~ HR, .)
#not 18.9 not 45


#Assessment: Baseball as a Motivating Example
library(Lahman)
library(tidyverse)
library(dslabs)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(W_per_game = W/G, E_per_game = E/G) %>%
  ggplot(aes(W_per_game, E_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(W_per_game = W/G, E_per_game = E/G) %>%
  lm(W_per_game~E_per_game,.)

Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(X3B_per_game = X3B/G, X2B_per_game = X2B/G) %>%
  ggplot(aes(X3B_per_game, X2B_per_game)) + 
  geom_point(alpha = 0.5)

#Assessment: Correlation
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  summarize(r = cor(AB_per_game, R_per_game)) %>% 
  pull(r)

#correct
Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
cor(Teams_small$R/Teams_small$G, Teams_small$AB/Teams_small$G)

cor(Teams_small$W/Teams_small$G, Teams_small$E/Teams_small$G)

cor(Teams_small$X2B/Teams_small$G, Teams_small$X3B/Teams_small$G)

#Assessment: Stratification and Variance Explained
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

female_heights %>%
  summarise(avg_m = mean(mother), avg_d = mean(daughter),sd_m = sd(mother), sd_d = sd(daughter), c = cor(mother,daughter))

female_heights %>%
  lm(daughter~mother,.)
1 * 0.325 * (2.39/2.29)

100*0.325^2

60 * 0.339 + 42.517 

#Assessment: Least Squares Estimates
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits
female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight) 

model <- female_heights %>%
  lm(mother~daughter,.)
model

model$coefficients[1]+model$coefficients[2]*female_heights[1,]
female_heights[1,]

library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

bat_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarise(mean_singles = mean(singles) ,mean_bb = mean(bb) ) %>%
  select(playerID, mean_singles, mean_bb)

sum(bat_01$mean_singles>0.2)
sum(bat_01$mean_bb>0.2)

bat_03 <- inner_join(bat_01,bat_02)
cor(bat_03$singles, bat_03$mean_singles)
cor(bat_03$bb, bat_03$mean_bb)

bat_03 %>%
  ggplot(aes(mean_singles,singles)) +
  geom_point()
bat_03 %>%
  ggplot(aes(mean_bb,bb)) +
  geom_point()

bat_03 %>%
  lm(singles~mean_singles,.)
bat_03 %>%
  lm(bb~mean_bb,.)

#Assessment: Tibbles, do, and broom
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton %>%
  group_by(pair) %>%
  summarize(obs = n())

galton %>%
  group_by(pair) %>%
  summarize(c = cor(childHeight,parentHeight))

library(broom)
galton %>%  
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight") 

#correct
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "father_daughter") %>%
  pull(estimate)
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "mother_son") %>%
  pull(estimate)



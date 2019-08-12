#1.4 Assessment: Discrete Probability  
library(gtools)
library(tidyverse)
options(digits = 3)    # report 3 significant digits

medals <- permutations(8,3)
nrow(medals)

jamaica <- permutations(3,3)
nrow(jamaica)

nrow(jamaica)/nrow(medals)

runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
n <- 10000
set.seed(1, sample.kind = "Rounding")
result <- replicate(n, {
  sample <- sample(runners, 3)
  all(sample == "Jamaica") 
})

mean(result)

#Question 2: Restaurant management
6*nrow(combinations(6,2))*2

6*nrow(combinations(6,2))*3

6*nrow(combinations(6,3))*3

meal_combinations <- function(entree_choices) {
  entree_choices*nrow(combinations(6,2))*3
}
lapply(1:12,meal_combinations)

#answer
entree_choices <- function(x){
  x * nrow(combinations(6,2)) * 3
}

combos <- sapply(1:12, entree_choices)

data.frame(entrees = 1:12, combos = combos) %>%
  filter(combos > 365) %>%
  min(.$entrees)

side_choices <- function(x){
  6 * nrow(combinations(x,2)) * 3
}
combos <- sapply(2:12, side_choices)
data.frame(sides = 2:12, combos = combos) %>%
  filter(combos > 365) %>%
  min(.$sides)

#Questions 3 and 4: Esophageal cancer and alcohol/tobacco use
library(tidyverse)
head(esoph)

dim(esoph)

all_cases <- sum(esoph$ncases)

all_controls <- sum(esoph$ncontrols)

table(esoph$alcgp)
ind <- esoph$alcgp == "120+"
sum(esoph$ncases[ind])/(sum(esoph$ncontrols[ind])+sum(esoph$ncases[ind]))

ind <- esoph$alcgp == "0-39g/day"
sum(esoph$ncases[ind])/(sum(esoph$ncontrols[ind])+sum(esoph$ncases[ind]))

table(esoph$tobgp)
ind <- esoph$tobgp %in% c("10-19","20-29","30+")
sum(esoph$ncases[ind])/all_cases

sum(esoph$ncontrols[ind])/all_controls

ind <- esoph$alcgp == "120+"
sum(esoph$ncases[ind])/all_cases

ind <- esoph$tobgp == "30+"
sum(esoph$ncases[ind])/all_cases

ind <- esoph$tobgp == "30+" & esoph$alcgp == "120+"
sum(esoph$ncases[ind])/all_cases

ind <- esoph$tobgp == "30+" | esoph$alcgp == "120+"
sum(esoph$ncases[ind])/all_cases

ind <- esoph$alcgp == "120+"
sum(esoph$ncontrols[ind])/all_controls

(sum(esoph$ncases[ind])/all_cases)/(sum(esoph$ncontrols[ind])/all_controls)

ind <- esoph$tobgp == "30+"
sum(esoph$ncontrols[ind])/all_controls

ind <- esoph$tobgp == "30+" & esoph$alcgp == "120+"
sum(esoph$ncontrols[ind])/all_controls

ind <- esoph$tobgp == "30+" | esoph$alcgp == "120+"
sum(esoph$ncontrols[ind])/all_controls

(sum(esoph$ncases[ind])/all_cases)/(sum(esoph$ncontrols[ind])/all_controls)

#2.2 Assessment: Continuous Probability  
set.seed(16, sample.kind = "Rounding")
act_scores <- rnorm(10000, 20.9,5.7)

mean(act_scores)
  
sd(act_scores)

sum(act_scores>=36)  

mean(act_scores>30)

mean(act_scores<=10)

x <- 1:36
f_x <- dnorm(x, 20.9, 5.7)
plot(x,f_x)

zact_scores <- (act_scores - mean(act_scores)) / sd(act_scores)

mean(zact_scores>2)

2 * sd(act_scores) + mean(act_scores)

qnorm(0.975, mean(act_scores),sd(act_scores))

cdf <- function(x){
  mean(act_scores <= x)
}

cdf_range <- sapply(1:36, cdf)
names(cdf_range) <- 1:36
cdf_range

qnorm(0.95, 20.9,5.7)

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores, probs =  p)
sample_quantiles

theoretical_quantiles <- qnorm(p, 20.9,5.7)

qqplot(theoretical_quantiles,sample_quantiles)

#3.3 Assessment: Random Variables, Sampling Models, and the Central Limit Theorem  

p <- 1/5

exp <- 1*p+-0.25*(1-p)

exp_score <- exp * 44

sd_error <- sqrt(p*(1-p))*abs(1--0.25)*sqrt(44)

1-pnorm(8,exp_score,sd_error)

set.seed(21, sample.kind = "Rounding")
n <- 10000
results <- replicate(n, {
  result <- sample(c(-0.25,1),44,replace = TRUE, prob = c(1-p,p))
  sum(result)
})

sum(results>=8)/n

p <- 1/4
mu <- 1*p+0*(1-p)
e_x <- mu * 44

se_x <- sqrt(p*(1-p))*abs(1--0)*sqrt(44)
1-pnorm(30,e_x,se_x)

p <- seq(0.25, 0.95, 0.05)
mu <- 1*p+0*(1-p)
e_x <- mu * 44
se_x <- sqrt(p*(1-p))*abs(1--0)*sqrt(44)
1-pnorm(35,e_x,se_x)

p <- 5/38
mu <- 6*p+-1*(1-p)

se_x <- sqrt(p*(1-p))*abs(6--1)

e_x <- mu * 500 / 500

se_x <- sqrt(p*(1-p))*abs(6--1) *sqrt(500) / 500

e_x <- mu * 500 

se_x <- sqrt(p*(1-p))*abs(6--1) *sqrt(500) 

pnorm(0,e_x,se_x)

#4.2 Assessment: The Big Short  
options(digits = 3)
library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)

p <- death_prob$prob[death_prob$sex == "Female" & death_prob$age == 50]
#correct
p <- death_prob %>%
  filter(sex == "Female" & age == "50") %>%
  pull(prob)
p

a <- -150000
b <- 1150
mu <- a*p + b*(1-p)
mu

sigma <- abs(b-a) * sqrt(p*(1-p))
sigma

n <- 1000
mu_1000 <- mu * n
mu_1000

sigma_1000 <- sigma * sqrt(n)
sigma_1000

pnorm(0,mu_1000,sigma_1000)

p <- death_prob %>%
  filter(sex == "Male" & age == "50") %>%
  pull(prob)
p

mu <- 700000
n <- 1000
a <- -150000
b <- (mu / n - a * p) / ((1-p))
b  

sigma <- abs(b-a) * sqrt(p*(1-p))* sqrt(n)
sigma
  
pnorm(0,mu,sigma)

p <- death_prob %>%
  filter(age == "50") %>%
  pull(prob)
p
#pandemic effect
p <- 0.015
a <- -150000
b <- 1150
n <- 1000
mu <- a*p + b*(1-p)
mu_1000 <- mu * n
mu_1000

sigma_1000 <- abs(b-a) * sqrt(p*(1-p)) * sqrt(n)
sigma_1000

pnorm(0,mu_1000,sigma_1000)

pnorm(-1000000,mu_1000,sigma_1000)

p <- seq(.01, .03, .0025)
a <- -150000
b <- 1150
n <- 1000
mu <- a*p + b*(1-p)
mu_1000 <- mu * n
sigma_1000 <- abs(b-a) * sqrt(p*(1-p)) * sqrt(n)
pnorm(0,mu_1000,sigma_1000)
#proper
p <- seq(.01, .03, .001)
a <- -150000    # loss per claim
b <- 1150    # premium - profit when no claim
n <- 1000
p_lose_money <- sapply(p, function(p){
  exp_val <- n*(a*p + b*(1-p))
  se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(0, exp_val, se)
})
data.frame(p, p_lose_money) %>%
  filter(p_lose_money > 0.9) %>%
  pull(p) %>%
  min()

pnorm(-1000000,mu_1000,sigma_1000)
#proper
p_lose_million <- sapply(p, function(p){
  exp_val <- n*(a*p + b*(1-p))
  se <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
  pnorm(-1*10^6, exp_val, se)
})
data.frame(p, p_lose_million) %>%
  filter(p_lose_million > 0.9) %>%
  pull(p) %>%
  min()

p_loss <- 0.015
n <- 1000

set.seed(25, sample.kind = "Rounding")
sum(sample(c(a,b), n, replace = TRUE, prob = c(p_loss, 1-p_loss)))/10^6


rep <- 10000
set.seed(27, sample.kind = "Rounding")
results <- replicate(rep, {
  sum(sample(c(a,b), n, replace = TRUE, prob = c(p_loss, 1-p_loss)))
})
mean(results < -1000000)
#proper
B <- 10000
profits <- replicate(B, {
  outcomes <- sample(c(loss, profit), n, prob = c(p, 1-p), replace = TRUE)
  sum(outcomes)/10^6
})
mean(profits < -1)

#Calculate the premium required for a 5% chance of losing money given  𝑛=1000  loans, probability of death  𝑝=0.015 , and loss per claim  𝑙=−150000 . Save this premium as x for use in further questions.
p <- 0.015
mu <- 0
n <- 1000
a <- -150000
b <- (mu / n - a * p) / (1-p)
b  
sigma <- abs(b-a) * sqrt(p*(1-p))* sqrt(n)
sigma
pnorm(0,mu,sigma)

b <- seq(3260, 3280, 1)
b_range <- sapply(b, function(x){
  exp_val <- n*(a*p + x*(1-p))
  se <- sqrt(n) * abs(x-a) * sqrt(p*(1-p))
  pnorm(0, exp_val, se)
})
x <- data.frame(b, b_range) %>%
  filter(b_range < 0.05) %>%
  pull(b) %>%
  min()
x
#proper
p <- .015
n <- 1000
l <- -150000
z <- qnorm(.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x

exp_val <- (a*p + x*(1-p))
exp_val
#proper
l*p + x*(1-p)

(l*p + x*(1-p))*1000
#proper
mu <- n*(l*p + x*(1-p))
mu

se <- abs(x-l) * sqrt(p*(1-p))* sqrt(n)
B <- 10000
set.seed(28, sample.kind = "Rounding")
p_losing_money <- replicate(B, {
  sum(sample(c(l,x),n, prob = c(p,1-p), replace = TRUE))
})
mean(p_losing_money<0)

B <- 10000
set.seed(29, sample.kind = "Rounding")
p_losing_money <- replicate(B, {
  p <- p + sample(seq(-0.01, 0.01, length = 100), 1)
  sum(sample(c(l,x),n, prob = c(p,1-p), replace = TRUE))
})
mean(p_losing_money)

mean(p_losing_money<0)

mean(p_losing_money < -1*10^6)


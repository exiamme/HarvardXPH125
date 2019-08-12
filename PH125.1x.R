#Section 1 Assessment
quadratic <- function(a,b,c) {
 (- b + c(1,-1) * sqrt(b^2-4*a*c)) / (2*a)
}
quadratic(2,-1,-4)

log(1024,base = 4)

library(dslabs)
data(movielens)
dim(movielens)
class(movielens$title)
class(movielens$genre)
nlevels(movielens$genre)

#Section 2 Assessment
x <- c(2, 43, 27, 96, 18)
#none
x[order(x)]
rank(x)
sort(x)
min(x)
which.min(x)
max(x)
which.max(x)

name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
time <- time / 60
speed <- distance / time
speed

#Section 3 Assessment
library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers
avg <- mean(heights$height)
ind <- heights[heights$height>avg,]
dim(ind)
table(ind$sex)
mean(heights$sex=="Female")
min(heights$height)
match(min(heights$height),heights$height)
heights$sex[match(min(heights$height),heights$height)]
max(heights$height)
x <- ceiling(min(heights$height)):floor(max(heights$height)) #x <- 50:82
sum(!x %in% heights$height)
heights2 <- heights %>% 
  mutate(ht_cm = height * 2.54)
heights2[18,]
mean(heights2$ht_cm)
females <- heights2[heights2$sex == "Female",]
dim(females)
mean(females$ht_cm)

library(dslabs)
data(olive)
head(olive)

olive %>% 
  ggplot(aes(palmitic,palmitoleic)) +
  geom_point()

olive %>% 
  ggplot(aes(eicosenoic)) +
  geom_histogram()

boxplot(palmitic ~ region, olive )

#Section 4 Assessment
library(dslabs)
data(heights)

sum(ifelse(heights$sex=="Female",1,2))
mean(ifelse(heights$height>72,heights$height,0))
inches_to_ft <- function(x) {
  x / 12
}
inches_to_ft(144)
count(inches_to_ft(heights$height)<5)

# define a vector of length m
m <- 10
f_n <- vector(length = m)

# make a vector of factorials
for(n in 1:m){
  f_n[n] <- factorial(n)
}

# inspect f_n
f_n

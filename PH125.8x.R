library(caret)
library(dslabs)
library(e1071)
library(dplyr)
library(lubridate)
library(MASS)
library(HistData)
data("reported_heights")
data("heights")
y <- heights$sex
x <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights[-test_index,]
test_set <- heights[test_index,]
y_hat <- sample(c("Male","Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
cutoff <- seq(61,70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})
best_cutoff <- cutoff[which.max(accuracy)]
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)

#comprehension
mnist <- read_mnist()
y <- mnist$train$labels
y[5] > y[6]

#continued
table(predicted = y_hat, actual = test_set$sex)
test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summary(accuracy = mean(y_hat == sex))
prev <- mean(y == "Male")
#sensitivity = true positive rate or recall
#specificity = true negative rate or 1- false positive rate
#specificity = positive predictive value or precision
confusionMatrix(data = y_hat, reference = test_set$sex)

#build for F score
cutoff <- seq(61,70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})
best_cutoff <- cutoff[which.max(F_1)]
y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)
confusionMatrix(data = y_hat, reference = test_set$sex)

#confusion matrix
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type
mean(y[x == 'online'] == 'Female' )
table(dat$sex)
table(dat$type)
summarise(group_by(dat,sex,type), count = n())

y_hat <- ifelse(x == "online", "Male", "Female") %>% factor(c("Female", "Male"))
mean(y_hat == y)

table(y_hat, y)
sensitivity(data = y_hat, reference = y)
specificity(data = y_hat, reference = y)
confusionMatrix(data = y_hat, reference = y)

#comprehension
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

names(train)
table(train$Species)
feature <- train$Petal.Length
fivenum(feature)
cutoff <- seq(fivenum(feature)[2],fivenum(feature)[4], 0.1)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(feature < x, "versicolor", "virginica") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
best_cutoff <- cutoff[which.max(accuracy)]
y_hat <- ifelse(test$Petal.Length < best_cutoff, "versicolor", "virginica") %>%
  factor(levels = levels(test$Species))

mean(y_hat == test$Species)

#train another feature on test data
names(test)
table(test$Species)
feature_02 <- test$Petal.Width
fivenum(feature_02)
cutoff <- seq(fivenum(feature_02)[2],fivenum(feature_02)[4], 0.1)
map_dbl(cutoff, function(x){
  y_hat <- ifelse(feature_02 < x, "versicolor", "virginica") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == test$Species)
})

plot(iris,pch=21,bg=iris$Species)

#combination
feature_02 <- train$Petal.Width
fivenum(feature_02)
cutoff_02 <- seq(fivenum(feature_02)[2],fivenum(feature_02)[4], 0.1)
accuracy_02 <- map_dbl(cutoff_02, function(x){
  y_hat <- ifelse(feature_02 < x, "versicolor", "virginica") %>%
    factor(levels = levels(test$Species))
  mean(y_hat == train$Species)
})
best_cutoff_02 <- cutoff_02[which.max(accuracy_02)]

y_hat <- ifelse(test$Petal.Length < best_cutoff , "versicolor", "virginica") %>%
  factor(levels = levels(test$Species))
y_hat_02 <- ifelse(test$Petal.Width < best_cutoff_02  , "versicolor", "virginica") %>%
  factor(levels = levels(test$Species))
y_hat_comb <- ifelse(test$Petal.Length > best_cutoff|test$Petal.Width > best_cutoff_02 , "virginica", "versicolor") %>%
  factor(levels = levels(test$Species))

mean(y_hat == test$Species )
mean(y_hat_02 == test$Species )
mean(y_hat_comb == test$Species )


mean(y_hat == test$Species | y_hat_02 == test$Species )
#answer
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

plot(iris,pch=21,bg=iris$Species)

set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)

#conditional probabilities
set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#What is the probability that a test is positive?
mean(test)

#What is the probability that an individual has the disease if the test is negative?
(1-mean(test[disease==1]))*mean(disease)/
  ((1-mean(test[disease==1]))*mean(disease) + 
     (1-mean(disease)) * (1-mean(test[disease==0])))

#What is the probability that you have the disease if the test is positive?
(mean(test[disease==1]))*mean(disease)/
  ((mean(test[disease==1]))*mean(disease) + 
     (1-mean(disease)) * (mean(test[disease==0])))

#If the test is positive, what is the relative risk of having the disease?
((mean(test[disease==1]))*mean(disease)/
    ((mean(test[disease==1]))*mean(disease) + 
       (1-mean(disease)) * (mean(test[disease==0]))))/mean(disease)

#height
library(dslabs)
data("heights")

heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

  ps <- seq(0, 1, 0.1)
  dat %>% 
    mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
    group_by(g) %>%
    summarize(y = mean(y), x = mean(x)) %>%
    qplot(x, y, data =.)

#regressions
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight) 

set.seed(1, sample.kind = "Rounding")
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

mean(train_set$son)
mean((mean(train_set$son) - test_set$son)^2)

fit <- lm(son~father, data = train_set)
y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
mean((y_hat - test_set$son)^2)
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)

#test
set.seed(1, sample.kind = "Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

set.seed(1, sample.kind = "Rounding")
RMSE <- replicate(n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y~x, data = train_set)
  y_hat <- predict.lm(fit, test_set)
  #y_hat <- fit$coef[1] + fit$coef[2]*test_set$x
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(RMSE)
sd(RMSE)
#not 0.12

RMSEs <- function(n) {
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  RMSE <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y~x, data = train_set)
    y_hat <- predict.lm(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
  c(mean(RMSE),sd(RMSE))
}

n <- c(100,500,1000,5000,10000)
set.seed(1, sample.kind = "Rounding")
sapply(n, RMSEs)


set.seed(1, sample.kind = "Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


set.seed(1, sample.kind = "Rounding")
RMSE <- replicate(n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y~x, data = train_set)
  y_hat <- predict.lm(fit, test_set)
  #y_hat <- fit$coef[1] + fit$coef[2]*test_set$x
  sqrt(mean((y_hat - test_set$y)^2))
})

mean(RMSE)
sd(RMSE)

set.seed(1, sample.kind = "Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)
fit <- lm(y~x_1+x_2, data = train_set)
y_hat <- predict.lm(fit, test_set)
sqrt(mean((y_hat - test_set$y)^2))

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1, sample.kind = "Rounding")
RMSE <- replicate(n, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y~x_1+x_2, data = train_set)
  y_hat <- predict.lm(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})

c(mean(RMSE),sd(RMSE))

#Logistic Regression
data("heights")
y <- heights$height
set.seed(2, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>%
  lm(y~height, data = .)

p_hat <- predict(lm_fit,test_set)
y_hat <- ifelse(p_hat > 0.5,"Female","Male") %>% factor()
confusionMatrix(y_hat, test_set$sex)

glm_fit <- train_set %>%
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y~height, data=., family="binomial")

p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")

y_hat_logit <- ifelse(p_hat_logit > 0.5,"Female","Male") %>% factor()
confusionMatrix(y_hat_logit, test_set$sex)

#Test
set.seed(2, sample.kind = "Rounding")
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

dat$train %>% 
  ggplot(aes(x, color = y)) + 
  geom_density()

set.seed(1, sample.kind = "Rounding")
accuracy <- 0
mu_1 <- seq(0, 3, len=25)
for (i in 1:25){
  dataset<-make_data(mu_1=mu_1[i])
  
  glm_fit <- dataset$train %>%
    glm(y~x, data=., family="binomial")
  
  p_hat_logit <- predict(glm_fit, newdata = dataset$test, type = "response")
  
  y_hat_logit <- ifelse(p_hat_logit > 0.5,1,0) %>% factor()
  accuracy[i] <- mean(y_hat_logit == dataset$test$y)
}

plot(mu_1,accuracy)

#smoothing
total_days <- diff(range(polls_2008$day))
span <- 21/total_days

fit <- loess(margin ~ day, degree = 1, span = span, data=polls_2008)

polls_2008 %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(day,margin)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(day,smooth), color = "red")
  
polls_2008 %>%
  ggplot(aes(day,margin)) +
  geom_point() +
  geom_smooth(color = "red", span = .15, method.args = list(degree = 1))
  
  
#questions
library(tidyverse)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01" & !is.na(deaths))



length(unique(dat$date))
span <- (365/12)/1205
fit <- loess(deaths ~ as.numeric(date), degree = 1, span = span, data=dat)
length(fit$fitted)

dat %>% mutate(smooth = fit$fitted) %>%
  ggplot(aes(date,deaths)) +
  geom_point(size = 3, alpha = .5, color = "grey") +
  geom_line(aes(date,smooth), color = "red")

#solution
span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)
dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = 2)

dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)


span <- diff(range(mnist_27$train$x_2)) /10
fit <- mnist_27$train %>% mutate(x_2 = as.numeric(x_2),y = as.numeric(y)) %>%
  loess(y ~ x_2, data = ., span = span, degree = 1)

mnist_27$train %>% mutate(smooth = predict(fit, as.numeric(x_2))) %>%
  ggplot() +
  geom_point(aes(x_2, y)) +
  geom_line(aes(x_2, smooth), lwd = 2, col = 2)

#matrix
library(matrixStats)
mnist <- read_mnist()

x <- mnist$train$images[1:1000,]
y <- mnist$train$labels[1:1000]

grid <- matrix(x[3,],28,28)
image(1:28,1:28,grid[,28:1])

plot(y,rowMeans(x))
plot(y,apply(x,1,mean))

sds <- colSds(x)
hist(sds)

#drop keeps matrix, coldSds remove pixel with low standard deviation (which do not bring much to the model)
new_x <- x[,colSds(x)>60, drop=FALSE]
dim(new_x)  
hist (colSds(new_x))

qplot(as.vector(x), bins = 30, color = I("black"))
new_x <- x
new_x[new_x < 50] <- 0
qplot(as.vector(new_x), bins = 30, color = I("black"))

bin_x <- x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
bin_x <- (x>255/2)*1
grid <- matrix(bin_x[3,],28,28)
image(1:28,1:28,grid[,28:1])

X_mean_0 <- sweep(x, 2, colMeans(x))
X_standardize <- sweep(X_mean_0, 2, colSds(x), FUN = "/")

x <- matrix(rnorm(100*10), 100, 10)
dim(x)
nrow(x)
ncol(x)

x <- x + seq(nrow(x))
x <- 1:nrow(x)
x <- sweep(x, 1, 1:nrow(x),"+")

x <- sweep(x, 2, 1:ncol(x), FUN = "+")

rowMeans(x)
colMeans(x)

bin_x <- x
bin_x <- (x>=50&x<=205)*1
mean(bin_x)

set.seed(0, sample.kind = "Rounding")
if(!exists("mnist")) mnist <- read_mnist()
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

y[1:3]
x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]
sqrt(sum((x_1 - x_2)^2))
sqrt(crossprod(x_1-x_2))
d<- dist(x)
as.matrix(d)[1:3,1:3]
image(as.matrix(d))
image(as.matrix(d)[order(y),order(y)])

d <- dist(t(x))
dim(as.matrix(d))

library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

d <- dist(tissue_gene_expression$x)
as.matrix(d)[c(1:2,39:40,73:74),c(1:2,39:40,73:74)]
image(as.matrix(d))

#knn
library(caret)
library(purrr)
#glm prediction
fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_logistic <- predict(fit_glm,mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

#y for all predictors
knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)

#y using x matrix of 2nd and 3rd predictors
x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x,y,k=5)
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]


ks <- seq(3,500,2)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  y_hat <- predict(fit,mnist_27$train, type = "class")
  train_error <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)$overall["Accuracy"]
  y_hat <- predict(fit,mnist_27$test, type = "class")
  test_error <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]    
  list(train = train_error, test = test_error)
})
ggplot() +
  geom_line(aes(ks, accuracy$train)) +
  geom_line(aes(ks, accuracy$test))
plot(ks,accuracy$train,accuracy$test)

#Comprehension Check: Nearest Neighbors
#Q1
library(caret)
library(dslabs)
data("heights")
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
train <- heights %>% slice(-test_index)
test <- heights %>% slice(test_index)

ks <- seq(1,101,3) 
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ ., data = train, k = k)
  y_hat <- predict(fit,test, type = "class") %>%   factor(levels = levels(test$sex))
#  test_error <- confusionMatrix(data = y_hat, reference = test$sex)$overall["Accuracy"]    
  F_meas(data = y_hat, reference = factor(test$sex))
})

max(F_1)
ks[which.max(F_1)]

fit <- knn3(sex ~ ., data = train, k = 43) #this k is correct as well
y_hat <- predict(fit,test, type = "class") %>%   factor(levels = levels(test$sex))
F_meas(data = y_hat, reference = factor(test$sex)) #this is correct as well

#full correct answer
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]     

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]

#Q2
data("tissue_gene_expression")
set.seed(1, sample.kind = "Rounding")
train_index <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)
train <- as.data.frame(tissue_gene_expression) %>% slice(train_index)
test <- as.data.frame(tissue_gene_expression) %>% slice(-train_index)

ks <- seq(1,12,2) 
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = train, k = k)
  y_hat <- predict(fit,test, type = "class") #%>%   factor(levels = levels(test$y))
  test_error <- confusionMatrix(data = y_hat, reference = test$y)$overall["Accuracy"]
  list(k = k, test_error = test_error)
})
accuracy

#copy
write_clip(as.character(round(accuracy$test_error[6],7)))

#full correct code (old version, but answers wrong for some reason)
set.seed(1, sample.kind = "Rounding")
library(caret)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
train_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[train_index,], y[train_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[-train_index,]),
                   type = "class")
  mean(y_hat == y[-train_index])
})

#Comprehension Check: Cross-validation
library(dplyr)
library(caret)
library(dslabs)
set.seed(1996, sample.kind = "Rounding")
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method = "glm")
fit$results

install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y) #genefilter
pvals <- tt$p.value
ind <- which(pvals<0.01)

x_subset <- x[ ,ind]
fit <- train(x_subset, y, method = "glm")
fit$results

fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

k = seq(1,7,2)
fit <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "knn", tuneGrid = data.frame(k = k))
#fit <- knn3(y ~ ., data = tissue_gene_expression, k = k)

#bootstrap
B <- 10^5
M_stars <- replicate(B {
  X_star <- sample(X,N, replace = TRUE)
  M_start <- median(X_star)
})

#Comprehension Check: Bootstrap
set.seed(1995, sample.kind = "Rounding")
indexes <- createResample(mnist_27$train$y, 10)
n <- 3
sum(indexes$Resample01 == n) +
  sum(indexes$Resample02 == n) +
  sum(indexes$Resample03 == n) +
  sum(indexes$Resample04 == n) +
  sum(indexes$Resample05 == n) +
  sum(indexes$Resample06 == n) +
  sum(indexes$Resample07 == n) +
  sum(indexes$Resample08 == n) +
  sum(indexes$Resample09 == n) +
  sum(indexes$Resample10 == n) 

y <- rnorm(100, 0, 1)
quantile(y, 0.75)
B <- 10^5
set.seed(1, sample.kind = "Rounding")
quantile <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})

mean(quantile)
sd(quantile)

set.seed(1, sample.kind = "Rounding")
y <- rnorm(100, 0, 1)

set.seed(1, sample.kind = "Rounding")
indexes <- createResample(y, 10)
sd(c(quantile(y[indexes$Resample01],0.75),
quantile(y[indexes$Resample02],0.75),
quantile(y[indexes$Resample03],0.75),
quantile(y[indexes$Resample04],0.75),
quantile(y[indexes$Resample05],0.75),
quantile(y[indexes$Resample06],0.75),
quantile(y[indexes$Resample07],0.75),
quantile(y[indexes$Resample08],0.75),
quantile(y[indexes$Resample09],0.75),
quantile(y[indexes$Resample10],0.75)))

set.seed(1, sample.kind = "Rounding")
indexes <- createResample(y, 10000)
v <- 1:10000
j <- 1
for (i in indexes) {
  v[j] <- quantile(y[i],0.75)
  j <- j + 1
}
mean(v)
sd(v)

#qda and lda
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize (avg1 = mean(x_1), avg2 = mean(x_2), sd1 = sd(x_1), sd2 = sd(x_2), r = cor(x_1,x_2))

train_qda <- train (y ~ . , method = "qda", data = mnist_27$train)
y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

#assume sd and mean at the same, lda
params <- params %>% mutate(sd1 = mean(sd1), sd2=mean(sd2), r=mean(r))

train_lda <- train (y ~ . , method = "lda", data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)$overall["Accuracy"]

#Comprehension Check: Generative Models
library(dslabs)
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind = "Rounding") # use this line of code if you are using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

train_lda <- train (x,y, method = "lda")
train_lda$results$Accuracy

plot(train_lda$finalModel$means[1,],train_lda$finalModel$means[2,])
#highest genes

train_qda <- train (x,y, method = "qda")
train_qda$results$Accuracy

plot(train_qda$finalModel$means[1,],train_qda$finalModel$means[2,])
train_qda$finalModel$means[1,]


train_lda <- train (x,y, method = "lda", preProcess = "center")
train_lda$results$Accuracy
train_lda$finalModel$means
plot(train_lda$finalModel$means[1,],train_lda$finalModel$means[2,])

set.seed(1993, sample.kind = "Rounding") # use this line of code if you are using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
train(x,y, method = "lda", preProcess = "center")$results$Accuracy

#Classification and Regression Trees (CART)
data("olive")
olive <- select(olive, -area)

library(caret)
fit <- train(region ~ ., method = "knn", tuneGrid = data.frame(k = seq(1,15,2)), data = olive)

fit <- rpart(margin ~ ., data = polls_2008)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

train_rpart <- train(margin ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data= polls_2008)
ggplot(train_rpart)

#Random Forests
train_rf <- randomForest(y ~ ., data = mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

fit <- train(y ~ .,method = "Rborist", tuneGrid = data.frame(predFixed = 2, minNode = seq(3,50)), data = mnist_27$train)
confusionMatrix(predict(fit, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]

#Comprehension Check: Trees and Random Forests
library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1, sample.kind = "Rounding")
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- rpart(y ~ ., data = dat)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)  

library(randomForest)
fit <- randomForest(y ~ x, data = dat)
dat %>% 
mutate(y_hat = predict(fit)) %>% 
ggplot() +
geom_point(aes(x, y)) +
geom_step(aes(x, y_hat), col = 2)  
  
plot(fit)

library(randomForest)
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)

dat %>% 
mutate(y_hat = predict(fit)) %>% 
ggplot() +
geom_point(aes(x, y)) +
geom_step(aes(x, y_hat), col = 2)

plot(fit)

#Caret Package
data("mnist_27")
train_glm <- train(y~.,method="glm",data=mnist_27$train)
train_knn <- train(y~.,method="knn",data=mnist_27$train)
y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hat_knn <- predict(train_knn, mnist_27$test, type = "raw")
confusionMatrix(y_hat_glm, mnist_27$test$y)$overall["Accuracy"]
confusionMatrix(y_hat_knn, mnist_27$test$y)$overall["Accuracy"]

#Tuning Parameters with Caret
getModelInfo("knn")
modelLookup("knn")

#highlight the k maximizing accuracy
ggplot(train_knn, highlight = TRUE)

#try different k beyond the default one
train_knn <- train(y~.,method="knn",data=mnist_27$train, 
                   tuneGrid = data.frame(k = seq(9,71,2)))

ggplot(train_knn, highlight = TRUE)

#k maximizing the accuracy
train_knn$bestTune

#best model
train_knn$finalModel

confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"), mnist_27$test$y)$overall["Accuracy"]

#ten fold validation (10 validation samples using 10% of the observation each)
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(y~.,method = "knn",data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(9,71,2)))
ggplot(train_knn_cv, highlight = TRUE)

#gamloess to make it smoother
modelLookup("gamLoess")

#gamLoess requires 2 parameters but we want to have the degree fix, so we set up a grid
grid <- expand.grid(span = seq(0.15,0.65,len = 10), degree = 1)
train_loess <- train(y~.,method = "gamLoess", data = mnist_27$train,
                     tuneGrid = grid)

ggplot(train_loess, highlight = TRUE)
train_loess$bestTune
confusionMatrix(predict(train_loess, mnist_27$test, type = "raw"), mnist_27$test$y)$overall["Accuracy"]


#Comprehension Check: Caret Package
modelLookup("Rborist")

#retrieve dat from previous excersise

minNode <- seq(25, 100, 25)
set.seed(1, sample.kind = "Rounding")

train_rborist <- train(y ~ x,method = "Rborist", tuneGrid = data.frame(predFixed = 1, minNode = minNode), data = dat)
ggplot(train_rborist, highlight = TRUE)
train_rborist$bestTune

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
  
data("tissue_gene_expression")

fit <- rpart(y ~ x, data = tissue_gene_expression)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

set.seed(1991, sample.kind = "Rounding")
train_rpart <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)))
ggplot(train_rpart)
train_rpart$bestTune

#answer
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rpart",
                  tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
ggplot(fit)            

confusionMatrix(train_rpart)

set.seed(1991, sample.kind = "Rounding")
train_rpart <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)), control = rpart.control(minsplit = 0))
ggplot(train_rpart)
train_rpart$bestTune

confusionMatrix(train_rpart)

fit <- rpart(y ~ x, data = tissue_gene_expression)
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

set.seed(1991, sample.kind = "Rounding")
train_rf <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "rf", tuneGrid = data.frame(mtry = seq(50, 200, 25)), nodesize = 1)
train_rf$bestTune

imp <- varImp(train_rf)
imp
#varImp(fit) is the answer

tree_terms <- as.character(unique(train_rpart$finalModel$frame$var[!(train_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms

imp


#Case Study: MNIST
mnist <- read_mnist()
names(mnist)
dim(mnist$train$images)
class(mnist$train$labels)
table(mnist$train$labels)
set.seed(123, sample.kind = "Rounding")
index <- sample(nrow(mnist$train$images),10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
index <- sample(nrow(mnist$test$images),1000)
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])

#show variability to eliminate useless data
library(matrixStats)
sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))
#remove these automatically with caret 
nzv <- nearZeroVar(x)
#remaining 252 columns
length(setdiff(1:ncol(x), nzv))

#Rborist package, use fewer trees at first
library(Rborist)
control <- trainControl(method = "cv", number = 5, p = 0.8)
grid <- expand.grid(minNode = c(1,5), predFixed = c(10,15,20,25,35,50))
#bug~~~
train_rf <- train(x[,col_index], y, 
                  method = "Rborist", nTree = 50, nSamp = 5000,
                  trControl = control, tuneGrid = grid)

#do it with more trees
train_rf <- Rborist(x[,col_index], y, 
                  nTree = 1000, 
                  minNode = train_rf$bestTune$minNode,
                  predFixed = train_rf$bestTune$predFixed)
y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[,col_index])$vPred])
cm <- confusionMatrix(y_hat_rf,y_test)
cm$overall["Accuracy"]

#use standard random forest package code (super long to run)
library(randomForest)
x <- mnist$train$images[index,]
y <- mnist$train$labels[index]
rf <- randomForest(x,y,ntree=50)
#show importance per feature, low importance one being at 0
imp <- importance(rf)
#plot image to show the importance
image(matrix(imp,28,28))

#show our mistakes
p_max <- predict(fit_knn, x_test[,col_index])
p_max <- apply(p_max,1,max)
ind <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]

#average probabilities of both algorithm to get a better one
p_rf <- predict(fit_rf, x_test[,col_index])$census
p_rf <- p_rf / rowSums(p_rf)
p_knn <- predict(fit_knn, x_test[,col_index])
p <- (p_rf + p_knn) / 2
y_pred <- factor(apply(p,1,which.max)-1)
confusionMatrix(y_pred,y_test)

#Comprehension Check: Ensembles
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)
set.seed(1, sample.kind = "Rounding")
data("mnist_27")

#train with a bunch of models
fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

#Q2
library(tidyverse)
trained<-sapply(fits,function(i){
  predict(i, mnist_27$test) %>% factor(levels = levels(mnist_27$test$y))
})

length(mnist_27$test$y)
length(models)
dim(trained)

#correct
pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))

#Q3 mean accuracy accross models
mod <- 1:length(models)
accuracy <- map_dbl(mod, function(i){
  y_hat <- trained[,i] %>%
    factor(levels = levels(mnist_27$test$y))
  mean(y_hat == mnist_27$test$y)
})

mean(accuracy)

#correct
acc <- colMeans(pred == mnist_27$test$y)

#q4 ensemble of prediction by majority vote
y_pred <- factor(apply(trained,1,function(i){
  ft <- table(i)
  names(ft[which.max(ft)])
}))
cm <- confusionMatrix(y_pred,mnist_27$test$y)
cm$overall["Accuracy"]

#correct
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")

#Q5 best performing accuracies
ind <- accuracy > mean(y_pred == mnist_27$test$y)
sum(ind)
models[ind]

#Q6 cross validation: retrieve only best accuracies per model
acc_hat <- sapply(mod, function(i){
  max(fits[[i]]$results$Accuracy)
}, simplify = "array")
#proper solution: 
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

#Q7 ensemble from cross validation best performers
ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)

#Comprehension Check: Dimension Reduction
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
library(matrixStats)

x <- tissue_gene_expression$x[,c(1,2)]
y <- tissue_gene_expression$y

#check dimensions with low variability
sds <- colSds(tissue_gene_expression$x)
hist(sds)

#keep only the 2 major
new_x <- tissue_gene_expression$x[,colSds(tissue_gene_expression$x)>2.5, drop=FALSE]
dim(new_x)  
hist (colSds(new_x))

as.data.frame(new_x,y) %>% 
  ggplot(aes(new_x[,1],new_x[,2],  color = y)) + 
  geom_point()

#retrieve principal component
pc <- prcomp(tissue_gene_expression$x)
pc$rotation
summary(pc)

data.frame(pc$x[,1:2], y=tissue_gene_expression$y) %>% 
  ggplot(aes(PC1,PC2, fill = y))+
  geom_point(cex=3, pch=21) +
  coord_fixed(ratio = 1)
#liver ok

#official code
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#average accross predictors
x_hat <- rowSums(tissue_gene_expression$x)/500

data.frame(pc_1 = pc$x[,1], x_hat, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, x_hat, color = tissue)) +
  geom_point()

cor(x_hat, pc$x[,1])

#correct one
avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
cor(avgs, pc$x[,1])

#remove center from pca
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()


data.frame(pc7 = pc$x[,7],
           tissue = tissue_gene_expression$y) %>% 
  ggplot(aes(y=pc7, x=tissue)) + geom_boxplot()

#correct code
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

summary()


importance_df <- data.frame(summary(pc)$importance)
importance_df <- importance_df[2,] %>% 
  gather(key = pc, value = importance)
importance_df <- importance_df %>% mutate(pc_index = as.integer(str_remove(importance_df$pc, "PC")))
importance_df$pc <- factor(importance_df$pc, levels = importance_df$pc[order(importance_df$pc_index)])
importance_df <- importance_df %>% mutate(cum_sum = cumsum(importance))

importance_df %>% 
  filter(pc_index < 20) %>% 
  arrange(pc_index, cum_sum) %>% 
  ggplot(aes(x = pc, y = cum_sum, fill=pc)) +
  geom_col() +
  scale_y_continuous(breaks = seq(0,1,0.1)) +
  theme_grey()

summary(importance_df)

#correct
plot(summary(pc)$importance[3,])

#Recommendation Systems
library(dslabs)
data("movielens")
head(movielens)
#total (distinct) number of users and movies
movielens %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))

library(caret)
set.seed(677, sample.kind = "Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.2, list = FALSE)
train_set <- movielens[-test_index,]
test_set <- movielens[test_index,]

#remove movies and users from test set that are not in train set
test_set <- test_set %>%
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)))
}

mu_hat <- mean(train_set$rating)
mu_hat  

#baseline RMSE using the average
naive_rmse <- RMSE(test_set$rating, mu_hat)

#store results of different approached
rmse_results <- data.frame(method = "Just the average", RMSE = naive_rmse)

#bias for a specific movie, average per movie being different
#slow version
fit <- lm(rating ~ as.factor(userId), data = movielens)

mu <- mean(train_set$rating)
movie_avg <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

#adding the new model
predicted_ratings <- mu + test_set %>%
  left_join(movie_avg, by = "movieId") %>%
  .$b_i

model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, 
                          data.frame(method = "Movie effect model", RMSE = model_1_rmse))
rmse_results %>% knitr::kable()

#user specific effect, approximated
user_avgs <- test_set %>%
  left_join(movie_avg, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
  
predicted_ratings <- mu + test_set %>%
  left_join(movie_avg, by = "movieId") %>%
  left_join(movie_avg, by="userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results, 
                          data.frame(method = "Movie + User effect model", RMSE = model_2_rmse))
rmse_results %>% knitr::kable()

#Comprehension Check: Recommendation Systems
library(dslabs)
data("movielens")
names(movielens)

#numbers
movielens %>%
  group_by(movieId,year) %>%
  summarise(n = sqrt(n())) %>%
  group_by(year) %>%
  summarise(med = median(n)) %>% 
  arrange(desc(med)) %>%
  head()

#graph
movielens %>%
  group_by(movieId, year) %>%
  summarise(n=(n())) %>%
  ggplot(aes(x=year, y=sqrt(n), group=year)) +
  geom_boxplot()

#25 movies with most rating per year
#quantile for top 25%  filter(n >= quantile(n,.75)) %>% 
sortedList <- movielens %>%
  filter(year >= 1993) %>%
  group_by(title, movieId, year) %>%
  summarise(n=n(), avg = mean(rating)) %>%
  group_by(year) %>%
  mutate(rank = order(order(n, year, decreasing=TRUE))) %>%
  filter(rank <= 25) %>%
  arrange(desc(rank)) 

library(anytime)  
library(lubridate)
sortedList %>% filter(title == "Shawshank Redemption, The")
movielens %>% 
  filter(title == "Forrest Gump") %>%
  summarise(n= n()) / (2018-1994)

#not necessary but keep for the sake of it
mutate(ratingyear = year(anydate(timestamp))) %>%
group_by(yearyear)  %>%
summarise(n= n()) %>% summarize(sumsum = sum(n))
summarize(avg = mean(n))

#average rating vs ratings post 1993
forplot <- 
  movielens %>%
  filter(year >= 1993) %>%
  group_by(movieId, year) %>%
  summarise(n=sqrt(n()), avg = mean(rating)) %>%
  semi_join(sortedList, by = "movieId") %>%
  ggplot(aes(n,avg)) +
  geom_point()

cor(forplot$avg,forplot$n)

#average rating vs rating per year post 1993
forplot <- 
  movielens %>%
  filter(year >= 1993) %>%
  mutate(ratingyear = year(anydate(timestamp))) %>%
  group_by(movieId, year,ratingyear) %>%
  summarise(n=n(), avg = mean(rating)) %>%
  group_by(movieId, year) %>%
  mutate(ratingsperyear = sqrt(n/n())) %>%
  semi_join(sortedList, by = "movieId") %>%
  ggplot(aes(ratingsperyear,avg)) +
  geom_point()

cor(forplot$avg,forplot$ratingsperyear)

#correct The more often a movie is rated, the higher its average rating.

movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2017 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

#Fill in the missing values with a lower value than the average rating across all movies.

movielens <- mutate(movielens, date = as_datetime(timestamp))


movielens %>% 
  filter(year >= 1993) %>%
  mutate(week = round_date(date, unit = "week")) %>%
  group_by(week) %>%
  summarize(n = n(), 
            rating = mean(rating)) %>%
  ggplot(aes(week, rating)) +
  geom_point() +
  geom_smooth()

#correct There is some evidence of a time effect on average rating.
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

#𝑌𝑢,𝑖=𝜇+𝑏𝑖+𝑏𝑢+𝑓(𝑑𝑢,𝑖)+𝜀𝑢,𝑖, with 𝑓 a smooth function of 𝑑𝑢,𝑖 correct

movielens %>%
  add_count(genres) %>%
  filter(n >= 1000) %>%
  group_by(genres) %>%
  summarize(avg = mean(rating), sd = sd(rating)) %>%
  arrange(avg) %>%
  head()

#=𝜇+𝑏𝑖+𝑏𝑢+∑𝑘=1𝐾𝑥𝑢,𝑖𝛽𝑘+𝜀𝑢,𝑖, with 𝑥𝑘𝑢,𝑖=1 if 𝑔𝑢,𝑖 is genre 𝑘 correct

  ggplot(aes(genres)) +
  geom_bar()

#Regularization
movie_titles <- movielens %>%
  select(movieId,title) %>%
  distinct()

movie_avg %>% left_join(movie_titles, by = "movieId") %>%
  arrange(desc(b_i)) %>% #  arrange((b_i)) %>%
  select(title, b_i, n) %>%
  slice(1:10) %>%
  knitr::kable()

#regularize by penalizing movies with less ratings
lambda <- 3
mu <- mean(train_set$rating)
movie_reg_avgs <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu) / (n() + lambda), n_i = n())

#predict from regularized
predicted_ratings <- test_set %>%
  left_join(movie_reg_avgs, by="movieId") %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings,test_set$ratings)
rmse_results <- bind_rows(rmse_results, 
                          data.frame(method = "Regularized movie effect model", RMSE = model_3_rmse))
rmse_results %>% knitr::kable()

#use cross validation to pick lambda
lambdas <- seq(0,10,0.25)

mu <- mean(train_set$rating)
just_the_sum <- train_set %>%
  group_by(movieId) %>%
  summarize(s = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>%
    left_join(just_the_sum, by="movieId") %>%
    mutate(b_i = s / (n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings,test_set$ratings))
})

#add user effect to get best lambda

lambda <- lambdas[which.min(rmses)]

#Comprehension Check: Regularization
library(tidyverse)
set.seed(1986, sample.kind = "Rounding")
n <- round(2^rnorm(1000, 8, 1))
set.seed(1, sample.kind = "Rounding")
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))

schools %>% top_n(10, quality) %>% arrange(desc(quality))

set.seed(1, sample.kind = "Rounding")
mu <- round(80 + 2*rt(1000, 5))
scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

top_10_schools <- schools %>% 
  top_n(10, score) %>% 
  arrange(desc(score))
top_10_schools

median(schools$size)
median(top_10_schools$size)

#bot 10
median(schools %>% 
         top_n(-10, score)  %>% .$size)

library(gghighlight)
schools %>%
  ggplot(aes(log(size), score)) +
  geom_point() +
  gghighlight(id %in% top_10_schools$id & size %in% top_10_schools$size) +
  geom_smooth()

#correct
schools %>% ggplot(aes(size, score)) +
  geom_point(alpha = 0.5) +
  geom_point(data = filter(schools, rank<=10), col = 2) 
#The standard error of the score has larger variability when the school is smaller, which is why both the best and the worst schools are more likely to be small.

overall <- mean(sapply(scores, mean))
alpha <- 25
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#use cross validation to pick alphas
alphas <- seq(10,250,1)
score_reg <- sapply(scores, function(x) sum(x-overall))
rmses <- sapply(alphas, function(l){
  schools_reg <- schools %>% 
    mutate(score_reg = overall + score_reg / (l + size))  
  return(sqrt(sum((schools_reg$quality - schools_reg$score_reg)^2)/1000))
})
alphas[which.min(rmses)]

#Q7
alpha <- 135
score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
schools %>% mutate(score_reg = score_reg) %>%
  top_n(10, score_reg) %>% arrange(desc(score_reg))

#Q8
alphas <- seq(10,250,1)
score_reg <- sapply(scores, function(x) sum(x))
rmses <- sapply(alphas, function(l){
  schools_reg <- schools %>% 
    mutate(score_reg =   score_reg / (l + size))  
  return(sqrt(sum((schools_reg$quality - schools_reg$score_reg)^2)/1000))
})
alphas[which.min(rmses)]

#Matrix Factorization
train_small <- movielens %>%
  group_by(movieId) %>%
  filter(n() >= 50 || movieId == 3252) %>% ungroup() %>%
  group_by(userId) %>%
  filter(n() >= 50) %>% ungroup()

y <- train_small %>%
  select(userId, movieId, rating) %>%
  spread(movieId, rating) %>%
  as.matrix()

rownames(y) <- y[,1]
y <- y[,-1]

colnames(y) <- with(movie_titles, title[match(colnames(y), movieId)])
  
#convert to residual by removing columns and rows averages
y <- sweep(y,1,rowMeans(y,na.rm=TRUE))
y <- sweep(y,2,colMeans(y,na.rm=TRUE))

#SVD and PCA
y[is.na(y)] <- 0
y <- sweep(y,1,rowMeans(y))
pca <- prcomp(y)
#principal components (vector q)
pca$rotation
#user effects (vector p)
pca$x
#variability of pricipal component
pca$dev
#explain variance
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)
#first principal components
pcs <- data.frame(pca$rotation, name = str_trunc(colnames(y), 30))
pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)
pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

#Comprehension Check: Matrix Factorization
set.seed(1987, sample.kind = "Rounding")
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

#Q1 The students that test well are at the top of the image and there seem to be three groupings by subject. 
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

#Q2 There is correlation among all tests, but higher if the tests are in science and math and even higher within each subject.  
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#Q3
s <- svd(y)
names(s)

y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

ss_y <- y^2
ss_yv <- (y %*% s$v)^2
sum(ss_yv)
q3 <- c(sum(ss_y),sum(ss_yv))
q3[1]

#correct
ss_y <- apply(y^2, 2, sum)
ss_yv <- apply((y%*%s$v)^2, 2, sum)
sum(ss_y)
sum(ss_yv)

#q4
colMeans(ss_y)
colMeans(ss_yv)
plot(seq(1,24),colMeans(ss_y))
plot(seq(1,24),colMeans(ss_yv))

#q5
plot(sqrt(colSums(ss_yv)), s$d)
#correct
data.frame(x = sqrt(ss_yv), y = s$d) %>%
  ggplot(aes(x,y)) +
  geom_point()

#q6
y_v <- y%*%s$v
y_v <- sweep(y,1,rowMeans(y_v))
pca <- prcomp(y_v)
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)
var_explained[3]
#correct
sum(s$d[1:3]^2) / sum(s$d^2)

identical(s$u %*% diag(s$d), sweep(s$u, 2, s$d, FUN = "*"))

avg_s <- rowMeans(y)
ud <- sweep(s$u, 2, s$d, FUN = "*")
plot(ud[,1],avg_s)
#correct
plot(-s$u[,1]*s$d[1], rowMeans(y))

my_image(s$v)

#q10
my_image(s$u[,1],drop = FALSE, zlim = c(-1,1))

#Comprehension Check: Clustering 
#https://rafalab.github.io/dsbook/clustering.html
library(dslabs)
data(tissue_gene_expression)
#Q1
d <- sweep(tissue_gene_expression$x, 1, rowMeans(tissue_gene_expression$x))
d <- dist(d)
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))

#Q2 hierarchical clustering plot / cluster dendogram
h <- hclust(d)
plot(h, cex = 0.65)

#Q3 k-means clustering 
k <- kmeans(d, centers = 7)
groups <- k$cluster
split(names(groups), groups)

#correct
cl <- kmeans(tissue_gene_expression$x, centers = 7)
table(cl$cluster, tissue_gene_expression$y)

#q4
library(RColorBrewer)
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)

#also from the book
groups <- cutree(h, k = 7)
split(names(groups), groups)
h_2 <- dist(t(x)) %>% hclust()
plot(h_2, cex = 0.35)

#Breast Cancer Project 
options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
library(dslabs)
data(brca)

length(brca$y)
dim(brca$x)
table(brca$y)/length(brca$y)
which.max(colMeans(brca$x))
which.min(colSds(brca$x))

#Q2 scaling
x <- sweep(brca$x, 2, colMeans(brca$x), FUN = "-")
x <- sweep(x, 2 , colSds(brca$x), FUN = "/")

colSds(x)
colMedians(x)

#correct
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")
sd(x_scaled[,1])

#Q3 distance
d <- dist(x)
dim(as.matrix(d))
mean(as.matrix(d)[1,brca$y == "B"][-1])
mean(as.matrix(d)[1,brca$y == "M"])

#correct
d_samples <- dist(x_scaled)
dist_BtoB <- as.matrix(d_samples)[1, brca$y == "B"]
mean(dist_BtoB[2:length(dist_BtoB)])
dist_BtoM <- as.matrix(d_samples)[1, brca$y == "M"]
mean(dist_BtoM)

#Q4
heatmap(t(x[,ind]),labRow = NA,labCol = NA)
#not the last one Incorrect: Try again. This heatmap is the whole scaled matrix.

#Q5
h <- hclust(d)
h <- hclust(dist(t(x)))
plot(h, cex = 0.65)
groups <- cutree(h, k = 5)
split(colnames(x), groups)

cl <- kmeans(d, centers = 5)
table(cl$cluster, brca$y)

#Question 6: PCA: proportion of variance
pca <- prcomp(x)
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)
length(var_explained)-sum(var_explained>0.9)

#Question 7: PCA: plotting PCs
for(i in 1:10){
  boxplot(pca$x[,i] ~ brca$y, main = paste("PC", i))
}

#correct
data.frame(pca$x[,1:2], type = brca$y) %>%
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point()

#Question 8: PCA: PC boxplot
for(i in 1:10){
  boxplot(pca$x[,i] ~ brca$y, main = paste("PC", i))
}

#correct
data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()

#Question 9: Training and test sets
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]

mean(train_y == "B")
mean(test_y == "B")

#Question 10a: K-means Clustering
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}
set.seed(3, sample.kind = "Rounding")    # if using R 3.6 or later
x_0 <- train_x
x_0[is.na(x_0)] <- 0
k <- kmeans(train_x, centers = 2)
mean(predict_kmeans(test_x,k) == as.numeric(test_y))

#correct
set.seed(3, sample.kind = "Rounding")    # if using R 3.6 or later
k <- kmeans(train_x, centers = 2)
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M") %>% factor()
mean(kmeans_preds == test_y)

#Question 10b: K-means Clustering
sum(kmeans_preds == test_y & kmeans_preds == "B")/sum(test_y == "B")
sum(kmeans_preds == test_y & kmeans_preds == "M")/sum(test_y == "M")
sensitivity(factor(kmeans_preds), test_y, positive = "B")
sensitivity(factor(kmeans_preds), test_y, positive = "M")

#Question 11: Logistic regression model
train <- data.frame(y = train_y, train_x)
test <- data.frame(y = test_y, test_x)

train_glm <- train(y~.,method="glm",data=train)
y_hat_glm <- predict(train_glm, test, type = "raw")
confusionMatrix(y_hat_glm, test$y)$overall["Accuracy"]

#Question 12: LDA and QDA models
train_lda <- train(y~.,method="lda",data=train)
y_hat_lda <- predict(train_lda, test, type = "raw")
confusionMatrix(y_hat_lda, test$y)$overall["Accuracy"]
train_qda <- train(y~.,method="qda",data=train)
y_hat_qda <- predict(train_qda, test, type = "raw")
confusionMatrix(y_hat_qda, test$y)$overall["Accuracy"]

#Question 13: Loess model
set.seed(5, sample.kind = "Rounding")    # if using R 3.6 or later
train_loess <- train(y~.,method="gamLoess",data=train)
y_hat_loess <- predict(train_loess, test, type = "raw")
confusionMatrix(y_hat_loess, test$y)$overall["Accuracy"]

#Question 14: K-nearest neighbors model
set.seed(7, sample.kind = "Rounding")    # if using R 3.6 or later
train_knn <- train(y~.,method="knn",data=train, 
                   tuneGrid = data.frame(k = seq(3, 21, 2)))
plot(train_knn)
train_knn$bestTune
y_hat_knn <- predict(train_knn, test, type = "raw")
confusionMatrix(y_hat_knn, test$y)$overall["Accuracy"]

#Question 15a: Random forest model
set.seed(9, sample.kind = "Rounding")    # if using R 3.6 or later
train_rf <- train(train_x,train_y, method = "rf", tuneGrid = data.frame(mtry = seq(3, 9, 2)), mportance=TRUE)
train_rf$bestTune
y_hat_rf <- predict(train_rf, test, type = "raw")
confusionMatrix(y_hat_rf, test$y)$overall["Accuracy"]
(varImp(train_rf))
#not concave_pts_worst not perimeter_worst

#Question 16a: Creating an ensemble
y_hat <- cbind(kmeans_preds,y_hat_glm,y_hat_lda,y_hat_qda,y_hat_loess,y_hat_knn,y_hat_rf)
pred <- ifelse(rowMeans(y_hat == 1) > 0.5, "B", "M")
mean(pred == test_y)
models <- ifelse((y_hat == 1) > 0.5, "B", "M")
colMeans(models == test_y)

#Titanic Exercises
library(titanic)    # loads titanic_train data frame
library(caret)
library(tidyverse)
library(rpart)

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived, Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)




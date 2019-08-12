################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- "/Users/ayumaki/Documents/max's/Dev/HarvardXPH125/projects/ml-10M100K/"


ratings <- fread(text = gsub("::", "\t", readLines(paste(dl, "ratings.dat", sep = ""))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(paste(dl, "movies.dat", sep = "")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

#sample to 10K rows
#movielens <- movielens[sample(nrow(movielens),10000),]

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

################################
# Prepare train & test sets to select lowest RMSE without using validation
################################

# Prepare a train and test sets from edx at 10% of edx data

test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index,]
temp <- edx[test_index,]

# Make sure userId and movieId in edx_test set are also in edx_train set

edx_test <- temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# Add rows removed from edx_test set back into edx_train set

removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, removed)

rm(test_index, temp, removed)

################################
# Get lowest RMSE
################################

# Prepare a RMSE function to make future calculations or RMSE easier

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Prepare a train & predict function to resue on different lambdas
train_predict <- function(l, tr = edx_train, te = edx_test){
  mu <- mean(tr$rating)
  b_i <- tr %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- tr %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    te %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, te$rating))
}

# Find lowest RMSE for different lambdas and use it to set optimal lambda

lambdas <- seq(0, 100, 10)
lambdas[which.min(sapply(lambdas, train_predict))]
lambdas <- seq(0, 20, 1)
lambdas[which.min(sapply(lambdas, train_predict))]
lambdas <- seq(4, 6, 0.1)
lambda <- lambdas[which.min(sapply(lambdas, train_predict))]

# Get RMSE for the best lambda using the whole train set and the validation set

rmse <- train_predict(lambda, edx, validation)

rmse

################################
# sandbox time
################################

#calculate average for train set

# prepare genre version of test set
edx_test_genre <- edx_test %>% 
  separate_rows(genres, sep = "\\|")

edx_train_genre <- edx_train %>% 
  separate_rows(genres, sep = "\\|")

# movie user genre effect
train_predict <- function(l, tr = edx_train_genre, te = edx_test_genre){
  print(l)
  mu <- mean(tr$rating)
  b_i <- tr %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- tr %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  b_g <- tr %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - mu - b_u)/(n()+l))
  predicted_ratings <- 
    te %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  rmse <- sqrt(mean((te$rating - predicted_ratings)^2))
  print(rmse)
  return(rmse)
}

lambdas <- seq(12.2, 12.8, 0.1)
rmses <- sapply(lambdas, train_predict, tr = edx_train_genre, te = edx_test_genre)
min(rmses)
lambdas[which.min(rmses)]
lambda <- 12.75 #0.863828

# optimize genre only

train_predict_genre <- function(l1, tr = edx_train, te = edx_test){
  b_g <- tr %>% 
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu)/(n()+l1))
  predicted_ratings <- 
    te %>% 
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_g) %>%
    .$pred
  return(RMSE(predicted_ratings, te$rating))
}


lambdas <- seq(77, 79, 0.1)
rmses <- sapply(lambdas, train_predict_genre, tr = edx_train_genre, te = edx_test_genre)

# keep best b_g
l1 <- lambdas[which.min(rmses)]
b_g <- edx_train_genre %>% 
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu)/(n()+l1))

edx_train_genre <- edx_train_genre %>%
  left_join(b_g, by="genres")

edx_test_genre <- edx_test_genre %>%
  left_join(b_g, by="genres")

# optimize movie only

train_predict_movie <- function(l2, tr = edx_train, te = edx_test){
  b_i <- tr %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l2))
  predicted_ratings <- 
    te %>% 
    left_join(b_i, by = "movieId") %>%
    mutate(pred = mu + b_i + b_g) %>%
    .$pred
  return(RMSE(predicted_ratings, te$rating))
}

lambdas <- seq(6, 8, 0.1)
rmses <- sapply(lambdas, train_predict_movie, tr = edx_train_genre, te = edx_test_genre)
min(rmses)

# keep best b_i
l2 <- lambdas[which.min(rmses)]
b_i <- edx_train_genre %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu )/(n()+l2))

edx_train_genre <- edx_train_genre %>%
  left_join(b_i, by="movieId")

edx_test_genre <- edx_test_genre %>%
  left_join(b_i, by="movieId")

# optimize user only

train_predict_user <- function(l3, tr = edx_train, te = edx_test){
  b_u <- tr %>% 
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu )/(n()+l3))
  predicted_ratings <- 
    te %>% 
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  return(RMSE(predicted_ratings, te$rating))
}

lambdas <- seq(13, 15, 0.1)
rmses <- sapply(lambdas, train_predict_user, tr = edx_train_genre, te = edx_test_genre)
min(rmses)

# keep best b_u
l3 <- lambdas[which.min(rmses)]

b_u <- edx_train_genre %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu - b_g)/(n()+l3))

edx_train_genre <- edx_train_genre %>%
  left_join(b_u, by="userId")

edx_test_genre <- edx_test_genre %>%
  left_join(b_u, by="userId")

#try again: just run the genre first then all together
edx_train_genre <- edx_train_genre %>%
  select(-b_i)
edx_test_genre <- edx_test_genre %>%
  select(-b_i)

train_predict <- function(l, tr = edx_train, te = edx_test){
  b_i <- tr %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu - b_g)/(n()+l))
  b_u <- tr %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu - b_g)/(n()+l))
  predicted_ratings <- 
    te %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  return(RMSE(predicted_ratings, te$rating))
}

lambdas <- seq(12, 14, 0.1)
rmses <- sapply(lambdas, train_predict, tr = edx_train_genre, te = edx_test_genre)
min(rmses)
lambdas[which.min(rmses)]


# movie genre effect?
train_predict_genre <- function(l1,l2 = 4.6,l3 = 4.6, tr = edx_train_genre, te = edx_test){
  mu <- mean(tr$rating)
  b_g <- tr %>% 
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu)/(n()+l1))
  b_i <- tr %>%
    left_join(b_g, by="genres") %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu - b_g)/(n()+l2))
  b_u <- tr %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_g, by="genres") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu - b_g)/(n()+l3))
  predicted_ratings <- 
    te %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  return(RMSE(predicted_ratings, te$rating))
}

train_predict(4.6,4.6,50)

lambdas <- seq(0, 20, 1)
lambdas[which.min(sapply(lambdas, train_predict, l1 = 4.6, l2 = 4.6, tr = edx_train, te = edx_test))]

edx_bygenre <- edx %>% 
  separate_rows(genres, sep = "\\|") %>%
  mutate(flag = 1) %>%
  spread(genres, flag, fill = 0)

edx_test_genre <- edx_test %>% 
  separate_rows(genres, sep = "\\|")

edx_train_genre <- edx_train %>% 
  separate_rows(genres, sep = "\\|")

edx_genre <- edx %>% 
  separate_rows(genres, sep = "\\|")

validation_genre <- validation %>% 
  separate_rows(genres, sep = "\\|")

train_predict_genre(100000000, l2 = 4.6, l3 = 4.6, tr = edx_train_genre, te = edx_test_genre)
#until now: 0.8648224
# with 10,000,000 0.8642455
#100,000,000 best 0.8629

train_predict_genre(100000000, l2 = 4.6, l3 = 4.6, tr = edx_genre, te = validation_genre)


lambdas <- seq(100000, 1000000, 100000)
lambdas[which.min(sapply(lambdas, train_predict_genre, l2 = 4.6, l3 = 4.6, tr = edx_train_genre, te = edx_test_genre))]


#simplified / faster
mu <- mean(edx_train$rating)
b_i <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+4.6))
b_u <- edx_train %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+4.6))

genre_predict <- function(l3, tr = edx_train, te = edx_test){
  b_g <- tr %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - mu - b_u)/(n()+l3))
  predicted_ratings <- 
    te %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  return(RMSE(predicted_ratings, te$rating))
}

lambdas <- seq(1, 3, 0.1)
lambdas[which.min(sapply(lambdas, genre_predict, tr = edx_train, te = edx_test))]

genre_predict(2.4)

bymovie <- edx %>%
  group_by(movieId) %>%
  summarise(total = n()) %>%
  .$total

fivenum(bymovie)

byuser <- edx %>%
  group_by(userId) %>%
  summarise(total = n()) %>%
  .$total

fivenum(byuser)

bygenres <- edx_train_genre %>%
  group_by(genres) %>%
  summarise(total = n()) %>%
  .$total

fivenum(bygenres)

# end

mu <- mean(edx_train$rating)

b_i_sum <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu), b_i_n = n())

b_u_sum <- edx_train %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu), b_u_n = n())

x <- edx_train %>% 
  group_by(userId) %>%
  filter(n() >= 5) %>%
  ungroup() %>% 
  select(movieId, userId, rating) %>%
  spread(userId, rating)

x <- x[,-1] %>% as.matrix()
x <- sweep(x, 2, colMeans(x, na.rm = TRUE))
x <- sweep(x, 1, rowMeans(x, na.rm = TRUE))

d <- dist(x) 
h <- hclust(d)

plot(h, cex = 0.65)


top <- edx %>%
  group_by(movieId) %>%
  summarize(n=n(), title = first(title)) %>%
  top_n(50, n) %>%
  pull(movieId)

x <- edx %>% 
  filter(movieId %in% top) %>%
  group_by(userId) %>%
  filter(n() >= 25) %>%
  ungroup() %>% 
  select(title, userId, rating) %>%
  spread(userId, rating)

row_names <- str_remove(x$title, ": Episode") %>% str_trunc(20)
x <- x[,-1] %>% as.matrix()
x <- sweep(x, 2, colMeans(x, na.rm = TRUE))
x <- sweep(x, 1, rowMeans(x, na.rm = TRUE))
rownames(x) <- row_names

d <- dist(x)

h <- hclust(d)

plot(h, cex = 0.65)

groups <- cutree(h, k = 10)
split(names(groups), groups)

h_2 <- dist(t(x)) %>% hclust()

plot(h_2, cex = 0.35)

library(matrixStats)
sds <- colSds(x, na.rm = TRUE)
o <- order(sds, decreasing = TRUE)[1:25]
heatmap(x[,o], col = RColorBrewer::brewer.pal(11, "Spectral"))

cl <- kmeans(edx, centers = 7)
table(cl$cluster, edx$rating)


#need to make a different validation set here
lambdas <- seq(5, 5.5, 0.1)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda
min(rmses)
########



mu_hat <- mean(edx$rating)
naive_rmse <- RMSE(validation$rating, mu_hat)
rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)
mu <- mean(edx$rating) 
movie_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  .$b_i
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = model_1_rmse ))
user_avgs <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  .$pred
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = model_2_rmse ))

validation %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10) %>% knitr::kable()

movie_titles <- movielens %>% 
  select(movieId, title) %>%
  distinct()
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10) %>%  
  knitr::kable()

edx %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

edx %>% dplyr::count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

lambda <- 3
mu <- mean(edx$rating)
movie_reg_avgs <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n()) 

data_frame(original = movie_avgs$b_i, 
           regularlized = movie_reg_avgs$b_i, 
           n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

edx %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

edx %>%
  dplyr::count(movieId) %>% 
  left_join(movie_reg_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i, n) %>% 
  slice(1:10) %>% 
  knitr::kable()

predicted_ratings <- validation %>% 
  left_join(movie_reg_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie Effect Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

lambdas <- seq(0, 10, 0.25)
mu <- mean(edx$rating)
just_the_sum <- edx %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- validation %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(edx$rating)
  b_i <- edx %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings <- 
    validation %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Movie + User Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

#pca
y[is.na(y)] <- 0
y <- sweep(y, 1, rowMeans(y))
pca <- prcomp(y)

dim(pca$rotation)

dim(pca$x)

plot(pca$sdev)

var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

library(ggrepel)
pcs <- data.frame(pca$rotation, name = colnames(y))
pcs %>%  ggplot(aes(PC1, PC2)) + geom_point() + 
  geom_text_repel(aes(PC1, PC2, label=name),
                  data = filter(pcs, 
                                PC1 < -0.1 | PC1 > 0.1 | PC2 < -0.075 | PC2 > 0.1))

pcs %>% select(name, PC1) %>% arrange(PC1) %>% slice(1:10)

pcs %>% select(name, PC1) %>% arrange(desc(PC1)) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(PC2) %>% slice(1:10)

pcs %>% select(name, PC2) %>% arrange(desc(PC2)) %>% slice(1:10)

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
models <- c("glm", "svmLinear", "knn", "gamLoess", "rf")

#train with a bunch of models
fits <- lapply(models, function(model){ 
  print(model)
  train(rating ~ (movieId), method = model, data = edx)
}) 

names(fits) <- models


control <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"

# Linear Discriminant Analysis
set.seed(1)
fit.lda <- train(rating~movieId, data=edx, method="lda", metric=metric, preProc=c("center", "scale"), trControl=control)
fit.glm <- train(rating~movieId, data=edx, method="glm", trControl=control)
fit.knn <- train(rating~movieId, data=edx, method="knn", trControl=control)

summary(resamples(list(fit.glm$Accuracy,fit.knn$Accuracy)))

results <- resamples(list(lda=fit.lda, logistic=fit.glm, glmnet=fit.glmnet,
                          svm=fit.svmRadial, knn=fit.knn, nb=fit.nb, cart=fit.cart, c50=fit.c50,
                          bagging=fit.treebag, rf=fit.rf, gbm=fit.gbm))
# Table comparison
summary(results)


p_hat_logit <- predict(glm_fit, newdata = validation, type = "response")

y_hat_logit <- ifelse(p_hat_logit > 0.5,"Female","Male") %>% factor()
confusionMatrix(y_hat_logit, validation$sex)





#sandboxing

#exploratory

#average rating by genre
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n(), avg = mean(rating)) %>%
  top_n(25) %>%
  arrange(desc(count))

#standard deviation by user
edx %>% 
  group_by(userId) %>%
  summarize(count = n(), avg = mean(rating), sd = sd(rating)) %>%
  arrange(desc(count)) %>%
  head()

#standard deviation by movie
edx %>% 
  group_by(movieId) %>%
  summarize(count = n(), avg = mean(rating), sd = sd(rating)) %>%
  arrange(desc(count)) %>%
  head()

#plot ratings by time for one movie
edx %>% filter(title == "Pulp Fiction (1994)") %>%
  ggplot(aes(timestamp)) +
  geom_bar()

#https://machinelearningmastery.com/evaluate-machine-learning-algorithms-with-r/





################################
# Working on data set
################################

# Split by genres to use as predictor
edx_bygenre <- edx %>% 
  separate_rows(genres, sep = "\\|") %>%
  mutate(flag = 1) %>%
  spread(genres, flag, fill = 0)



# Regularize the data

#1 Regularize by movie
sample <- sample_10K
mu <- mean(sample$rating)
lambda <- 5
edx_bymovieId <- sample %>%
  group_by(movieId) %>%
  summarize(s = mu + sum(rating - mu) / (n() + lambda))

#2 Regularize by user
mu <- mean(sample$rating)
lambda <- 5
edx_byuserId <- sample %>%
  group_by(userId) %>%
  summarize(s = sum(rating - mu) / (n() + lambda)) 

#3 Join
sample <- sample %>%
  left_join(edx_bymovieId, by = "movieId") %>%
  left_join(edx_byuserId, by = "userId") 
#which rating matters???  

# Try
train_loess <- train(rating~movieId,method="gamLoess",data=edx)
y_hat_loess <- predict(train_loess, validation, type = "raw")
confusionMatrix(y_hat_loess, validation$rating)$overall["Accuracy"]

pca <- prcomp(sample[,7:25])
var_explained <- cumsum(pca$sdev^2/sum(pca$sdev^2))
plot(var_explained)

#2 Get RMSEs for a range of lambda
lambda <- 5

edx_bymovieId <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu) / (n() + lambda), n_i = n())

#predict from regularized
predicted_ratings <- validation %>%
  left_join(edx_bymovieId, by="movieId") %>%
  mutate(pred = mu + b_i) %>%
  .$pred

model_3_rmse <- RMSE(predicted_ratings,validation$rating)

# Test out different models
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

fits <- lapply(models[1], function(model){ 
  train(rating ~ movieId+userId, method = model, data = sample_10K)
}) 

pred <- sapply(fits, function(object) 
  predict(object, newdata = validation))

acc <- colMeans(pred == validation$rating)
votes <- rowMeans(pred == "7")
y_hat <- ifelse(votes > 0.5, "7", "2")




rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>%
    left_join(just_the_sum, by="movieId") %>%
    mutate(b_i = s / (n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    .$pred
  return(RMSE(predicted_ratings,validation$ratings))
})

lambdas <- seq(0,10,0.25)





pca <- prcomp(y)

x <- sweep(brca$x, 2, colMeans(brca$x), FUN = "-")
x <- sweep(x, 2 , colSds(brca$x), FUN = "/")

sum(!is.na(edx$rating))





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


RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)))
}

#bootstrap
indexes <- createResample(edx_bygenre, 10)
quantile(edx_bygenre[indexes$Resample02],0.75)

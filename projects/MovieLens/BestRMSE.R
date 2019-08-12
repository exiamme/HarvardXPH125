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

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

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

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# prepare genre version of test set
edx_test_genre <- edx_test %>% 
  separate_rows(genres, sep = "\\|")

edx_train_genre <- edx_train %>% 
  separate_rows(genres, sep = "\\|")

# a little bit exploratory: where are the top factors?

#genre
edx_train_genre %>% 
  group_by(genres) %>%
  summarize(count = n(), avg = mean(rating)) %>%
  arrange(desc(count)) %>%
  head()

#movie
edx_train_genre %>% 
  group_by(movieId) %>%
  summarize(count = n(), avg = mean(rating)) %>%
  arrange(desc(count)) %>%
  head()

#user
edx_train_genre %>% 
  group_by(userId) %>%
  summarize(count = n(), avg = mean(rating)) %>%
  arrange(desc(count)) %>%
  head()

# evaluate for movie then user with some verbose to follow
# use non genre split version to avoid penalizing by the number of genres attributed to a movie

train_predict <- function(l, tr, te){
  print(l)
  mu <- mean(tr$rating)
  b_i <- tr %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- tr %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  predicted_ratings <- 
    te %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u ) %>%
    .$pred
  rmse <- sqrt(mean((te$rating - predicted_ratings)^2))
  print(rmse)
  return(rmse)
}

lambdas <- seq(4, 6, 0.1)
rmses <- sapply(lambdas, train_predict, tr = edx_train, te = edx_test)
min(rmses)
lambdas[which.min(rmses)]

# update data set based on prediction
l <- lambdas[which.min(rmses)]
mu <- mean(edx_train$rating)
b_i <- edx_train %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))
b_u <- edx_train %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+l))

# add number of genre per movie in case we do not want to add that penalty
edx_train_genre <- edx_train_genre %>% 
  select(movieId,genres) %>%
  distinct() %>%
  group_by(movieId) %>%
  mutate(genre_count = n()) %>%
  select(genre_count, movieId) %>%
  inner_join(edx_train_genre, by = "movieId") 

edx_train_genre <- edx_train_genre %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") 

edx_test_genre <- edx_test_genre %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") 

# try to refine it with genre
# just penalizing by number of genre seems to lower RMSE to 0.8639613

train_predict_genre <- function(l, tr, te){
  print(l)
  b_g <- tr %>% 
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - mu - b_u)/(n()+l))
  predicted_ratings <- 
    te %>% 
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  rmse <- sqrt(mean((te$rating - predicted_ratings)^2))
  print(rmse)
  return(rmse)
}

lambdas <- seq(20, 50, 5)
rmses <- sapply(lambdas, train_predict_genre, tr = edx_train_genre, te = edx_test_genre)
min(rmses)
lambdas[which.min(rmses)]

# replace summarize(b_g = sum(rating - b_i - mu - b_u)/(n()+l))
# by summarize(b_g = sum(rating - b_i - mu - b_u)/(n()+l*genre_count))
# to reduce the impact of multi genre

train_predict_genre <- function(l, tr, te){
  print(l)
  b_g <- tr %>% 
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - mu - b_u)/(n()+l*max(genre_count)))
  predicted_ratings <- 
    te %>% 
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    .$pred
  rmse <- sqrt(mean((te$rating - predicted_ratings)^2))
  print(rmse)
  return(rmse)
}

lambdas <- seq(32, 64, 16)
rmses <- sapply(lambdas, train_predict_genre, tr = edx_train_genre, te = edx_test_genre)
min(rmses)
lambdas[which.min(rmses)]
ll <- 70

b_g <- edx_train_genre %>% 
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - mu - b_u)/(n()+ll*max(genre_count)))

validation_genre <- validation %>%
  separate_rows(genres, sep = "\\|")

predicted_ratings <- 
  validation_genre %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  .$pred

rmse <- sqrt(mean((validation_genre$rating - predicted_ratings)^2))

print(rmse)


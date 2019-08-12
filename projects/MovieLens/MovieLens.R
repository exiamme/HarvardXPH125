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

# Check the lower hinge of the data to estimate the initial lambda range
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

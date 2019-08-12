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
# if using R 3.5 or earlier, use `set.seed(1)` instead
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


#Q1
dim(edx)

#Q2
sum(edx$rating == 0)
sum(edx$rating == 3)
#correct
edx %>% filter(rating == 0) %>% tally()

#Q3
edx %>% select(movieId) %>% distinct() %>% dim()
#correct
n_distinct(edx$movieId)

#Q4
n_distinct(edx$userId)

#Q5
edx %>% 
  filter(grepl("Drama",genres)) %>%
  summarise(ratings = n())

edx %>% 
  filter(grepl("Comedy",genres)) %>%
  summarise(ratings = n())

edx %>% 
  filter(grepl("Thriller",genres)) %>%
  summarise(ratings = n())

edx %>% 
  filter(grepl("Romance",genres)) %>%
  summarise(ratings = n())
  
#correct but way too slow
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#Q6
edx %>% 
  group_by(title) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head()

#Q7
edx %>% 
  group_by(rating) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#correct
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))  

#Q8 correct
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()


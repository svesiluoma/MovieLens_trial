library(tidyverse)
library(caret)
library(dslabs)

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

### End of the given code ###

### Studying the data
# Dimensios of used data sets
dim(edx)
dim(validation)
# Summarixing the training set
summary(edx)

# Looking what the rows look alike
edx %>% head()

# How many different movies
n_distinct(edx$movieId)
# How many different users
n_distinct(edx$userId)

# Amounts of ratings per movie
ratings <- edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
hist(ratings$count)

# Amounts of ratings per user
ratings_per_user <- edx %>% group_by(userId) %>%
  summarize(count_user = n()) %>%
  arrange(desc(count_user))
hist(ratings_per_user$count_user)

# Amounts of the ratings per rating level
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(rating, count)) +
  geom_line()

# RMSE score used for comparing the algorithms
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2))
}

# Baseline value for RMSE based on the average
mu_hat <- mean(edx$rating)
mu_hat
naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse
# Adding the values to a rmse_results data frame to be able to show these as a summary later
rmse_results <- data.frame(method = "Based on average", RMSE=naive_rmse)
rmse_results

# Differences in ratings per movie
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu_hat))
# Taking into consideration that some movies are rated higher than the others
predicted_ratings <- mu_hat + validation %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
model_1_rmse
# Adding the resulting value to the list
rmse_results <- bind_rows(rmse_results, 
                          data_frame(method="Movie effect model", 
                                     RMSE = model_1_rmse))

# Differences in ratings per user and noticing both movie effect and user effect
user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))
# Taking into consideration both movie and user effect
predicted_ratings <- validation %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
model_2_rmse
# Adding the resulting value to the list
rmse_results <- bind_rows(rmse_results, 
                          data_frame(method="Movie + user effect model", 
                                     RMSE = model_2_rmse))

# Results as a table
rmse_results
